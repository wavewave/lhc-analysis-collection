module Main where

import Control.Applicative
import Control.Monad
import Control.Monad.Reader 
import Control.Monad.Error
import System.Directory
import System.Environment
import System.FilePath 
import System.Log.Logger
import System.Process
-- 
import HEP.Automation.EventGeneration.Config
import qualified HEP.Automation.EventGeneration.Work as EV
import HEP.Automation.MadGraph.Model
import HEP.Automation.MadGraph.Model.SM
import HEP.Automation.MadGraph.Run
import HEP.Automation.MadGraph.SetupType
import HEP.Automation.MadGraph.Type
import HEP.Parser.LHE.Sanitizer.Type
import HEP.Storage.WebDAV
-- 
import qualified Paths_madgraph_auto as PMadGraph
import qualified Paths_madgraph_auto_model as PModel

-- |  
getScriptSetup :: IO ScriptSetup
getScriptSetup = do 
  workdir <- getEnv "WORKDIR" 

  mdldir <- (</> "template") <$> PModel.getDataDir
  rundir <- (</> "template") <$> PMadGraph.getDataDir 
  return $ 
    SS { modeltmpldir = mdldir 
       , runtmpldir = rundir 
       , sandboxdir = workdir </> "montecarlo/sandbox"
       , mg5base    = workdir </> "montecarlo/MG5_aMC_v2_1_2"
       , mcrundir   = workdir </> "montecarlo/mcrun"
       , pythia8dir = ""
       , pythia8toHEPEVT = "" 
       , hepevt2stdhep = "" 
       , pythiapgsdir = "/nix/store/8c0ja4dh6rlii1xnbq48c9bcii57wly4-pythia-pgs-2.4.0/share/pythia-pgs"
       }

-- | 
processSetup :: ProcessSetup SM
processSetup = PS {  
    model = SM
  , process = MGProc [] [ "e+ e- > j j QED=2" ]
  , processBrief = "eejj" 
  , workname   = "eejj"
  , hashSalt = HashSalt Nothing
  }

-- | 
pset :: ModelParam SM
pset = SMParam

-- | 
rsetup :: Double -> Int -> RunSetup
rsetup sqrts n = 
    RS { numevent = 100000
       , machine = Parton (0.5*sqrts) ATLAS -- Tevatron -- ATLAS -- LHC14 ATLAS
       , rgrun   = Fixed 
       , rgscale = 200.0
       , match   = NoMatch
       , cut     = NoCut -- DefCut 
       , pythia  = RunPYTHIA 
       , lhesanitizer = [] 
       , pgs     = RunPGS (AntiKTJet 0.4, WithTau)
       , uploadhep = NoUploadHEP
       , setnum  = n
       } 

-- | 
getWSetup :: (Double,Int) -> IO (WorkSetup SM)
getWSetup (sqrts,n) = WS <$> getScriptSetup 
                        <*> pure processSetup 
                        <*> pure pset
                        <*> pure (rsetup sqrts n)
                        <*> pure (WebDAVRemoteDir "montecarlo/HeavyHiggs/standardcandle")

genset = [ (sqrts,n) | sqrts <- {- [100,200..1500] -} [1600,1800..3000],  n <- [1] ]

preparedir = do
  workdir <- getEnv "WORKDIR" 
  let mcrootdir = workdir </> "montecarlo"
      sandboxdir = workdir </> "montecarlo/sandbox"
      mcrundir   = workdir </> "montecarlo/mcrun"
  mapM_ (\x -> doesDirectoryExist x >>= \b -> when (not b) (createDirectory x)) [workdir,mcrootdir,sandboxdir, mcrundir]
  setCurrentDirectory mcrootdir
  let testsh = mcrootdir </> "test.sh"
  
  system ("echo 'unpack' > " ++ testsh ++ " && chmod u+x " ++ testsh ++ " && sleep 1; load-env-MadGraph5_aMCatNLO-2.1.2 " ++ testsh)
  return ()


main :: IO ()
main = do 
  preparedir
  let pkey = "/home/wavewave/temp/madgraph/priv.txt"
      pswd = "/home/wavewave/temp/madgraph/cred.txt"
  Just cr <- getCredential pkey pswd
  let whost = "http://top.physics.lsa.umich.edu:10080/webdav/"
  let wdavcfg = WebDAVConfig cr whost
  updateGlobalLogger "MadGraphAuto" (setLevel DEBUG)
  mapM_ (work wdavcfg <=< getWSetup) genset

-- | 
work :: WebDAVConfig -> (WorkSetup SM) -> IO ()
work wdavcfg wsetup = do 
  r <- flip runReaderT wsetup . runErrorT $ do 
       WS ssetup psetup _ rs _ <- ask                  
       let wb = mcrundir ssetup 
           wn = workname psetup  
       b <- liftIO $ (doesDirectoryExist (wb </> wn))
       when (not b) $ createWorkDir ssetup psetup
       cardPrepare           
       generateEvents   
       case (lhesanitizer rs, pythia rs) of
         ([], _) -> do
           renamePythiaPGSResult
           makeHepGz
         (_:_, RunPYTHIA8) -> return ()
         (_:_, RunPYTHIA) -> do 
           sanitizeLHE
           runPYTHIA
           runPGS           
           runClean         
         (_:_, NoPYTHIA) -> do 
           sanitizeLHE
       cleanHepFiles  
  case r of
    Left err -> print err
    Right _ -> do EV.uploadEventFull NoUploadHEP wdavcfg wsetup 
                  EV.uploadJSON wdavcfg wsetup 
  return () 
