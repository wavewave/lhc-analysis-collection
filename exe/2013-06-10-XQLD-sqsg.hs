{-# LANGUAGE ScopedTypeVariables, NoMonomorphismRestriction, RecordWildCards #-}

module Main where

import           Control.Applicative
import           Control.Monad 
import           Control.Monad.Error
import           Control.Monad.Reader 
import           Control.Monad.State 
import           Data.Conduit
import qualified Data.Conduit.List as CL
import qualified Data.Traversable as T
import qualified Data.HashMap.Lazy as HM
import           Data.Maybe 
import           System.Directory
import           System.Environment
import           System.FilePath ((</>))
import           System.IO
import           System.Log.Logger
-- 
import HEP.Automation.EventChain.Driver 
import HEP.Automation.EventChain.File
import HEP.Automation.EventChain.LHEConn
import HEP.Automation.EventChain.Type.Skeleton
import HEP.Automation.EventChain.Type.Spec
import HEP.Automation.EventChain.Type.Process
import HEP.Automation.EventChain.SpecDSL
import HEP.Automation.EventChain.Simulator 
import HEP.Automation.EventChain.Process
import HEP.Automation.EventChain.Process.Generator
import HEP.Automation.EventGeneration.Config
import HEP.Automation.EventGeneration.Type
import HEP.Automation.EventGeneration.Work 
import HEP.Automation.MadGraph.Model.ADMXQLD111
import HEP.Automation.MadGraph.Run
import HEP.Automation.MadGraph.SetupType
import HEP.Automation.MadGraph.Type
import HEP.Parser.LHE.Type
import HEP.Parser.LHE.Sanitizer.Type
import HEP.Storage.WebDAV
-- 
import qualified Paths_madgraph_auto as PMadGraph 
import qualified Paths_madgraph_auto_model as PModel 


jets = [1,2,3,4,-1,-2,-3,-4,21]

leptons = [11,13,-11,-13] 

lepplusneut = [11,12,13,14,-11,-12,-13,-14]

adms = [9000201,-9000201,9000202,-9000202]

sup = [1000002,-1000002] 

sdownR = [2000001,-2000001]

p_gluino = d ([1000021], [t lepplusneut, t jets, t jets, t adms])


p_sdownR :: DDecay
p_sdownR = d (sdownR, [t lepplusneut, t jets, t adms])


p_sqsg_2l3j2x :: DCross 
p_sqsg_2l3j2x = x (t proton, t proton, [p_gluino, p_sdownR])


idx_sqsg_2l3j2x :: CrossID ProcSmplIdx
idx_sqsg_2l3j2x = mkCrossIDIdx (mkDICross p_sqsg_2l3j2x)

map_sqsg_2l3j2x :: ProcSpecMap
map_sqsg_2l3j2x = 
    HM.fromList [(Nothing             , MGProc [] [ "p p > go dr QED=0" 
                                                  , "p p > go dr~ QED=0" ])
                ,(Just (3,1000021,[]), MGProc [ "define lep = e+ e- mu+ mu- ve ve~ vm vm~ " 
                                              , "define sxx = sxxp sxxp~ "]
                                              [ "go > lep j j sxx " ] ) 
                ,(Just (4,-2000001,[]), MGProc [] [ "dr~ > u~ e+ sxxp~ "
                                                  , "dr~ > d~ ve~ sxxp~ " ])
                ,(Just (4,2000001,[]) , MGProc [] [ "dr > u e- sxxp "
                                                  , "dr > d ve sxxp " ])
                ] 



modelparam mgl msq msl mneut = ADMXQLD111Param mgl msq msl mneut 

-- | 
mgrunsetup :: Int -> RunSetup
mgrunsetup n = 
  RS { numevent = n
     , machine = LHC8 ATLAS
     , rgrun   = Auto
     , rgscale = 200.0
     , match   = NoMatch
     , cut     = NoCut 
     , pythia  = RunPYTHIA 
     , lhesanitizer = LHESanitize (Replace [(9000201,1000022),(-9000201,1000022)]) 
     , pgs     = RunPGS (AntiKTJet 0.4,NoTau)
     , uploadhep = NoUploadHEP
     , setnum  = 1
     }


worksets = [ (mgl,msq,50000,50000, 10000) | mgl <- [100,200..2000], msq <- [100,200..2000] ] 

main :: IO () 
main = do 
  args <- getArgs 
  let fp = args !! 0 
      n1 = read (args !! 1) :: Int
      n2 = read (args !! 2) :: Int
  --  fp <- (!! 0) <$> getArgs 
  updateGlobalLogger "MadGraphAuto" (setLevel DEBUG)
  -- print (length worksets) 
  mapM_ (scanwork fp) (drop (n1-1) . take n2 $ worksets )

{- 
-- |  
getScriptSetup :: FilePath  -- ^ sandbox directory 
               -> FilePath  -- ^ mg5base 
               -> FilePath  -- ^ main montecarlo run 
               -> IO ScriptSetup
getScriptSetup dir_sb dir_mg5 dir_mc = do 
  dir_mdl <- (</> "template") <$> PModel.getDataDir
  dir_tmpl <- (</> "template") <$> PMadGraph.getDataDir 
  return $ 
    SS { modeltmpldir = dir_mdl
       , runtmpldir = dir_tmpl 
       , sandboxdir = dir_sb 
       , mg5base    = dir_mg5
       , mcrundir   = dir_mc 
       }
-}



scanwork :: FilePath -> (Double,Double,Double,Double,Int) -> IO () 
scanwork fp (mgl,msq,msl,mneut,n) = do
  homedir <- getHomeDirectory 

  getConfig fp >>= 
    maybe (return ()) (\ec -> do 
      let ssetup = evgen_scriptsetup ec
          whost = evgen_webdavroot ec
          pkey = evgen_privatekeyfile ec
          pswd = evgen_passwordstore ec 
      Just cr <- getCredential pkey pswd 
      let wdavcfg = WebDAVConfig { webdav_credential = cr 
                                 , webdav_baseurl = whost } 
          param = modelparam mgl msq msl mneut
          mgrs = mgrunsetup n

      evchainGen ADMXQLD111
        ssetup 
        ("Work20130610_sqsg","sqsg_2l3j2x") 
        param 
        map_sqsg_2l3j2x p_sqsg_2l3j2x 
        mgrs 

      let wsetup' = getWorkSetupCombined ADMXQLD111 ssetup param ("Work20130610_sqsg","sqsg_2l3j2x")  mgrs 
          wsetup = wsetup' { ws_storage = WebDAVRemoteDir "montecarlo/admproject/XQLD/8TeV/scan_sqsg_2l3j2x"} 

      putStrLn "phase2work start"              
      phase2work wsetup
      putStrLn "phase3work start"
      phase3work wdavcfg wsetup 
    )


phase2work :: WorkSetup ADMXQLD111 -> IO ()
phase2work wsetup = do 
    r <- flip runReaderT wsetup . runErrorT $ do 
       ws <- ask 
       let (ssetup,psetup,param,rsetup) = 
             ((,,,) <$> ws_ssetup <*> ws_psetup <*> ws_param <*> ws_rsetup) ws 
       cardPrepare                      
       case (lhesanitizer rsetup,pythia rsetup) of
         (NoLHESanitize,_) -> return ()
         (LHESanitize pid, RunPYTHIA) -> do 
           sanitizeLHE
           runPYTHIA
           runPGS           
           runClean         
         (LHESanitize pid, NoPYTHIA) -> do 
           sanitizeLHE
       cleanHepFiles  
    print r  
    return ()

-- | 
phase3work :: WebDAVConfig -> WorkSetup ADMXQLD111 -> IO () 
phase3work wdav wsetup = do 
  uploadEventFull NoUploadHEP wdav wsetup 
  return () 


