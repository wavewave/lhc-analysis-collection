{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

import           Codec.Compression.GZip
import           Control.Applicative
import           Control.Lens
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Maybe
import qualified Data.ByteString.Lazy.Char8 as LB
import           Data.Default       
import qualified Data.Foldable as F
import           Data.List (foldl')
import           System.Environment
import           System.FilePath
import           System.IO as IO
-- 
import HEP.Automation.EventGeneration.Config
import HEP.Automation.MadGraph.Model
import HEP.Automation.MadGraph.Model.SM
import HEP.Automation.MadGraph.SetupType
import HEP.Automation.MadGraph.Type
import HEP.Automation.MadGraph.Util
import HEP.Parser.LHCOAnalysis.PhysObj
import HEP.Parser.LHCOAnalysis.Parse
import HEP.Storage.WebDAV.CURL
import HEP.Storage.WebDAV.Type
--
import HEP.Physics.Analysis.Common.XSecNTotNum
--
import qualified Paths_madgraph_auto as PMadGraph
import qualified Paths_madgraph_auto_model as PModel


-- |  
getScriptSetup :: IO ScriptSetup
getScriptSetup = do 
  workdir <- getEnv "WORKDIR" 
  -- homedir <- getHomeDirectory

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
  , process = MGProc [] [ "g g > t t~ QED=0" ]
  , processBrief = "ttbarQCD" 
  , workname   = "ttbarQCD"
  , hashSalt = HashSalt Nothing
  }

-- | 
pset :: ModelParam SM
pset = SMParam

-- | 
rsetup :: Double -> Int -> RunSetup
rsetup sqrts n = 
    RS { numevent = 10000
       , machine = Parton (0.5*sqrts) ATLAS -- LHC14 ATLAS
       , rgrun   = Auto 
       , rgscale = 200.0
       , match   = NoMatch
       , cut     = NoCut -- DefCut 
       , pythia  = NoPYTHIA --  RunPYTHIA 
       , lhesanitizer = [] 
       , pgs     = NoPGS -- RunPGS (Cone 0.4, WithTau)
       , uploadhep = NoUploadHEP
       , setnum  = n
       } 

-- | 
getWSetup :: (Double,Int) -> IO (WorkSetup SM)
getWSetup (sqrts,n) = WS <$> getScriptSetup 
                        <*> pure processSetup 
                        <*> pure pset
                        <*> pure (rsetup sqrts n)
                        <*> pure (WebDAVRemoteDir "montecarlo/HeavyHiggs/interfere_test")

genset = [ (sqrts,n) | sqrts <- [350,360..850] {-  ,420..1000] -} ,  n <- [1] ]

checkAndDownload :: WebDAVConfig -> WebDAVRemoteDir -> FilePath -> MaybeT IO ()
checkAndDownload cfg rdir fpath = do
    b <- liftIO $ doesFileExistInDAV cfg rdir fpath
    guard b
    liftIO $ downloadFile False cfg rdir fpath 
    liftIO $ putStrLn $ fpath ++ " is successfully downloaded"

readxsec :: Handle -> Double -> IO ()
readxsec h sqrts = do
  ws1 <- getWSetup (sqrts,1)
  let rname = makeRunName (ws_psetup ws1) (ws_param ws1) (ws_rsetup ws1)
  let pkey = "/afs/cern.ch/work/i/ikim/private/webdav/priv.txt"
      pswd = "/afs/cern.ch/work/i/ikim/private/webdav/cred.txt"
  Just cr <- getCredential pkey pswd
  let whost = "http://top.physics.lsa.umich.edu:10080/webdav/"
      wdavcfg = WebDAVConfig cr whost
      wdavrdir = WebDAVRemoteDir "montecarlo/HeavyHiggs/interfere_test"
  mr <- xsec XSecLHE wdavcfg wdavrdir rname
  F.forM_ mr $ \r -> do
    print (sqrts,r)
    hPutStrLn h (show sqrts ++ " " ++ show r)

main = do
  withFile "myresultqcd.dat" WriteMode $ \h -> do
    mapM_ (readxsec h) [350,360..850]

