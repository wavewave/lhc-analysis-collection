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
import HEP.Automation.MadGraph.Model.HEFTNLO
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
processSetup :: ProcessSetup HEFTNLO
processSetup = PS {  
    model = HEFTNLO
  , process = MGProc [] [ "g g > h1 > t t~ QED=2 HIG=1 HIW=1" ]
  , processBrief = "ttbarheft_onlypseudo" 
  , workname   = "ttbarheft_onlypseudo"
  , hashSalt = HashSalt Nothing
  }

-- |
pset :: (Double,Double,Double,Double) -> ModelParam HEFTNLO
pset (mh,wh,ma,wa) = HEFTNLOParam mh wh ma wa

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
getWSetup :: (Double,Double) -> (Double,Int) -> IO (WorkSetup HEFTNLO)
getWSetup (m,gamma) (sqrts,n) = WS <$> getScriptSetup 
                        <*> pure processSetup 
                        <*> pure (pset (2000,10,m,gamma))
                        <*> pure (rsetup sqrts n)
                        <*> pure (WebDAVRemoteDir "montecarlo/HeavyHiggs/interfere_test")

genset = [ (sqrts,n) | sqrts <- [370,374..850],  n <- [1] ]

rdir :: WebDAVRemoteDir
rdir = WebDAVRemoteDir "newtest2"

checkAndDownload :: WebDAVConfig -> WebDAVRemoteDir -> FilePath -> MaybeT IO ()
checkAndDownload cfg rdir fpath = do
    b <- liftIO $ doesFileExistInDAV cfg rdir fpath
    guard b
    liftIO $ downloadFile False cfg rdir fpath 
    liftIO $ putStrLn $ fpath ++ " is successfully downloaded"

readxsec :: Handle -> (Double,Double) -> Double -> IO ()
readxsec h (m,gamma) sqrts = do
  ws1 <- getWSetup (m,gamma) (sqrts,1)
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
  let (m,gamma) = (400,11.82) -- (400,2.97)-- (500,11.1) -- (400,3.3) -- (500,11.1)
      filename = "xsecptn_only_MA" ++ show m ++ "GA" ++ show gamma ++ ".dat"
  withFile filename WriteMode $ \h -> do
    mapM_ (readxsec h (m,gamma)) [350,352..550] -- [350,354..600]

