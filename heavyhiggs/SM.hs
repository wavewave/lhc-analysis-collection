{-# LANGUAGE MultiWayIf #-}

module SM where

import Control.Applicative
import System.Directory
import System.FilePath
-- 
import HEP.Automation.MadGraph.Model
import HEP.Automation.MadGraph.Model.SM
import HEP.Automation.MadGraph.SetupType
import HEP.Automation.MadGraph.Type
import HEP.Automation.MadGraph.Util
import HEP.Storage.WebDAV
--
import qualified Paths_madgraph_auto as PMadGraph
import qualified Paths_madgraph_auto_model as PModel

-- |  
getScriptSetup :: IO ScriptSetup
getScriptSetup = do 
  homedir <- getHomeDirectory
  mdldir <- (</> "template") <$> PModel.getDataDir
  rundir <- (</> "template") <$> PMadGraph.getDataDir 
  return $ 
    SS { modeltmpldir = mdldir 
       , runtmpldir = rundir 
       , sandboxdir = homedir </> "temp/montecarlo/sandbox"
       , mg5base    = homedir </> "temp/montecarlo/MG5_aMC_v2_1_2"
       , mcrundir   = homedir </> "temp/montecarlo/mcrun"
       , pythia8dir = ""
       , pythia8toHEPEVT = "" 
       , hepevt2stdhep = "" 
       , pythiapgsdir = "/nix/store/8c0ja4dh6rlii1xnbq48c9bcii57wly4-pythia-pgs-2.4.0/share/pythia-pgs"
       }



psetup :: ProcessSetup SM
psetup = PS { model = SM
            , process = MGProc [] [ "p p > t t~     QCD=99 QED=2 @0" 
                                  , "p p > t t~ j   QCD=99 QED=2 @1" 
                                  , "p p > t t~ j j QCD=99 QED=2 @2" ] 
            , processBrief = "tt012j"
            , workname = "tt012j"
            , hashSalt = HashSalt Nothing }

rsetupgen  :: Int -> RunSetup 
rsetupgen x = 
    RS { numevent = 10000 
       , machine  = LHC14 ATLAS
       , rgrun    = Auto 
       , rgscale  = 91.0
       , match    = MLM
       , cut      = DefCut
       , pythia   = RunPYTHIA
       , lhesanitizer  = []
       , pgs      = RunPGS (AntiKTJet 0.4, WithTau)
       , uploadhep = NoUploadHEP
       , setnum   = x 
       }


-- | 
getWSetup :: Int -> IO (WorkSetup SM)
getWSetup n = WS <$> getScriptSetup 
                 <*> pure psetup
                 <*> pure SMParam
                 <*> pure (rsetupgen n)
                 <*> (pure . WebDAVRemoteDir . ("montecarlo/SMBKG/LHC14/tt012j_set"++) $
                       if | n <= 1000              -> "1"
                          | n >= 1001 && n <= 2000 -> "2"
                          | n >= 2001 && n <= 3000 -> "3"
                          | n >= 3001 && n <= 4000 -> "4"
                          | n >= 4001 && n <= 5000 -> "5"
                          | n >= 5001 && n <= 6000 -> "6"
                          | n >= 6001 && n <= 7000 -> "7"
                          | n >= 7001 && n <= 8000 -> "8"
                          | n >= 8001 && n <= 9000 -> "9"
                          | n >= 9001 && n <= 10000 -> "10" )

