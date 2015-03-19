module HeavyHiggs4T where

import Control.Applicative
import System.Directory
import System.FilePath
-- 
import HEP.Automation.MadGraph.Model
import HEP.Automation.MadGraph.Model.HeavyHiggs
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

-- | 
processSetup :: ProcessSetup HeavyHiggs
processSetup = PS {  
    model = HeavyHiggs
  , process = MGProc [] [ "p p > t t~ h2, (h2 > t t~)"
                        , "p p > t t~ h3, (h3 > t t~)"
                        ]
  , processBrief = "fourtop" 
  , workname   = "fourtop"
  , hashSalt = HashSalt Nothing
  }

-- | 
pset :: Double -> ModelParam HeavyHiggs
pset mass = HeavyHiggsParam mass

-- | 
rsetup :: Int -> RunSetup
rsetup n = RS { numevent = 10000
              , machine = LHC14 ATLAS
              , rgrun   = Auto -- Fixed
              , rgscale = 200.0
              , match   = NoMatch
              , cut     = DefCut 
              , pythia  = RunPYTHIA 
              , lhesanitizer = [] -- [Replace [(9000201,1000022),(-9000201,1000022)]]
              , pgs     = RunPGS (Cone 0.4, WithTau)
              , uploadhep = NoUploadHEP
              , setnum  = n
              } 

-- | 
getWSetup :: (Double,Int) -> IO (WorkSetup HeavyHiggs)
getWSetup (mass,n) = WS <$> getScriptSetup 
                        <*> pure processSetup 
                        <*> pure (pset mass) 
                        <*> pure (rsetup n)
                        <*> pure (WebDAVRemoteDir "montecarlo/HeavyHiggs/fourtopsimple")

genset = [ (mass,n) | mass <- [800,850,900,950,1000],
                      n <- [2..10] ]
