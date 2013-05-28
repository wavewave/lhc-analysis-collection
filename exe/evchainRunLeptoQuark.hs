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
import HEP.Parser.LHE.Type
import HEP.Automation.MadGraph.Model.LeptoQuark1
import HEP.Automation.MadGraph.Run
import HEP.Automation.MadGraph.SetupType
import HEP.Automation.MadGraph.Type
import HEP.Parser.LHE.Sanitizer.Type
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
import HEP.Storage.WebDAV
-- 
import qualified Paths_madgraph_auto as PMadGraph 
import qualified Paths_madgraph_auto_model as PModel 


jets = [1,2,3,4,-1,-2,-3,-4,21]

leptons = [11,13,-11,-13] 

lepplusneut = [11,12,13,14,-11,-12,-13,-14]




p_lq :: DDecay
p_lq = d ([9000006], [t jets, t lepplusneut])

p_antilq :: DDecay 
p_antilq = d ([-9000006], [t jets, t lepplusneut])

p_2lq_2l2j :: DCross 
p_2lq_2l2j = x (t proton, t proton, [p_lq, p_antilq])

idx_2lq_2l2j :: CrossID ProcSmplIdx
idx_2lq_2l2j = mkCrossIDIdx (mkDICross p_2lq_2l2j)

map_2lq_2l2j :: ProcSpecMap
map_2lq_2l2j = 
    HM.fromList [(Nothing             , MGProc [] [ "p p > lq lq~ QED=0" ])
                ,(Just (3,9000006,[]) , MGProc [] [ "lq > u e- " 
                                                  , "lq > d ve " ])
                ,(Just (4,-9000006,[]), MGProc [] [ "lq~ > u~ e+ " 
                                                  , "lq~ > d~ ve~ " ])
                ] 



modelparam mlq = LeptoQuark1Param mlq (pi/4) -- 0 -- (pi/4) 

-- | 
mgrunsetup :: Int -> RunSetup
mgrunsetup n = 
  RS { numevent = n
     , machine = LHC7 ATLAS
     , rgrun   = Auto
     , rgscale = 200.0
     , match   = NoMatch
     , cut     = NoCut 
     , pythia  = RunPYTHIA 
     , lhesanitizer = -- NoLHESanitize 
                      LHESanitize (Replace [(9000201,1000022),(-9000201,1000022)]) 
     , pgs     = RunPGS (AntiKTJet 0.4,NoTau)
     , uploadhep = NoUploadHEP
     , setnum  = 1
     }



-- worksets = [ (mlq,10000) | mlq <- [100,200..2000] ]

worksets = [ (mlq,10000) | mlq <- [600] ]

main :: IO () 
main = do 
  fp <- (!! 0) <$> getArgs 
  updateGlobalLogger "MadGraphAuto" (setLevel DEBUG)
  mapM_ (scanwork fp) worksets -- [(500,10000)] 


 
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




scanwork :: FilePath -> (Double,Int) -> IO () 
scanwork fp (mlq,n) = do
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
          param = modelparam mlq
          mgrs = mgrunsetup n

      evchainGen LeptoQuark1
        ssetup 
        ("test_2lq","2lq_2l2j") 
        param 
        map_2lq_2l2j p_2lq_2l2j 
        mgrs 

      let wsetup' = getWorkSetupCombined LeptoQuark1 ssetup param ("test_2lq","2lq_2l2j")  mgrs 
          wsetup = wsetup' { ws_storage = WebDAVRemoteDir "montecarlo/admproject/LeptoQuark/scan_2l2j" } 

      putStrLn "phase2work start"              
      phase2work wsetup
      putStrLn "phase3work start"
      phase3work wdavcfg wsetup 
    )


phase2work :: WorkSetup LeptoQuark1 -> IO ()
phase2work wsetup = do 
    r <- flip runReaderT wsetup . runErrorT $ do 
       ws <- ask 
       let (ssetup,psetup,param,rsetup) = 
             ((,,,) <$> ws_ssetup <*> ws_psetup <*> ws_param <*> ws_rsetup) ws 
       cardPrepare                      
       case (lhesanitizer rsetup,pythia rsetup) of
         -- (NoLHESanitize,NoPYTHIA) -> return ()
         -- (NoLHESanitize,RunPYTHIA) -> do 
         --   runPYTHIA
         --   runPGS           
         --   runClean         
         (LHESanitize pid, RunPYTHIA) -> do 
           sanitizeLHE
           runPYTHIA
           -- runHEP2LHE
           runPGS           
           runClean         
           -- updateBanner   
         (LHESanitize pid, NoPYTHIA) -> do 
           sanitizeLHE
           -- updateBanner   
       cleanHepFiles  
    print r  
    return ()

-- | 
phase3work :: WebDAVConfig -> WorkSetup LeptoQuark1 -> IO () 
phase3work wdav wsetup = do 
  uploadEventFull NoUploadHEP wdav wsetup 
  return () 

