{-# LANGUAGE PackageImports #-}

module Main where

import Control.Applicative
import Control.Monad
import "mtl" Control.Monad.Reader 
import "mtl" Control.Monad.Error
import System.FilePath 
import System.Directory 
import System.Log.Logger
-- 
import HEP.Automation.MadGraph.Model
import HEP.Automation.MadGraph.Model.SM
import HEP.Automation.MadGraph.Machine
import HEP.Automation.MadGraph.SetupType
import HEP.Automation.MadGraph.Run
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
       , sandboxdir = homedir </> "repo/workspace/montecarlo/working"
       , mg5base    = homedir </> "repo/ext/MadGraph5_v1_4_8_4/"
       , mcrundir   = homedir </> "repo/workspace/montecarlo/mc/"
       }

-- | 
processSetup :: ProcessSetup SM
processSetup = PS {  
    model = SM
  , process = [ "p p > a QCD=99 QED=2 @0 " 
              , "p p > a j QCD=99 QED=2 @1 " ]
  , processBrief = "ajet"

  , workname   = "ajet"
  }

-- | 
pset :: ModelParam SM
pset = SMParam 

-- | 
rsetup = RS { numevent = 10000
            , machine = LHC7 ATLAS
            , rgrun   = Auto -- Fixed
            , rgscale = 200.0
            , match   = MLM -- NoMatch
            , cut     = DefCut 
            , pythia  = RunPYTHIA
            , lhesanitizer = NoLHESanitize
            , pgs     = NoPGS -- RunPGS (Cone 0.4, WithTau)
            , uploadhep = NoUploadHEP
            , setnum  = 1
            }

-- | 
getWSetup :: IO (WorkSetup SM)
getWSetup = WS <$> getScriptSetup 
               <*> pure processSetup 
               <*> pure pset 
               <*> pure rsetup  
               <*> pure (WebDAVRemoteDir "") 

main = do 
  updateGlobalLogger "MadGraphAuto" (setLevel DEBUG)
  wsetup <- getWSetup 
  print wsetup
  work wsetup 

-- | 
-- work p  -- :: IO ()
work wsetup = do -- wsetup <- getWSetup 
            r <- flip runReaderT wsetup . runErrorT $ do 
                 WS ssetup psetup param rsetup _ <- ask 
                 liftIO $ print ssetup 
                 let wb = mcrundir ssetup 
                     wn = workname psetup  
                 b <- liftIO $ (doesDirectoryExist (wb </> wn))
                 when (not b) $ createWorkDir ssetup psetup
                 cardPrepare                      
                 generateEvents   
                 case (lhesanitizer rsetup,pythia rsetup) of
                   (NoLHESanitize, _) -> return ()
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




          {-
          -- create working directory (only once for each process)
          mapM_ (createWorkDir my_ssetup) psetuplist
          sleep 2
          mapM_ (runReaderT cmdSequence) totaltasklist 
          -}
          