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
import HEP.Automation.MadGraph.Model.ADMXQLD211
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
processSetup :: ProcessSetup ADMXQLD211
processSetup = PS {  
    model = ADMXQLD211
  , process = "\n\
 \generate p p > go go / ul dl sl cl ur dr sr cr QED=0, (go > c~ cl, cl > d e+ sxxp~) , (go > c~ cl, cl > d e+ sxxp~ ) \n\
 \add process p p > go go / ul dl sl cl ur dr sr cr QED=0, (go > c~ cl, cl > d e+ sxxp~) , (go > c cl~, cl~ > d~ e- sxxp ) \n\
 \add process p p > go go / ul dl sl cl ur dr sr cr QED=0, (go > c cl~, cl~ > d~ e- sxxp) , (go > c~ cl, cl > d e+ sxxp~ ) \n\
 \add process p p > go go / ul dl sl cl ur dr sr cr QED=0, (go > c cl~, cl~ > d~ e- sxxp) , (go > c cl~, cl~ > d~ e- sxxp ) \n"

    -- "\ngenerate P P > t1 t1~ QED=0, t1 > d e+ sxxp~ , t1~ > d~ e- sxxp \n"
    -- "\ngenerate P P > t t~ \n" -- 
  , processBrief = "gluinopair_stopdecayfull" 
    -- "ttbar" -- 
  , workname   = "Test22_20130314_ADMXQLD211"
  }

-- | 
psets :: [ModelParam ADMXQLD211]
psets = [ ADMXQLD211Param { mstop = 50000, mgluino = x, msquark = y }
        | (x,y) 
            <- [(300,100)
               ,(400,100),(400,200)
               ,(500,100),(500,200),(500,300)
               ,(600,100),(600,200),(600,300),(600,400)
               ,(700,100),(700,200),(700,300),(700,400),(700,500)
               ,(800,100),(800,200),(800,300),(800,400),(800,500),(800,600)
               ,(900,100),(900,200),(900,300),(900,400),(900,500),(900,600),(900,700)
               ,(1000,100),(1000,200),(1000,300),(1000,400),(1000,500),(1000,600),(1000,700),(1000,800) ] ] 


-- [200,300,400] ] 

-- x <- [100,500,1000,1500] ]

-- [500,1000] ] 
-- [1500] ] 
-- [100] ] 
-- [600,700,800] ]

-- [100,200..1600] ]


-- | 
rsetup = RS { numevent = 10000
            , machine = LHC7 ATLAS
            , rgrun   = Auto -- Fixed
            , rgscale = 200.0
            , match   = NoMatch
            , cut     = NoCut 
            , pythia  = RunPYTHIA
            , lhesanitizer = -- NoLHESanitize
                             LHESanitize (Replace [(9000201,1000022),(-9000201,1000022)]) 
                             -- LHESanitize (Elim [9000201]) 
            , pgs     = RunPGS (Cone 0.4, WithTau)
            , uploadhep = NoUploadHEP
            , setnum  = 1
            }

-- | 
getWSetup :: [IO (WorkSetup ADMXQLD211)]
getWSetup = [ WS <$> getScriptSetup <*> pure processSetup <*> pure p 
                 <*> pure rsetup  
                 <*> pure (WebDAVRemoteDir "") | p <- psets ]

main = do 
  updateGlobalLogger "MadGraphAuto" (setLevel DEBUG)

  mapM_ work =<< sequence getWSetup 

-- | 
-- work p  -- :: IO ()
work wsetup = do -- wsetup <- getWSetup 
            r <- flip runReaderT wsetup . runErrorT $ do 
                 WS ssetup psetup param rsetup _ <- ask 
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
                     runHEP2LHE
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
          