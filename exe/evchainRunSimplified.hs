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
import HEP.Automation.MadGraph.Model
import HEP.Automation.MadGraph.Model.SimplifiedSUSY
import HEP.Automation.MadGraph.Run
import HEP.Automation.MadGraph.SetupType
import HEP.Automation.MadGraph.Type
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
-- 
import qualified Paths_madgraph_auto as PMadGraph 
import qualified Paths_madgraph_auto_model as PModel 


jets = [1,2,3,4,-1,-2,-3,-4,21]

leptons = [11,13,-11,-13] 

neut = 1000022

adms = [9000201,-9000201,9000202,-9000202]

squarks = [  1000001, -1000001  -- sdown_L
          ,  1000002, -1000002  -- sup_L
          ,  1000003, -1000003  -- sstrange_L
          ,  1000004, -1000004  -- scharm_L 
          ,  2000001, -2000001  -- sdown_R 
          ,  2000002, -2000002  -- sup_R
          ,  2000003, -2000003  -- sstrange_R
          ,  2000004, -2000004  -- scharm_R 
          ] 
 


p_2sq_2j2x :: DCross 
p_2sq_2j2x = x (t proton, t proton, [p_squark, p_squark])

p_squark :: DDecay 
p_squark = d ( squarks, [t jets, neut ] )

idx_2sq_2j2x :: CrossID ProcSmplIdx
idx_2sq_2j2x = mkCrossIDIdx (mkDICross p_2sq_2j2x)


map_2sq_2j2x :: ProcSpecMap
map_2sq_2j2x = 
    HM.fromList [ (Nothing             , MGProc ["define  sql = ul ul~ dl dl~ sl sl~ cl cl~"] 
                                                ["p p > sql sql  QED=0"])
                , (Just (3, 1000001,[]), MGProc [] ["dl  > d n1"])
                , (Just (3,-1000001,[]), MGProc [] ["dl~ > d~ n1"])
                , (Just (3, 1000002,[]), MGProc [] ["ul  > u n1"])
                , (Just (3,-1000002,[]), MGProc [] ["ul~ > u~ n1"])
                , (Just (3, 1000003,[]), MGProc [] ["sl  > s n1"])
                , (Just (3,-1000003,[]), MGProc [] ["sl~ > s~ n1"])
                , (Just (3, 1000004,[]), MGProc [] ["cl  > c n1"])
                , (Just (3,-1000004,[]), MGProc [] ["cl~ > c~ n1"]) 
                , (Just (4, 1000001,[]), MGProc [] ["dl  > d n1"])
                , (Just (4,-1000001,[]), MGProc [] ["dl~ > d~ n1"])
                , (Just (4, 1000002,[]), MGProc [] ["ul  > u n1"])
                , (Just (4,-1000002,[]), MGProc [] ["ul~ > u~ n1"])
                , (Just (4, 1000003,[]), MGProc [] ["sl  > s n1"])
                , (Just (4,-1000003,[]), MGProc [] ["sl~ > s~ n1"])
                , (Just (4, 1000004,[]), MGProc [] ["cl  > c n1"])
                , (Just (4,-1000004,[]), MGProc [] ["cl~ > c~ n1"]) 
                 
                ] 



modelparam :: Double -> Double -> Double -> ModelParam SimplifiedSUSY
modelparam mneut mgl msq = SimplifiedSUSYParam mneut mgl msq 

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


worksets = [ (mn,50000,mq,10000) | mn <- [100,200..1200], mq <- [mn,mn+100..1200] ] 

--  | mgl <- [200,300..2000], msq <- [100,200..mgl-100] ] 

main :: IO () 
main = do 
  updateGlobalLogger "MadGraphAuto" (setLevel DEBUG)
  mapM_ scanwork worksets 


-- (100,50000,2000,10000)


 
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




scanwork :: (Double,Double,Double,Int) -> IO () 
scanwork (mneut,mgl,msq,n) = do
  homedir <- getHomeDirectory 
  ssetup <- getScriptSetup "/tmp/pipeline/flux-login1/sandbox"
                           "/tmp/pipeline/flux-login1/MadGraph5_v1_5_8/"
                           "/tmp/pipeline/flux-login1/mc"

                           -- (homedir </> "repo/workspace/montecarlo/working")
                           -- (homedir </> "repo/ext/MadGraph5_v1_4_8_4/")
                           -- (homedir </> "repo/workspace/montecarlo/mc/")
  let param = modelparam mneut mgl msq 
      mgrs = mgrunsetup n
  evchainGen SimplifiedSUSY
    ssetup 
    ("2sq_2j2x","2sq_2j2x") 
    param 
    map_2sq_2j2x p_2sq_2j2x 
    mgrs 
  let wsetup = getWorkSetupCombined SimplifiedSUSY ssetup param ("2sq_2j2x","2sq_2j2x")  mgrs 
  phase2work wsetup 





phase2work :: WorkSetup SimplifiedSUSY -> IO ()
phase2work wsetup = do 
    r <- flip runReaderT wsetup . runErrorT $ do 
       ws <- ask 
       let (ssetup,psetup,param,rsetup) = 
             ((,,,) <$> ws_ssetup <*> ws_psetup <*> ws_param <*> ws_rsetup) ws 
       cardPrepare                      
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




