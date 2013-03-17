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
import HEP.Automation.MadGraph.Model.ADMXQLD111
import HEP.Automation.MadGraph.Machine
import HEP.Automation.MadGraph.Run
import HEP.Automation.MadGraph.SetupType
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

adms = [9000201,-9000201,9000202,-9000202]

sup = [1000002,-1000002] 

p_2sq_2l2j2x :: DCross 
p_2sq_2l2j2x = x (t proton, t proton, [p_sup, p_sup])

p_sqsg_2l3j2x :: DCross 
p_sqsg_2l3j2x = x (t proton, t proton, [p_sup,p_gluino]) 

p_2sg_2l4j2x :: DCross
p_2sg_2l4j2x = x (t proton, t proton, [p_gluino,p_gluino])

p_gluino :: DDecay 
p_gluino = d ([1000021], [p_sup,t jets]) 

p_sup :: DDecay 
p_sup = d (sup, [t leptons, t jets, t adms])

idx_2sg_2l4j2x :: CrossID ProcSmplIdx
idx_2sg_2l4j2x = mkCrossIDIdx (mkDICross p_2sg_2l4j2x) 


idx_sqsg_2l3j2x :: CrossID ProcSmplIdx 
idx_sqsg_2l3j2x = mkCrossIDIdx (mkDICross p_sqsg_2l3j2x)


idx_2sq_2l2j2x :: CrossID ProcSmplIdx
idx_2sq_2l2j2x = mkCrossIDIdx (mkDICross p_2sq_2l2j2x)


map_2sg_2l4j2x :: ProcSpecMap
map_2sg_2l4j2x = 
    HM.fromList [(Nothing             , ["p p > go go QED=0"])
                ,(Just (3,1000021,[]) , ["go > ul u~"
                                        ,"go > ul~ u "])
                ,(Just (4,1000021,[]) , ["go > ul u~ "
                                        ,"go > ul~ u "])
                ,(Just (1,1000002,[3]), ["ul > d e+ sxxp~ "])
                ,(Just (1,-1000002,[3]),["ul~ > d~ e- sxxp "])
                ,(Just (1,1000002,[4]), ["ul > d e+ sxxp~ "])
                ,(Just (1,-1000002,[4]),["ul~ > d~ e- sxxp "])
                ] 
{-


map_sqsg_2l3j2x :: ProcSpecMap
map_sqsg_2l3j2x = 
    HM.fromList [(Nothing             , "\n\
                                        \generate p p > ul go QED=0\n\
                                        \add process p p > ul~ go QED=0 \n")
                ,(Just (3,1000002,[]) , "\ngenerate ul > d e+ sxxp~ \n")
                ,(Just (3,-1000002,[]), "\ngenerate ul~ > d~ e- sxxp \n")
                ,(Just (4,1000021,[]) , "\n\
                                        \generate go > ul u~ \n\
                                        \add process go > ul~ u \n" )
                ,(Just (1,1000002,[4]), "\ngenerate ul > d e+ sxxp~ \n")
                ,(Just (1,-1000002,[4]),"\ngenerate ul~ > d~ e- sxxp \n")
                ] 



map_2sq_2l2j2x :: ProcSpecMap
map_2sq_2l2j2x = 
    HM.fromList [(Nothing            ,"\n\
                                      \generate p p > ul ul~ QED=0\n\
                                      \add process p p > ul ul QED=0 \n\
                                      \add process p p > ul~ ul~ QED=0 \n" )
                ,(Just (3,1000002,[]), "\ngenerate ul > d e+ sxxp~ \n")
                ,(Just (3,-1000002,[]), "\ngenerate ul~ > d~ e- sxxp \n")
                ,(Just (4,1000002,[]), "\ngenerate ul > d e+ sxxp~ \n")
                ,(Just (4,-1000002,[]), "\ngenerate ul~ > d~ e- sxxp \n")
                ] 

-}

modelparam mgl msq msl mneut = ADMXQLD111Param mgl msq msl mneut 

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


worksets = take 1 [ (mgl,msq,50000,50000, 100) | mgl <- [2000], msq <- [1500] ] 

--  | mgl <- [200,300..2000], msq <- [100,200..mgl-100] ] 

main :: IO () 
main = do 
  updateGlobalLogger "MadGraphAuto" (setLevel DEBUG)
  mapM_ scanwork worksets 


 
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




scanwork :: (Double,Double,Double,Double,Int) -> IO () 
scanwork (mgl,msq,msl,mneut,n) = do
  homedir <- getHomeDirectory 
  ssetup <- getScriptSetup (homedir </> "repo/workspace/montecarlo/working")
                           (homedir </> "repo/ext/MadGraph5_v1_4_8_4/")
                           (homedir </> "repo/workspace/montecarlo/mc/")



  let param = modelparam mgl msq msl mneut
      mgrs = mgrunsetup n

  evchainGen ADMXQLD111
    ssetup 
    ("Work20130317_4_2sg","2sg_2l4j2x") 
    param 
    map_2sg_2l4j2x p_2sg_2l4j2x 
    -- map_sqsg_2l3j2x p_sqsg_2l3j2x 
    mgrs 

  let wsetup = getWorkSetupCombined ADMXQLD111 ssetup param ("Work20130317_4_2sg","2sg_2l4j2x")  mgrs 
  phase2work wsetup 





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




