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
import HEP.Parser.LHEParser.Type
import HEP.Automation.MadGraph.Model.ADMXQLD111
import HEP.Automation.MadGraph.Machine
import HEP.Automation.MadGraph.Run
import HEP.Automation.MadGraph.SetupType
import HEP.Automation.MadGraph.UserCut
-- 
import HEP.Automation.EventChain.LHEConn
import HEP.Automation.EventChain.FileDriver
import HEP.Automation.EventChain.Type.Skeleton
import HEP.Automation.EventChain.Type.Spec
import HEP.Automation.EventChain.Type.Process
import HEP.Automation.EventChain.SpecDSL
import HEP.Automation.EventChain.Simulator 
import HEP.Automation.EventChain.Process
import HEP.Automation.EventChain.Process.Generator
import HEP.Automation.EventChain.Main 
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

p_gluino :: DDecay 
p_gluino = d ([1000021], [p_sup,t jets]) 

p_sup :: DDecay 
p_sup = d (sup, [t leptons, t jets, t adms])

idx_sqsg_2l3j2x :: CrossID ProcSmplIdx 
idx_sqsg_2l3j2x = mkCrossIDIdx (mkDICross p_sqsg_2l3j2x)

{-
idx_2sq_2l2j2x :: CrossID ProcSmplIdx
idx_2sq_2l2j2x = mkCrossIDIdx (mkDICross p_2sq_2l2j2x)
-}

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

{-
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
mgrunsetup :: Int -> MGRunSetup
mgrunsetup n = 
  MGRS { mgrs_numevent = n
       , mgrs_machine = LHC7 ATLAS
       , mgrs_rgrun   = Auto
       , mgrs_rgscale = 200.0
       , mgrs_match   = NoMatch
       , mgrs_cut     = NoCut 
       , mgrs_pythia  = RunPYTHIA -- NoPYTHIA
       , mgrs_usercut = NoUserCutDef 
       , mgrs_lhesanitizer = -- NoLHESanitize 
                             LHESanitize (Replace [(9000201,1000022),(-9000201,1000022)]) 
       , mgrs_pgs     = RunPGS
       , mgrs_jetalgo = Cone 0.4
       , mgrs_uploadhep = NoUploadHEP
       , mgrs_setnum  = 1
       }


worksets = [ (mgl,msq,50000,50000, 100) {- 10000) -} | mgl <- [200,300..2000], msq <- [100,200..mgl-100] ] 

main :: IO () 
main = do 
  updateGlobalLogger "MadGraphAuto" (setLevel DEBUG)
  mapM_ scanwork worksets 

  -- args <- getArgs 
  -- when (length args /= 5) $ 
  --   fail "admproject_qld mgl msq msl mneut numofevent"
  {- let mgl :: Double = read (args !! 0) 
      msq :: Double = read (args !! 1) 
      msl :: Double = read (args !! 2)
      mneut :: Double = read (args !! 3) 
      n :: Int = read (args !! 4) -}


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
    ("Work20130301_sqsg","sqsg_2l3j2x") 
    param 
    map_sqsg_2l3j2x p_sqsg_2l3j2x 
    mgrs 

  let wsetup = getWorkSetupCombined ADMXQLD111 ssetup param ("Work20130301_sqsg","sqsg_2l3j2x")  mgrs 
  phase2work wsetup 





phase2work :: WorkSetup ADMXQLD111 -> IO ()
phase2work wsetup = do 
    r <- flip runReaderT wsetup . runErrorT $ do 
       WS ssetup psetup rsetup _ <- ask 
       cardPrepare                      
       case (lhesanitizer rsetup,usercut rsetup,pythia rsetup) of
         (NoLHESanitize, NoUserCutDef,_) -> return ()
         (NoLHESanitize, UserCutDef _,_) -> do 
           runHEP2LHE       
           runHEPEVT2STDHEP 
           runPGS           
           runClean         
           updateBanner   
         (LHESanitize pid, NoUserCutDef, RunPYTHIA) -> do 
           sanitizeLHE
           runPYTHIA
           runHEP2LHE
           runPGS           
           runClean         
           updateBanner   
         (LHESanitize pid, NoUserCutDef, NoPYTHIA) -> do 
           sanitizeLHE
           updateBanner   
         (LHESanitize pid, UserCutDef _,RunPYTHIA) -> do 
           sanitizeLHE
           runPYTHIA
           runHEP2LHE       
           runHEPEVT2STDHEP 
           runPGS           
           runClean         
           updateBanner   
         (LHESanitize pid, UserCutDef _,NoPYTHIA) -> do 
           sanitizeLHE
           updateBanner    
       cleanHepFiles  
    print r  
    return ()




{-
p_multijet :: DCross  
p_multijet = x (t proton,t proton, [p_go, p_go]) 

p_go :: DDecay 
p_go = d ([1000021], [ {- t [1000022] -} p_neut, t jets, t jets]) 

p_neut :: DDecay 
p_neut = d ([1000022], [t adms, t jets, t jets, t leptons])

idx_multijet :: CrossID ProcSmplIdx
idx_multijet = mkCrossIDIdx (mkDICross p_multijet )
-}
