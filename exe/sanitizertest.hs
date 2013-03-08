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
       , mgrs_pythia  = NoPYTHIA -- RunPYTHIA 
       , mgrs_usercut = NoUserCutDef 
       , mgrs_lhesanitizer = -- NoLHESanitize 
                             LHESanitize (Replace [(9000201,1000022),(-9000201,1000022)]) 
       , mgrs_pgs     = NoPGS -- RunPGS
       , mgrs_jetalgo = Cone 0.4
       , mgrs_uploadhep = NoUploadHEP
       , mgrs_setnum  = 1
       }


worksets = take 1 $ [ (mgl,msq,50000,50000, 5) {- 10000) -} | mgl <- [200,300..2000], msq <- [100,200..mgl-100] ] 

main :: IO () 
main = do 
  updateGlobalLogger "MadGraphAuto" (setLevel DEBUG)
  mapM_ scanwork worksets 



scanwork :: (Double,Double,Double,Double,Int) -> IO () 
scanwork (mgl,msq,msl,mneut,n) = do
  homedir <- getHomeDirectory 
  ssetup <- getScriptSetup (homedir </> "repo/workspace/montecarlo/working")
                           (homedir </> "repo/ext/MadGraph5_v1_4_8_4/")
                           (homedir </> "repo/workspace/montecarlo/mc/")



  let param = modelparam mgl msq msl mneut
      mgrs = mgrunsetup n

  {-  evchainGen ADMXQLD111
    ssetup 
    ("Work20130307_sqsg","sqsg_2l3j2x") 
    param 
    -- map_2sg_2l4j2x p_2sg_2l4j2x 
    map_sqsg_2l3j2x p_sqsg_2l3j2x 
    mgrs 
  -}
  let wsetup = getWorkSetupCombined ADMXQLD111 ssetup param ("Work20130307_sqsg","sqsg_2l3j2x")  mgrs 
  -- phase2work wsetup 
  r <- flip runReaderT wsetup . runErrorT $ do 
    sanitizeLHE
  return ()



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




