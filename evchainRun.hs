{-# LANGUAGE ScopedTypeVariables, NoMonomorphismRestriction, RecordWildCards #-}

module Main where

import           Control.Applicative
import           Control.Monad 
import           Control.Monad.Error 
import           Control.Monad.State 
import           Data.Conduit
import qualified Data.Conduit.List as CL
import qualified Data.Traversable as T
import qualified Data.HashMap.Lazy as HM
import           Data.Maybe 
import           System.Environment
import           System.IO
import           System.Log.Logger
-- 
import HEP.Parser.LHEParser.Type
import HEP.Automation.MadGraph.Model.ADMXQLD111
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


jets = [1,2,3,4,-1,-2,-3,-4,21]

leptons = [11,13,-11,-13] 

adms = [9000201,-9000201,9000202,-9000202]

sup = [1000002,-1000002] 

p_2sq_2l2j2x :: DCross 
p_2sq_2l2j2x = x (t proton, t proton, [p_sup, p_sup])

p_sup :: DDecay 
p_sup = d (sup, [t leptons, t jets, t adms])

idx_2sq_2l2j2x :: CrossID ProcSmplIdx
idx_2sq_2l2j2x = mkCrossIDIdx (mkDICross p_2sq_2l2j2x)


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



modelparam mgl msq msl mneut = ADMXQLD111Param mgl msq msl mneut 

main :: IO () 
main = do 
  updateGlobalLogger "MadGraphAuto" (setLevel DEBUG)
  args <- getArgs 
  when (length args /= 5) $ 
    fail "admproject_qld mgl msq msl mneut numofevent"
  let mgl :: Double = read (args !! 0) 
      msq :: Double = read (args !! 1) 
      msl :: Double = read (args !! 2)
      mneut :: Double = read (args !! 3) 
      n :: Int = read (args !! 4)
  evchainGen ADMXQLD111
    ( "/home/wavewave/repo/workspace/montecarlo/working"
    , "/home/wavewave/repo/ext/MadGraph5_v1_4_8_4/"
    , "/home/wavewave/repo/workspace/montecarlo/mc/" ) 
    ("Work20130228","2sq_2l2j2x") 
    (modelparam mgl msq msl mneut) 
    map_2sq_2l2j2x p_2sq_2l2j2x 
    n



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
