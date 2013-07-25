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
import           Data.Monoid
import           System.Directory
import           System.Environment
import           System.FilePath ((</>))
import           System.IO
import           System.Log.Logger
-- 
import HEP.Automation.EventChain.Driver 
import HEP.Automation.EventChain.File
import HEP.Automation.EventChain.LHEConn
import HEP.Automation.EventChain.Type.MultiProcess
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
import HEP.Automation.MadGraph.Model
import HEP.Automation.MadGraph.Model.SimplifiedSUSYlep
import HEP.Automation.MadGraph.Run
import HEP.Automation.MadGraph.SetupType
import HEP.Automation.MadGraph.Type
import HEP.Parser.LHE.Type
import HEP.Parser.LHE.Sanitizer.Type
import HEP.Storage.WebDAV
-- 
import qualified Paths_madgraph_auto as PMadGraph 
import qualified Paths_madgraph_auto_model as PModel 


jets = [1,2,3,4,-1,-2,-3,-4,21]

leptons = [11,13,-11,-13] 


lepplusneut = [11,12,13,14,-11,-12,-13,-14]

tauplusneut = [15,16,-15,-16]

lightobjs = jets++lepplusneut++tauplusneut

neut = 1000022

charginos = [1000024, -1000024]

squarkL     = [ 1000001, 1000002, 1000003, 1000004 ] 

antisquarkL = [-1000001,-1000002,-1000003,-1000004 ]

p_squark :: DDecay 
p_squark = d ( squarkL, [ p_chargino, t jets ] )

p_antisquark :: DDecay 
p_antisquark = d ( antisquarkL, [ p_chargino, t jets ] )


p_chargino :: DDecay 
p_chargino = d ( charginos, [neut, t lightobjs, t lightobjs] )

p_lep1step_2sq :: DCross 
p_lep1step_2sq = x (t proton, t proton, [p_squark, p_antisquark])

map_lep1step_2sq :: ProcSpecMap
map_lep1step_2sq = 
    HM.fromList [ (Nothing, MGProc ["define  sql = ul dl sl cl"
                                   ,"define  sql~ = ul~ dl~ sl~ cl~"]
                                   ["p p > sql sql~ QED=0"])
                , (Just (3, 1000001,[]), MGProc [] ["dl  > u  x1-"])
                , (Just (3, 1000002,[]), MGProc [] ["ul  > d  x1+"])
                , (Just (3, 1000003,[]), MGProc [] ["sl  > c  x1-"])
                , (Just (3, 1000004,[]), MGProc [] ["cl  > s  x1+"])
                -- 
                , (Just (4,-1000001,[]), MGProc [] ["dl~ > u~ x1+"])
                , (Just (4,-1000002,[]), MGProc [] ["ul~ > d~ x1-"])
                , (Just (4,-1000003,[]), MGProc [] ["sl~ > c~ x1+"])
                , (Just (4,-1000004,[]), MGProc [] ["cl~ > s~ x1-"]) 
                -- 
                , (Just (1, 1000024,[3]), MGProc [] [ "x1+ > w+ > n1 ve e+ QED=2"
                                                    , "x1+ > w+ > n1 vm mu+ QED=2"
                                                    , "x1+ > w+ > n1 vt ta+ QED=2"
                                                    , "x1+ > w+ > n1 j j QED=2" ])
                , (Just (1,-1000024,[3]), MGProc [] [ "x1- > w- > n1 ve~ e- QED=2"
                                                    , "x1- > w- > n1 vm~ mu- QED=2"
                                                    , "x1- > w- > n1 vt~ ta- QED=2"
                                                    , "x1- > w- > n1 j j QED=2" ])
                -- 
                , (Just (1, 1000024,[4]), MGProc [] [ "x1+ > w+ > n1 ve e+ QED=2"
                                                    , "x1+ > w+ > n1 vm mu+ QED=2"
                                                    , "x1+ > w+ > n1 vt ta+ QED=2"
                                                    , "x1+ > w+ > n1 j j QED=2" ])
                , (Just (1,-1000024,[4]), MGProc [] [ "x1- > w- > n1 ve~ e- QED=2"
                                                    , "x1- > w- > n1 vm~ mu- QED=2"
                                                    , "x1- > w- > n1 vt~ ta- QED=2"
                                                    , "x1- > w- > n1 j j QED=2" ])
                ] 

pdir = ProcDir "Work20130722" "montecarlo/admproject/SimplifiedSUSYlep/8TeV" "scan"

sproc = SingleProc "1step_2sq" p_lep1step_2sq map_lep1step_2sq mgrunsetup

mprocs = mkMultiProc pdir [sproc]



-- | 
mgrunsetup :: NumOfEv -> SetNum -> RunSetup
mgrunsetup (NumOfEv nev) (SetNum sn) = 
  RS { numevent = nev
     , machine = LHC8 ATLAS
     , rgrun   = Auto
     , rgscale = 200.0
     , match   = NoMatch
     , cut     = NoCut 
     , pythia  = RunPYTHIA 
     , lhesanitizer = [Replace [(9000201,1000022),(-9000201,1000022)]] 
     , pgs     = RunPGS (AntiKTJet 0.4,NoTau)
     , uploadhep = NoUploadHEP
     , setnum  = sn
     }


minfty :: Double
minfty = 50000.0

worksets :: [ (String, ModelParam SimplifiedSUSYlep, Int) ]
worksets = [ ("1step_2sq",SimplifiedSUSYlepParam mn minfty mq mc minfty minfty,10000) 
             | (mq,mn) <- mqmn, let mc = (mq+mn)*0.5 ]
  where mqmn = [ (mq,mn) | mq <- [ 200,250..1300 ], mn <- [ 50,100..mq-50] ] 


main :: IO () 
main = do 
  args <- getArgs 
  let fp = args !! 0 
      n1 = read (args !! 1) :: Int
      n2 = read (args !! 2) :: Int
  updateGlobalLogger "MadGraphAuto" (setLevel DEBUG) 
  -- print $ length worksets
  -- mapM_ print $ worksets
  mapM_ (scanwork fp) (drop (n1-1) . take n2 $ worksets )


scanwork :: FilePath -> (String,ModelParam SimplifiedSUSYlep,Int) -> IO () 
scanwork fp (cmd,param,n) = do
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
          -- param = modelparam mneut mgl msq 
      let mjob = Just ("1step_2sq", NumOfEv n, SetNum 1)
      -- print mjob  
      -- maybe (return ()) (genMultiProcess SimplifiedSUSYlep ssetup mprocs param wdavcfg) mjob
      
      let nev = NumOfEv n
          sn = SetNum 1
      genPhase1 SimplifiedSUSYlep ssetup pdir sproc param (nev,sn)
      genPhase2 SimplifiedSUSYlep ssetup pdir sproc param (nev,sn)
      genPhase3 SimplifiedSUSYlep ssetup pdir sproc param (nev,sn) wdavcfg
      
      return ()
    )
   

