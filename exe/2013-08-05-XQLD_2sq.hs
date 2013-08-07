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
import           Data.Monoid ((<>))
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
import HEP.Automation.MadGraph.Model.ADMXQLD111degen
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

neut = [1000022]

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

 


p_neut :: DDecay 
p_neut = d (neut, [t lepplusneut, t jets, t jets, t adms])

p_squark :: DDecay 
p_squark = d (squarks, [p_neut, t jets])


p_2sq_2l6j2x :: DCross 
p_2sq_2l6j2x = x (t proton, t proton, [p_squark, p_squark])



map_2sq_2l6j2x :: ProcSpecMap
map_2sq_2l6j2x = 
    HM.fromList 
      [ (Nothing, MGProc [ "define sq =  ul ul~ cl cl~ ur ur~ cr cr~ dl dl~ sl sl~ dr dr~ sr sr~" ] 
                         [ "p p > sq sq QED=0"])
      , (Just (3,-1000001,[]), MGProc [] [ "dl~ > n1 j " ] )
      , (Just (3, 1000001,[]), MGProc [] [ "dl  > n1 j " ] )
      , (Just (3,-1000002,[]), MGProc [] [ "ul~ > n1 j " ] )
      , (Just (3, 1000002,[]), MGProc [] [ "ul  > n1 j " ] )
      , (Just (3,-1000003,[]), MGProc [] [ "sl~ > n1 j " ] )
      , (Just (3, 1000003,[]), MGProc [] [ "sl  > n1 j " ] )
      , (Just (3,-1000004,[]), MGProc [] [ "cl~ > n1 j " ] )
      , (Just (3, 1000004,[]), MGProc [] [ "cl  > n1 j " ] )
      , (Just (3,-2000001,[]), MGProc [] [ "dr~ > n1 j " ] )
      , (Just (3, 2000001,[]), MGProc [] [ "dr  > n1 j " ] )
      , (Just (3,-2000002,[]), MGProc [] [ "ur~ > n1 j " ] )
      , (Just (3, 2000002,[]), MGProc [] [ "ur  > n1 j " ]) 
      , (Just (3,-2000003,[]), MGProc [] [ "sr~ > n1 j " ] )
      , (Just (3, 2000003,[]), MGProc [] [ "sr  > n1 j " ] )
      , (Just (3,-2000004,[]), MGProc [] [ "cr~ > n1 j " ] )
      , (Just (3, 2000004,[]), MGProc [] [ "cr  > n1 j " ]) 
      -- 
      , (Just (4,-1000001,[]), MGProc [] [ "dl~ > n1 j " ] )
      , (Just (4, 1000001,[]), MGProc [] [ "dl  > n1 j " ] )
      , (Just (4,-1000002,[]), MGProc [] [ "ul~ > n1 j " ] )
      , (Just (4, 1000002,[]), MGProc [] [ "ul  > n1 j " ] )
      , (Just (4,-1000003,[]), MGProc [] [ "sl~ > n1 j " ] )
      , (Just (4, 1000003,[]), MGProc [] [ "sl  > n1 j " ] )
      , (Just (4,-1000004,[]), MGProc [] [ "cl~ > n1 j " ] )
      , (Just (4, 1000004,[]), MGProc [] [ "cl  > n1 j " ] )
      , (Just (4,-2000001,[]), MGProc [] [ "dr~ > n1 j " ] )
      , (Just (4, 2000001,[]), MGProc [] [ "dr  > n1 j " ] )
      , (Just (4,-2000002,[]), MGProc [] [ "ur~ > n1 j " ] )
      , (Just (4, 2000002,[]), MGProc [] [ "ur  > n1 j " ]) 
      , (Just (4,-2000003,[]), MGProc [] [ "sr~ > n1 j " ] )
      , (Just (4, 2000003,[]), MGProc [] [ "sr  > n1 j " ] )
      , (Just (4,-2000004,[]), MGProc [] [ "cr~ > n1 j " ] )
      , (Just (4, 2000004,[]), MGProc [] [ "cr  > n1 j " ]) 
      --
      , (Just (1,1000022,[3]), MGProc [ "define lep = e+ e- mu+ mu- ve ve~ vm vm~ "
                                      , "define sxx = sxxp sxxp~ " ] 
                                      [ "n1 > sxx lep j j " ] )
      , (Just (1,1000022,[4]), MGProc [ "define lep = e+ e- mu+ mu- ve ve~ vm vm~ "
                                      , "define sxx = sxxp sxxp~ " ]
                                      [ "n1 > sxx lep j j " ] )
      ]

mprocs = mkMultiProc pdir [ SingleProc "2sq_2l6j2x" p_2sq_2l6j2x map_2sq_2l6j2x mgrunsetup 
                          ]


modelparam mgl msq msl mneut = ADMXQLD111degenParam mgl msq msl mneut 

-- | 
mgrunsetup :: NumOfEv -> SetNum -> RunSetup
mgrunsetup (NumOfEv nev) (SetNum sn) = 
  RS { numevent = nev
     , machine = LHC8 ATLAS
     , rgrun   = Auto
     , rgscale = 200.0
     , match   = NoMatch
     , cut     = NoCut 
     , pythia  = RunPYTHIA8 
     , lhesanitizer = [Replace [(9000201,1000022),(-9000201,1000022)]] 
     , pgs     = RunPGS (AntiKTJet 0.4,NoTau)
     , uploadhep = NoUploadHEP
     , setnum  = sn
     }

pdir = ProcDir "Work201308052sq" "montecarlo/admproject/XQLDdegen/8TeV/neutLOSP_mqmnscan" "scan"


worksets :: [ (String, (Double,Double,Double,Double,Int)) ]
worksets = set_2sq 
  where   
    makeset str lst = 
     [ (str,(50000.0,mq,50000.0,mn,10000)) | (mq,mn) <- lst ] 
    set_2sq  = makeset "2sq" massset_2sq


massset_2sq = [ (450.0,250.0), (500.0,250.0), (500.0,300.0), (500.0, 350.0), (550.0,150.0) ]

-- [ (mq,mn) | mq <- [ 200,250..1300], mn <- [ 50,100..mq-50 ] ]

main :: IO () 
main = do 
  args <- getArgs 
  let fp = args !! 0 
      -- cmd = args !! 1 
      n1 = read (args !! 1) :: Int
      n2 = read (args !! 2) :: Int
  updateGlobalLogger "MadGraphAuto" (setLevel DEBUG)
  -- print (length worksets) 
  mapM_ (scanwork fp) (drop (n1-1) . take n2 $ worksets )




scanwork :: FilePath -> (String, (Double,Double,Double,Double,Int)) -> IO () 
scanwork fp (cmd, (mgl,msq,msl,mneut,n)) = do
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
          param = modelparam mgl msq msl mneut
      let mjob = case cmd of 
                   "2sq" -> Just ("2sq_2l6j2x", NumOfEv n, SetNum 1)
                   _ -> Nothing
      print mjob  
      maybe (return ()) (genMultiProcess ADMXQLD111degen ssetup mprocs param wdavcfg) mjob
    )
   

