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

 

p_gluino :: DDecay 
p_gluino = d ([1000021], [p_neut, t jets, t jets])

p_neut :: DDecay 
p_neut = d (neut, [t lepplusneut, t jets, t jets, t adms])

p_squark :: DDecay 
p_squark = d (squarks, [p_neut, t jets])

p_2sg_2l8j2x :: DCross 
p_2sg_2l8j2x = x (t proton, t proton, [p_gluino, p_gluino])


map_2sg_2l8j2x :: ProcSpecMap
map_2sg_2l8j2x = 
    HM.fromList [ (Nothing             , MGProc [] [ "p p > go go QED=0" ])
                , (Just (3,1000021,[]), MGProc []
                                               [ "go > n1 j j " ] ) 
                , (Just (4,1000021,[]), MGProc [] 
                                               [ "go > n1 j j " ] )
                , (Just (1,1000022,[3]), MGProc [ "define lep = e+ e- mu+ mu- ve ve~ vm vm~ "
                                                , "define sxx = sxxp sxxp~ " ] 
                                                [ "n1 > sxx lep j j " ] )
                , (Just (1,1000022,[4]), MGProc [ "define lep = e+ e- mu+ mu- ve ve~ vm vm~ "
                                                , "define sxx = sxxp sxxp~ " ]
                                                [ "n1 > sxx lep j j " ] )
                                
                ] 


mprocs = mkMultiProc pdir [ SingleProc "2sg_2l8j2x" p_2sg_2l8j2x map_2sg_2l8j2x mgrunsetup
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

pdir = ProcDir "Work20130805" "montecarlo/admproject/XQLDdegen/8TeV/neutLOSP_mgmnscan" "scan"

m_neutralino :: Double 
m_neutralino = 500.0

worksets :: [ (String, (Double,Double,Double,Double,Int)) ]
worksets = set_2sg 
  where   
    makeset str lst = 
     [ (str,(mg,50000.0,50000.0,mn,10000)) | (mg,mn) <- lst ] 
    set_2sg  = makeset "2sg" massset_2sg 


massset_2sg = [ (mg, mn) | mg <- [ 200,250..1500 ], mn <- [ 50,100..mg-50] ]


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
                   "2sg"    -> Just ("2sg_2l8j2x", NumOfEv n, SetNum 1)
                   _ -> Nothing
      print mjob  
      maybe (return ()) (genMultiProcess ADMXQLD111degen ssetup mprocs param wdavcfg) mjob
    )
   

