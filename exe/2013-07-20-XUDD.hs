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
import HEP.Automation.MadGraph.Model.ADMXUDD112degen
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

-- leptons = [11,13,-11,-13] 

-- lepplusneut = [11,12,13,14,-11,-12,-13,-14]

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
p_neut = d (neut, [t jets, t jets, t jets, t adms])

p_squark :: DDecay 
p_squark = d (squarks, [p_neut, t jets])

p_2sg_10j2x :: DCross 
p_2sg_10j2x = x (t proton, t proton, [p_gluino, p_gluino])

p_sqsg_9j2x :: DCross 
p_sqsg_9j2x = x (t proton, t proton, [p_gluino, p_squark])

p_2sq_8j2x :: DCross 
p_2sq_8j2x = x (t proton, t proton, [p_squark, p_squark])


map_2sg_10j2x :: ProcSpecMap
map_2sg_10j2x = 
    HM.fromList [ (Nothing             , MGProc [] [ "p p > go go QED=0" ])
                , (Just (3,1000021,[]), MGProc []
                                               [ "go > n1 j j " ] ) 
                , (Just (4,1000021,[]), MGProc [] 
                                               [ "go > n1 j j " ] )
                , (Just (1,1000022,[3]), MGProc [ "define sxx = sxxp sxxp~ " ] 
                                                [ "n1 > sxx j j j " ] )
                , (Just (1,1000022,[4]), MGProc [ "define sxx = sxxp sxxp~ " ]
                                                [ "n1 > sxx j j j " ] )
                                
                ] 


map_sqsg_9j2x :: ProcSpecMap
map_sqsg_9j2x = 
    HM.fromList 
      [ (Nothing, MGProc [ "define sq =  ul ul~ cl cl~ ur ur~ cr cr~ dl dl~ sl sl~ dr dr~ sr sr~" ] 
                         [ "p p > go sq QED=0"])
      , (Just (3,1000021,[]), MGProc []  [ "go > n1 j j " ] ) 
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
      , (Just (1,1000022,[3]), MGProc [ "define sxx = sxxp sxxp~ " ] 
                                      [ "n1 > sxx j j j " ] )
      , (Just (1,1000022,[4]), MGProc [ "define sxx = sxxp sxxp~ " ]
                                      [ "n1 > sxx j j j " ] )
      ]



map_2sq_8j2x :: ProcSpecMap
map_2sq_8j2x = 
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
      , (Just (1,1000022,[3]), MGProc [ "define sxx = sxxp sxxp~ " ] 
                                      [ "n1 > sxx j j j " ] )
      , (Just (1,1000022,[4]), MGProc [ "define sxx = sxxp sxxp~ " ]
                                      [ "n1 > sxx j j j " ] )
      ]

mprocs = mkMultiProc pdir [ SingleProc "2sg_10j2x" p_2sg_10j2x map_2sg_10j2x mgrunsetup
                          , SingleProc "sqsg_9j2x" p_sqsg_9j2x map_sqsg_9j2x mgrunsetup 
                          , SingleProc "2sq_8j2x" p_2sq_8j2x map_2sq_8j2x mgrunsetup 
                          ]


modelparam mgl msq msl mneut = ADMXUDD112degenParam mgl msq msl mneut 

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

pdir = ProcDir "Work20130720" "montecarlo/admproject/XUDDdegen/8TeV/neutLOSP" "scan"

m_neutralino :: Double 
m_neutralino = 500

worksets :: [ (String, (Double,Double,Double,Double,Int)) ]
worksets = set_2sg <> set_sqsg <> set_2sq 
  where   
    makeset str lst = 
     [ (str,(mgl,msq,50000,m_neutralino,10000)) | (mgl,msq) <- lst ] 
    set_2sg  = makeset "2sg" massset_2sg 
    set_sqsg = makeset "sqsg" massset_sqsg 
    set_2sq  = makeset "2sq" massset_2sq


mesh = [ (g, q) | g<- [m_neutralino+100,m_neutralino+200..3000], q<- [m_neutralino+100,m_neutralino+200..3000] ]

massset_2sg = mesh
massset_sqsg = mesh
massset_2sq = mesh 
 

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
                   "2sg"    -> Just ("2sg_10j2x", NumOfEv 10000, SetNum 1)
                   "sqsg" -> Just ("sqsg_9j2x", NumOfEv 10000, SetNum 1)
                   "2sq" -> Just ("2sq_8j2x", NumOfEv 10000, SetNum 1)
                   _ -> Nothing
      print mjob  
      maybe (return ()) (genMultiProcess ADMXUDD112degen ssetup mprocs param wdavcfg) mjob
    )
   

