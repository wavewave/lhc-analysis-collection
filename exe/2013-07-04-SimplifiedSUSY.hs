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
import HEP.Automation.MadGraph.Model.SimplifiedSUSY
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

p_squark :: DDecay 
p_squark = d ( squarks, [t jets, neut ] )

p_gluino :: DDecay 
p_gluino = d ( [1000021], [t jets, t jets, neut] )


p_2sq_2j2n :: DCross 
p_2sq_2j2n = x (t proton, t proton, [p_squark, p_squark])

p_sqsg_3j2n :: DCross
p_sqsg_3j2n = x (t proton, t proton, [p_squark, p_gluino])

p_2sg_4j2n :: DCross
p_2sg_4j2n = x (t proton, t proton, [p_gluino, p_gluino])

map_2sq_2j2n :: ProcSpecMap
map_2sq_2j2n = 
    HM.fromList [ (Nothing, MGProc ["define  sq = ul ul~ dl dl~ sl sl~ cl cl~ ur ur~ dr dr~ sr sr~ cr cr~"] 
                                   ["p p > sq sq  QED=0"])
                , (Just (3, 1000001,[]), MGProc [] ["dl  > d n1"])
                , (Just (3,-1000001,[]), MGProc [] ["dl~ > d~ n1"])
                , (Just (3, 1000002,[]), MGProc [] ["ul  > u n1"])
                , (Just (3,-1000002,[]), MGProc [] ["ul~ > u~ n1"])
                , (Just (3, 1000003,[]), MGProc [] ["sl  > s n1"])
                , (Just (3,-1000003,[]), MGProc [] ["sl~ > s~ n1"])
                , (Just (3, 1000004,[]), MGProc [] ["cl  > c n1"])
                , (Just (3,-1000004,[]), MGProc [] ["cl~ > c~ n1"]) 
                --
                , (Just (3, 2000001,[]), MGProc [] ["dr  > d n1"])
                , (Just (3,-2000001,[]), MGProc [] ["dr~ > d~ n1"])
                , (Just (3, 2000002,[]), MGProc [] ["ur  > u n1"])
                , (Just (3,-2000002,[]), MGProc [] ["ur~ > u~ n1"])
                , (Just (3, 2000003,[]), MGProc [] ["sr  > s n1"])
                , (Just (3,-2000003,[]), MGProc [] ["sr~ > s~ n1"])
                , (Just (3, 2000004,[]), MGProc [] ["cr  > c n1"])
                , (Just (3,-2000004,[]), MGProc [] ["cr~ > c~ n1"]) 
                -- 
                , (Just (4, 1000001,[]), MGProc [] ["dl  > d n1"])
                , (Just (4,-1000001,[]), MGProc [] ["dl~ > d~ n1"])
                , (Just (4, 1000002,[]), MGProc [] ["ul  > u n1"])
                , (Just (4,-1000002,[]), MGProc [] ["ul~ > u~ n1"])
                , (Just (4, 1000003,[]), MGProc [] ["sl  > s n1"])
                , (Just (4,-1000003,[]), MGProc [] ["sl~ > s~ n1"])
                , (Just (4, 1000004,[]), MGProc [] ["cl  > c n1"])
                , (Just (4,-1000004,[]), MGProc [] ["cl~ > c~ n1"]) 
                --            
                , (Just (4, 2000001,[]), MGProc [] ["dr  > d n1"])
                , (Just (4,-2000001,[]), MGProc [] ["dr~ > d~ n1"])
                , (Just (4, 2000002,[]), MGProc [] ["ur  > u n1"])
                , (Just (4,-2000002,[]), MGProc [] ["ur~ > u~ n1"])
                , (Just (4, 2000003,[]), MGProc [] ["sr  > s n1"])
                , (Just (4,-2000003,[]), MGProc [] ["sr~ > s~ n1"])
                , (Just (4, 2000004,[]), MGProc [] ["cr  > c n1"])
                , (Just (4,-2000004,[]), MGProc [] ["cr~ > c~ n1"]) 
                ] 

map_sqsg_3j2n :: ProcSpecMap
map_sqsg_3j2n = 
    HM.fromList [ (Nothing, MGProc ["define  sq = ul ul~ dl dl~ sl sl~ cl cl~ ur ur~ dr dr~ sr sr~ cr cr~"] 
                                   ["p p > sq go  QED=0"])
                , (Just (3, 1000001,[]), MGProc [] ["dl  > d n1"])
                , (Just (3,-1000001,[]), MGProc [] ["dl~ > d~ n1"])
                , (Just (3, 1000002,[]), MGProc [] ["ul  > u n1"])
                , (Just (3,-1000002,[]), MGProc [] ["ul~ > u~ n1"])
                , (Just (3, 1000003,[]), MGProc [] ["sl  > s n1"])
                , (Just (3,-1000003,[]), MGProc [] ["sl~ > s~ n1"])
                , (Just (3, 1000004,[]), MGProc [] ["cl  > c n1"])
                , (Just (3,-1000004,[]), MGProc [] ["cl~ > c~ n1"]) 
                --
                , (Just (3, 2000001,[]), MGProc [] ["dr  > d n1"])
                , (Just (3,-2000001,[]), MGProc [] ["dr~ > d~ n1"])
                , (Just (3, 2000002,[]), MGProc [] ["ur  > u n1"])
                , (Just (3,-2000002,[]), MGProc [] ["ur~ > u~ n1"])
                , (Just (3, 2000003,[]), MGProc [] ["sr  > s n1"])
                , (Just (3,-2000003,[]), MGProc [] ["sr~ > s~ n1"])
                , (Just (3, 2000004,[]), MGProc [] ["cr  > c n1"])
                , (Just (3,-2000004,[]), MGProc [] ["cr~ > c~ n1"]) 
                -- 
                , (Just (4,1000021,[]), MGProc [] [ "go > j j n1 " ] )
                ] 

map_2sg_4j2n :: ProcSpecMap
map_2sg_4j2n = 
    HM.fromList [ (Nothing, MGProc [] ["p p > go go  QED=0"])
                , (Just (3,1000021,[]), MGProc [] [ "go > j j n1 " ] ) 
                , (Just (4,1000021,[]), MGProc [] [ "go > j j n1 " ] )
                ] 

mprocs = mkMultiProc pdir [ SingleProc "2sg_4j2n" p_2sg_4j2n map_2sg_4j2n mgrunsetup
                          , SingleProc "2sq_2j2n" p_2sq_2j2n map_2sq_2j2n mgrunsetup 
                          , SingleProc "sqsg_3j2n" p_sqsg_3j2n map_sqsg_3j2n mgrunsetup 
                          ] 

modelparam :: Double -> Double -> Double -> ModelParam SimplifiedSUSY
modelparam mneut mgl msq = SimplifiedSUSYParam mneut mgl msq 

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

pdir = ProcDir "Work20130704" "montecarlo/admproject/SimplifiedSUSY/8TeV" "scan"

{-
notgood =
  [ ("200.0","100.0")
  , ("200.0","200.0")
  , ("200.0","300.0")
  , ("200.0","400.0")
  , ("1400.0","1300.0")
  , ("1400.0","1400.0")
  , ("1400.0","1500.0")
  , ("1400.0","1600.0")
  , ("1400.0","1700.0")
  , ("1400.0","1800.0")
  , ("1400.0","1900.0")
  , ("1400.0","2000.0")
  , ("1500.0","100.0")
  , ("1500.0","200.0")
  , ("1500.0","300.0")
  , ("1500.0","400.0")
  , ("1500.0","500.0")
  , ("1500.0","600.0")
  , ("1500.0","700.0")
  , ("1500.0","800.0")
  , ("1500.0","900.0")
  , ("1500.0","1000.0")
  , ("1500.0","1100.0")
  , ("1500.0","1200.0")
  ]
-}

-- worksets = [ (10,mgl,msq,10000) | (mglstr,msqstr) <- notgood, let mgl = read mglstr, let msq = read msqstr] 

worksets = [ (10,mg,mq,10000) | mg <- [100,200..2000], mq <- [100,200..2000] ] 


main :: IO () 
main = do 
  args <- getArgs 
  let fp = args !! 0 
      cmd = args !! 1 
      n1 = read (args !! 2) :: Int
      n2 = read (args !! 3) :: Int
  --  fp <- (!! 0) <$> getArgs 
  updateGlobalLogger "MadGraphAuto" (setLevel DEBUG)
  -- print (length worksets) 
  mapM_ (scanwork fp cmd) (drop (n1-1) . take n2 $ worksets )


scanwork :: FilePath -> String -> (Double,Double,Double,Int) -> IO () 
scanwork fp cmd (mneut,mgl,msq,n) = do
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
          param = modelparam mneut mgl msq 
      let mjob = case cmd of 
                   "2sg"    -> Just ("2sg_4j2n", NumOfEv 10000, SetNum 1)
                   "2sq" -> Just ("2sq_2j2n", NumOfEv 10000, SetNum 1)
                   "sqsg" -> Just ("sqsg_3j2n", NumOfEv 10000, SetNum 1)
                   _ -> Nothing
      print mjob  
      maybe (return ()) (genMultiProcess SimplifiedSUSY ssetup mprocs param wdavcfg) mjob

      return ()
    )
   

