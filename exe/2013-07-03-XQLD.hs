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

adms = [9000201,-9000201,9000202,-9000202]

sup = [1000002,-1000002] 

sdownR = [2000001,-2000001]

 

othersq = [ 1000001, -1000001, 1000002, -1000002, 1000003, -1000003, 1000004, -1000004
                             , 2000002, -2000002, 2000003, -2000003, 2000004, -2000004 ] 


p_gluino = d ([1000021], [t lepplusneut, t jets, t jets, t adms])

p_sdownR :: DDecay
p_sdownR = d (sdownR, [t lepplusneut, t jets, t adms])

p_othersq :: DDecay
p_othersq = d ( othersq, [p_sdownR, t jets, t jets])



p_2sg_2l4j2x :: DCross 
p_2sg_2l4j2x = x (t proton, t proton, [p_gluino, p_gluino])

p_2sq_oo_2l2j2x :: DCross 
p_2sq_oo_2l2j2x = x (t proton, t proton, [p_sdownR, p_sdownR])

p_2sq_no_2l2j2x :: DCross 
p_2sq_no_2l2j2x = x (t proton, t proton, [p_sdownR, p_othersq])

p_2sq_nn_2l2j2x :: DCross 
p_2sq_nn_2l2j2x = x (t proton, t proton, [p_othersq, p_othersq])

p_sqsg_o_2l3j2x :: DCross 
p_sqsg_o_2l3j2x = x (t proton, t proton, [p_gluino, p_sdownR])

p_sqsg_n_2l3j2x :: DCross 
p_sqsg_n_2l3j2x = x (t proton, t proton, [p_gluino, p_othersq])

map_2sg_2l4j2x :: ProcSpecMap
map_2sg_2l4j2x = 
    HM.fromList [ (Nothing            , MGProc [ ]  
                                               [ "p p > go go QED=0" ])
                ,(Just (3,1000021,[]), MGProc [ "define lep = e+ e- mu+ mu- ve ve~ vm vm~ " 
                                              , "define sxx = sxxp sxxp~ "]
                                              [ "go > lep j j sxx " ] ) 
                ,(Just (4,1000021,[]), MGProc [ "define lep = e+ e- mu+ mu- ve ve~ vm vm~ "
                                              , "define sxx = sxxp sxxp~ "] 
                                              [ "go > lep j j sxx " ] )
                ] 

map_2sq_oo_2l2j2x :: ProcSpecMap
map_2sq_oo_2l2j2x = 
    HM.fromList [(Nothing             , MGProc [] [ "p p > dr dr~ QED=0"
                                                  , "p p > dr dr QED=0"
                                                  , "p p > dr~ dr~ QED=0"])
                ,(Just (3,-2000001,[]), MGProc [] [ "dr~ > u~ e+ sxxp~" 
                                                  , "dr~ > d~ ve~ sxxp~" ])
                ,(Just (3,2000001,[]) , MGProc [] [ "dr > u e- sxxp" 
                                                  , "dr > d ve sxxp" ])
                ,(Just (4,-2000001,[]), MGProc [] [ "dr~ > u~ e+ sxxp~ "
                                                  , "dr~ > d~ ve~ sxxp~ " ])
                ,(Just (4,2000001,[]) , MGProc [] [ "dr > u e- sxxp "
                                                  , "dr > d ve sxxp " ])
                ] 

map_2sq_no_2l2j2x :: ProcSpecMap
map_2sq_no_2l2j2x = 
    HM.fromList 
      [ (Nothing             , MGProc [ "define osq = ul ul~ cl cl~ ur ur~ cr cr~ dl dl~ sl sl~ sr sr~"
                                      , "define drs = dr dr~ "]  
                                      [ "p p > drs osq QED=0" ])
      , (Just (3,-2000001,[]), MGProc [] 
                                      [ "dr~ > u~ e+ sxxp~" 
                                      , "dr~ > d~ ve~ sxxp~" ])
      , (Just (3,2000001,[]) , MGProc [] 
                                      [ "dr > u e- sxxp" 
                                      , "dr > d ve sxxp" ])
      , (Just (4,-1000001,[]), MGProc [ "define drs = dr dr~" ] 
                                      [ "dl~ > j j drs QED=0" ] )
      , (Just (4, 1000001,[]), MGProc [ "define drs = dr dr~" ] 
                                      [ "dl > j j drs QED=0" ] )
      , (Just (4,-1000002,[]), MGProc [ "define drs = dr dr~" ] 
                                      [ "ul~ > j j drs QED=0" ] )
      , (Just (4, 1000002,[]), MGProc [ "define drs = dr dr~" ] 
                                      [ "ul > j j drs QED=0" ] )
      , (Just (4,-1000003,[]), MGProc [ "define drs = dr dr~" ] 
                                      [ "sl~ > j j drs QED=0" ] )
      , (Just (4, 1000003,[]), MGProc [ "define drs = dr dr~" ] 
                                      [ "sl  > j j drs QED=0" ] )
      , (Just (4,-1000004,[]), MGProc [ "define drs = dr dr~" ] 
                                      [ "cl~ > j j drs QED=0" ] )
      , (Just (4, 1000004,[]), MGProc [ "define drs = dr dr~" ] 
                                      [ "cl  > j j drs QED=0" ] )
      , (Just (4,-2000002,[]), MGProc [ "define drs = dr dr~" ] 
                                      [ "ur~ > j j drs QED=0" ] )
      , (Just (4, 2000002,[]), MGProc [ "define drs = dr dr~" ] 
                                      [ "ur  > j j drs QED=0" ]) 
      , (Just (4,-2000003,[]), MGProc [ "define drs = dr dr~" ] 
                                      [ "sr~ > j j drs QED=0" ] )
      , (Just (4, 2000003,[]), MGProc [ "define drs = dr dr~" ] 
                                      [ "sr  > j j drs QED=0" ] )
      , (Just (4,-2000004,[]), MGProc [ "define drs = dr dr~" ] 
                                      [ "cr~ > j j drs QED=0" ] )
      , (Just (4, 2000004,[]), MGProc [ "define drs = dr dr~" ] 
                                      [ "cr  > j j drs QED=0" ]) 
      , (Just (1,-2000001,[4]), MGProc [] [ "dr~ > u~ e+ sxxp~" 
                                          , "dr~ > d~ ve~ sxxp~" ] ) 
      , (Just (1, 2000001,[4]), MGProc [] [ "dr > u e- sxxp "
                                         , "dr > d ve sxxp " ])
      ] 

map_2sq_nn_2l2j2x :: ProcSpecMap
map_2sq_nn_2l2j2x = 
    HM.fromList 
      [ (Nothing             , MGProc [ "define osq = ul ul~ cl cl~ ur ur~ cr cr~ dl dl~ sl sl~ sr sr~" ]
                                      [ "p p > osq osq QED=0" ])
      , (Just (3,-1000001,[]), MGProc [ "define drs = dr dr~" ] 
                                      [ "dl~ > j j drs QED=0" ] )
      , (Just (3, 1000001,[]), MGProc [ "define drs = dr dr~" ] 
                                      [ "dl > j j drs QED=0" ] )
      , (Just (3,-1000002,[]), MGProc [ "define drs = dr dr~" ] 
                                      [ "ul~ > j j drs QED=0" ] )
      , (Just (3, 1000002,[]), MGProc [ "define drs = dr dr~" ] 
                                      [ "ul > j j drs QED=0" ] )
      , (Just (3,-1000003,[]), MGProc [ "define drs = dr dr~" ] 
                                      [ "sl~ > j j drs QED=0" ] )
      , (Just (3, 1000003,[]), MGProc [ "define drs = dr dr~" ] 
                                      [ "sl  > j j drs QED=0" ] )
      , (Just (3,-1000004,[]), MGProc [ "define drs = dr dr~" ] 
                                      [ "cl~ > j j drs QED=0" ] )
      , (Just (3, 1000004,[]), MGProc [ "define drs = dr dr~" ] 
                                      [ "cl  > j j drs QED=0" ] )
      , (Just (3,-2000002,[]), MGProc [ "define drs = dr dr~" ] 
                                      [ "ur~ > j j drs QED=0" ] )
      , (Just (3, 2000002,[]), MGProc [ "define drs = dr dr~" ] 
                                      [ "ur  > j j drs QED=0" ]) 
      , (Just (3,-2000003,[]), MGProc [ "define drs = dr dr~" ] 
                                      [ "sr~ > j j drs QED=0" ] )
      , (Just (3, 2000003,[]), MGProc [ "define drs = dr dr~" ] 
                                      [ "sr  > j j drs QED=0" ] )
      , (Just (3,-2000004,[]), MGProc [ "define drs = dr dr~" ] 
                                      [ "cr~ > j j drs QED=0" ] )
      , (Just (3, 2000004,[]), MGProc [ "define drs = dr dr~" ] 
                                      [ "cr  > j j drs QED=0" ]) 
      -- 
      , (Just (4,-1000001,[]), MGProc [ "define drs = dr dr~" ] 
                                      [ "dl~ > j j drs QED=0" ] )
      , (Just (4, 1000001,[]), MGProc [ "define drs = dr dr~" ] 
                                      [ "dl > j j drs QED=0" ] )
      , (Just (4,-1000002,[]), MGProc [ "define drs = dr dr~" ] 
                                      [ "ul~ > j j drs QED=0" ] )
      , (Just (4, 1000002,[]), MGProc [ "define drs = dr dr~" ] 
                                      [ "ul > j j drs QED=0" ] )
      , (Just (4,-1000003,[]), MGProc [ "define drs = dr dr~" ] 
                                      [ "sl~ > j j drs QED=0" ] )
      , (Just (4, 1000003,[]), MGProc [ "define drs = dr dr~" ] 
                                      [ "sl  > j j drs QED=0" ] )
      , (Just (4,-1000004,[]), MGProc [ "define drs = dr dr~" ] 
                                      [ "cl~ > j j drs QED=0" ] )
      , (Just (4, 1000004,[]), MGProc [ "define drs = dr dr~" ] 
                                      [ "cl  > j j drs QED=0" ] )
      , (Just (4,-2000002,[]), MGProc [ "define drs = dr dr~" ] 
                                      [ "ur~ > j j drs QED=0" ] )
      , (Just (4, 2000002,[]), MGProc [ "define drs = dr dr~" ] 
                                      [ "ur  > j j drs QED=0" ]) 
      , (Just (4,-2000003,[]), MGProc [ "define drs = dr dr~" ] 
                                      [ "sr~ > j j drs QED=0" ] )
      , (Just (4, 2000003,[]), MGProc [ "define drs = dr dr~" ] 
                                      [ "sr  > j j drs QED=0" ] )
      , (Just (4,-2000004,[]), MGProc [ "define drs = dr dr~" ] 
                                      [ "cr~ > j j drs QED=0" ] )
      , (Just (4, 2000004,[]), MGProc [ "define drs = dr dr~" ] 
                                      [ "cr  > j j drs QED=0" ]) 
      , (Just (1,-2000001,[3]), MGProc [] [ "dr~ > u~ e+ sxxp~" 
                                          , "dr~ > d~ ve~ sxxp~" ] ) 
      , (Just (1, 2000001,[3]), MGProc [] [ "dr > u e- sxxp "
                                         , "dr > d ve sxxp " ])
      , (Just (1,-2000001,[4]), MGProc [] [ "dr~ > u~ e+ sxxp~" 
                                          , "dr~ > d~ ve~ sxxp~" ] ) 
      , (Just (1, 2000001,[4]), MGProc [] [ "dr > u e- sxxp "
                                         , "dr > d ve sxxp " ])
      ] 

map_sqsg_o_2l3j2x :: ProcSpecMap
map_sqsg_o_2l3j2x = 
    HM.fromList [ (Nothing            , MGProc [ "define drs = dr dr~ "]  
                                               [ "p p > go drs QED=0" ])
                , (Just (3,1000021,[]), MGProc [ "define lep = e+ e- mu+ mu- ve ve~ vm vm~ " 
                                               , "define sxx = sxxp sxxp~ "]
                                               [ "go > lep j j sxx " ] ) 
                , (Just (4,-2000001,[]), MGProc [] [ "dr~ > u~ e+ sxxp~" 
                                                   , "dr~ > d~ ve~ sxxp~" ] ) 
                , (Just (4, 2000001,[]), MGProc [] [ "dr > u e- sxxp "
                                                   , "dr > d ve sxxp " ])
                ] 

map_sqsg_n_2l3j2x :: ProcSpecMap
map_sqsg_n_2l3j2x = 
    HM.fromList [ (Nothing            , MGProc [ "define osq = ul ul~ cl cl~ ur ur~ cr cr~ dl dl~ sl sl~ sr sr~"]  
                                               [ "p p > go osq QED=0" ])
                , (Just (3,1000021,[]), MGProc [ "define lep = e+ e- mu+ mu- ve ve~ vm vm~ " 
                                               , "define sxx = sxxp sxxp~ "]
                                               [ "go > lep j j sxx " ] ) 
                , (Just (4,-1000001,[]), MGProc [ "define drs = dr dr~" ] 
                                                [ "dl~ > j j drs QED=0" ] )
                , (Just (4, 1000001,[]), MGProc [ "define drs = dr dr~" ] 
                                                [ "dl > j j drs QED=0" ] )
                , (Just (4,-1000002,[]), MGProc [ "define drs = dr dr~" ] 
                                                [ "ul~ > j j drs QED=0" ] )
                , (Just (4, 1000002,[]), MGProc [ "define drs = dr dr~" ] 
                                                [ "ul > j j drs QED=0" ] )
                , (Just (4,-1000003,[]), MGProc [ "define drs = dr dr~" ] 
                                                [ "sl~ > j j drs QED=0" ] )
                , (Just (4, 1000003,[]), MGProc [ "define drs = dr dr~" ] 
                                                [ "sl  > j j drs QED=0" ] )
                , (Just (4,-1000004,[]), MGProc [ "define drs = dr dr~" ] 
                                                [ "cl~ > j j drs QED=0" ] )
                , (Just (4, 1000004,[]), MGProc [ "define drs = dr dr~" ] 
                                                [ "cl  > j j drs QED=0" ] )
                , (Just (4,-2000002,[]), MGProc [ "define drs = dr dr~" ] 
                                                [ "ur~ > j j drs QED=0" ] )
                , (Just (4, 2000002,[]), MGProc [ "define drs = dr dr~" ] 
                                                [ "ur  > j j drs QED=0" ]) 
                , (Just (4,-2000003,[]), MGProc [ "define drs = dr dr~" ] 
                                                [ "sr~ > j j drs QED=0" ] )
                , (Just (4, 2000003,[]), MGProc [ "define drs = dr dr~" ] 
                                                [ "sr  > j j drs QED=0" ] )
                , (Just (4,-2000004,[]), MGProc [ "define drs = dr dr~" ] 
                                                [ "cr~ > j j drs QED=0" ] )
                , (Just (4, 2000004,[]), MGProc [ "define drs = dr dr~" ] 
                                                [ "cr  > j j drs QED=0" ]) 
                , (Just (1,-2000001,[4]), MGProc [] [ "dr~ > u~ e+ sxxp~" 
                                                    , "dr~ > d~ ve~ sxxp~" ] ) 
                , (Just (1, 2000001,[4]), MGProc [] [ "dr > u e- sxxp "
                                                   , "dr > d ve sxxp " ])
                ] 



mprocs = mkMultiProc pdir [ SingleProc "2sg_2l4j2x" p_2sg_2l4j2x map_2sg_2l4j2x mgrunsetup
                          , SingleProc "2sq_oo_2l2j2x" p_2sq_oo_2l2j2x map_2sq_oo_2l2j2x mgrunsetup 
                          , SingleProc "2sq_no_2l2j2x" p_2sq_no_2l2j2x map_2sq_no_2l2j2x mgrunsetup 
                          , SingleProc "2sq_nn_2l2j2x" p_2sq_nn_2l2j2x map_2sq_nn_2l2j2x mgrunsetup 
                          , SingleProc "sqsg_o_2l3j2x" p_sqsg_o_2l3j2x map_sqsg_o_2l3j2x mgrunsetup 
                          , SingleProc "sqsg_n_2l3j2x" p_sqsg_n_2l3j2x map_sqsg_n_2l3j2x mgrunsetup ]

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
     , pythia  = RunPYTHIA 
     , lhesanitizer = [Replace [(9000201,1000022),(-9000201,1000022)]] 
     , pgs     = RunPGS (AntiKTJet 0.4,NoTau)
     , uploadhep = NoUploadHEP
     , setnum  = sn
     }

pdir = ProcDir "Work20130702" "montecarlo/admproject/XQLDdegen/8TeV" "scan"


worksets :: [ (String, (Double,Double,Double,Double,Int)) ]
worksets = set_2sg <> set_sqsg_o <> set_sqsg_n <> set_2sq_oo <> set_2sq_no <> set_2sq_nn
  where   
    makeset str lst = 
     [ (str,(mgl,msq,50000,50000,10000)) | (mgl,msq) <- lst ] 
    set_2sg = makeset "2sg" massset_2sg 
    set_sqsg_o = makeset "sqsg_o" massset_sqsg_o 
    set_sqsg_n = makeset "sqsg_n" massset_sqsg_n 
    set_2sq_oo = makeset "2sq_oo" massset_2sq_oo
    set_2sq_no = makeset "2sq_no" massset_2sq_no 
    set_2sq_nn = makeset "2sq_nn" massset_2sq_nn 


mesh = [ (g, q) | g <- [100,200..3000], q <- [100,200..3000] ]

massset_2sg = mesh
massset_sqsg_o = mesh
massset_sqsg_n = mesh
massset_2sq_oo = mesh 
massset_2sq_no = mesh
massset_2sq_nn = mesh 



main :: IO () 
main = do 
  args <- getArgs 
  let fp = args !! 0 
      n1 = read (args !! 1) :: Int
      n2 = read (args !! 2) :: Int
  updateGlobalLogger "MadGraphAuto" (setLevel DEBUG)
  print (length worksets) 
  -- mapM_ (scanwork fp) (drop (n1-1) . take n2 $ worksets )


setN = 2 


scanwork :: FilePath -> (String,(Double,Double,Double,Double,Int)) -> IO () 
scanwork fp (cmd,(mgl,msq,msl,mneut,n)) = do
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
                   "2sg"    -> Just ("2sg_2l4j2x", NumOfEv 10000, SetNum setN)
                   "2sq_oo" -> Just ("2sq_oo_2l2j2x", NumOfEv 10000, SetNum setN)
                   "2sq_no" -> Just ("2sq_no_2l2j2x", NumOfEv 10000, SetNum setN)
                   "2sq_nn" -> Just ("2sq_nn_2l2j2x", NumOfEv 10000, SetNum setN)
                   "sqsg_o" -> Just ("sqsg_o_2l3j2x", NumOfEv 10000, SetNum setN)
                   "sqsg_n" -> Just ("sqsg_n_2l3j2x", NumOfEv 10000, SetNum setN)
                   _ -> Nothing
      print mjob  
      maybe (return ()) (genMultiProcess ADMXQLD111degen ssetup mprocs param wdavcfg) mjob
        
      return ()
    )
   

