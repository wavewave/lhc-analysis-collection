{-# LANGUAGE ScopedTypeVariables, NoMonomorphismRestriction, RecordWildCards #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import           Control.Applicative
import           Control.Monad 
import           Control.Monad.Error
import           Control.Monad.Reader 
import           Control.Monad.State 
import qualified Data.Aeson.Generic as G
import           Data.Aeson.Types hiding (parse)
import qualified Data.ByteString.Lazy.Char8 as LB
import           Data.Conduit
import qualified Data.Conduit.List as CL
import           Data.Data
-- import           Data.Text hiding (map)
import qualified Data.Traversable as T
import qualified Data.HashMap.Lazy as HM
import           Data.Maybe 
import           System.Directory
import           System.Environment
import           System.FilePath ((</>), (<.>))
import           System.IO
import           System.Log.Logger
-- 
import HEP.Automation.MadGraph.Model.ADMXUDD112
import HEP.Automation.MadGraph.Run
import HEP.Automation.MadGraph.SetupType
import HEP.Automation.MadGraph.Type
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
import HEP.Automation.EventGeneration.Config
import HEP.Automation.EventGeneration.Type
import HEP.Automation.EventGeneration.Work 
import HEP.Parser.LHE.Type
import HEP.Parser.LHE.Sanitizer.Type
import HEP.Storage.WebDAV
-- 
import qualified Paths_madgraph_auto as PMadGraph 
import qualified Paths_madgraph_auto_model as PModel 

data WorkSet = WorkSet { mgluino :: Double, msquark :: Double } 
             deriving (Show, Typeable, Data) 

instance ToJSON WorkSet where toJSON = G.toJSON


jets = [1,2,3,4,-1,-2,-3,-4,21]

leptons = [11,13,-11,-13] 

lepplusneut = [11,12,13,14,-11,-12,-13,-14]

adms = [9000201,-9000201,9000202,-9000202]

sup = [1000002,-1000002] 

sdownR = [2000001,-2000001]

p_gluino = d ([1000021], [t jet, t jets, t jets, t adms])


p_2sg_6j2x :: DCross 
p_2sg_6j2x = x (t proton, t proton, [p_gluino, p_gluino])


idx_2sg_6j2x :: CrossID ProcSmplIdx
idx_2sg_6j2x = mkCrossIDIdx (mkDICross p_2sg_6j2x)

map_2sg_6j2x :: ProcSpecMap
map_2sg_6j2x = 
    HM.fromList [(Nothing             , MGProc [] [ "p p > go go QED=0" ])
                ,(Just (3,1000021,[]), MGProc [ "define sxx = sxxp sxxp~ "]
                                              [ "go > j j j sxx " ] ) 
                ,(Just (4,1000021,[]), MGProc [ "define sxx = sxxp sxxp~ "] 
                                              [ "go > j j j sxx " ] )
                ] 

modelparam mgl msq msl mneut = ADMXUDD112Param mgl msq msl mneut 

-- | 
mgrunsetup :: Int -> RunSetup
mgrunsetup n = 
  RS { numevent = n
     , machine = LHC7 ATLAS
     , rgrun   = Auto
     , rgscale = 200.0
     , match   = NoMatch
     , cut     = NoCut 
     , pythia  = RunPYTHIA8 
     , lhesanitizer = LHESanitize (Replace [(9000201,1000022),(-9000201,1000022)]) 
     , pgs     = RunPGS (AntiKTJet 0.4,NoTau)
     , uploadhep = NoUploadHEP
     , setnum  = 1
     }

-- need to be scrapped
-- worksets = [ (mgl,msq,50000,50000, 10000) | mgl <- [100,200..2000], msq <- [100,200..2000] ] 

main :: IO () 
main = do 
  args <- getArgs 
  let fp = args !! 0 
      job = args !! 1
  lbstr <- LB.readFile job
  let Just (WorkSet mgl msq) = G.decode' lbstr
      fullwset = (mgl, msq, 50000, 50000, 10000) 
  updateGlobalLogger "MadGraphAuto" (setLevel DEBUG) 
  scanwork fp fullwset 

 

scanwork :: FilePath -> (Double,Double,Double,Double,Int) -> IO () 
scanwork fp (mgl,msq,msl,mneut,n) = do
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
          mgrs = mgrunsetup n
      evchainGen ADMXUDD112
        ssetup 
        ("Work20130527_2sg","2sg_6j2x") 
        param 
        map_2sg_6j2x p_2sg_6j2x 
        mgrs 

      let wsetup' = getWorkSetupCombined ADMXUDD112 ssetup param ("Work20130527_2sg","2sg_6j2x")  mgrs 
          wsetup = wsetup' { ws_storage = WebDAVRemoteDir "montecarlo/admproject/XUDD/scan_2sg_6j2x" } 

      putStrLn "phase2work start"              
      phase2work wsetup
      putStrLn "phase3work start"
      phase3work wdavcfg wsetup 
    )


phase2work :: WorkSetup ADMXUDD112 -> IO ()
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
           runPGS           
           runClean         
         (LHESanitize pid, RunPYTHIA8) -> do 
           sanitizeLHE
           runPYTHIA8
           runPGS           
           runClean         
         (LHESanitize pid, NoPYTHIA) -> do 
           sanitizeLHE
       cleanHepFiles  
    print r  
    return ()

-- | 
phase3work :: WebDAVConfig -> WorkSetup ADMXUDD112 -> IO () 
phase3work wdav wsetup = do 
  uploadEventFull NoUploadHEP wdav wsetup 
  return () 



