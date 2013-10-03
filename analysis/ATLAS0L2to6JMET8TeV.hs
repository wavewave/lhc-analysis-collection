{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}

import Control.Applicative 
import Control.Monad 
import           Control.Monad.Trans (liftIO, lift)
import           Control.Monad.Trans.Either (EitherT(..))
import           Control.Monad.Trans.Maybe 
import           Control.Monad.Trans.Reader 
-- import Data.Attoparsec.Lazy
import qualified Data.Aeson.Generic as G
import qualified Data.ByteString.Lazy.Char8 as LB
import           Data.Data
import           Data.Either (lefts,rights)
import           Data.Foldable (foldrM)
import           Data.Maybe 
import qualified Data.Map as M
import           Data.Monoid ((<>))
import System.Environment
import System.IO
-- 

import HEP.Storage.WebDAV.CURL
import HEP.Storage.WebDAV.Type
-- import HEP.Storage.WebDAV.Util
import HEP.Util.Either 
-- 
import HEP.Physics.Analysis.ATLAS.Common
import HEP.Physics.Analysis.ATLAS.SUSY.SUSY_0L2to6JMET_8TeV
import HEP.Physics.Analysis.Common.XSecNTotNum
import HEP.Physics.Analysis.Common.Prospino
import HEP.Util.Work 
--
import KFactor
import Util
import Debug.Trace

newtype SetNum = SetNum { unSetNum :: Int }
               deriving (Show,Eq,Ord)

-- models 
data QLDNeutLOSP   = QLDNeutLOSP
data QLDSquarkLOSP = QLDSquarkLOSP
data UDDNeutLOSP   = UDDNeutLOSP
data UDDSquarkLOSP = UDDSquarkLOSP
data Sim0  = Sim0
data Sim1q = Sim1q
data Sim1g = Sim1g


data Param a where 
  QLDNeutLOSPParam :: Int -> Int -> Int -> Param QLDNeutLOSP  
  Sim0Param        :: Int -> Int -> Int -> Param Sim0  

mkQLDNeut :: Int -> Int -> Int -> Param QLDNeutLOSP 
mkQLDNeut = QLDNeutLOSPParam

mkSim0 :: Int -> Int -> Int -> Param Sim0 
mkSim0 = Sim0Param 

data family Proc a :: * 

data instance Proc QLDNeutLOSP = QLDNeutLOSPProc_2SG | QLDNeutLOSPProc_SQSG | QLDNeutLOSPProc_2SQ

data instance Proc Sim0 = Sim0Proc_2SG | Sim0Proc_SQSG | Sim0Proc_2SQ

deriving instance Show (Proc QLDNeutLOSP)

deriving instance Show (Proc Sim0)

class MGluino p where 
  mgluino :: p -> Int 

class MSquark p where 
  msquark :: p -> Int 

class MNeut p where 
  mneut :: p -> Int 


instance MGluino (Param QLDNeutLOSP) where 
  mgluino (QLDNeutLOSPParam x _ _) = x
 
instance MGluino (Param Sim0) where
  mgluino (Sim0Param x _ _) = x

instance MSquark (Param QLDNeutLOSP) where 
  msquark (QLDNeutLOSPParam _ x _) = x
 
instance MSquark (Param Sim0) where
  msquark (Sim0Param _ x _) = x

instance MNeut (Param QLDNeutLOSP) where 
  mneut   (QLDNeutLOSPParam _ _ x) = x 

instance MNeut (Param Sim0) where
  mneut   (Sim0Param _ _ x) = x


instance Show (Param QLDNeutLOSP) where 
  show p = "(gluino=" ++ show (mgluino p) 
           ++ ",squark=" ++ show (msquark p) 
           ++ ",neut=" ++ show (mneut p) ++ ")"

instance Show (Param Sim0) where 
  show p = "(gluino=" ++ show (mgluino p) 
           ++ ",squark=" ++ show (msquark p) 
           ++ ",neut=" ++ show (mneut p) ++ ")"


class ProcessName a where
  processName :: Proc a -> String 

instance ProcessName QLDNeutLOSP where 
  processName QLDNeutLOSPProc_2SG  = "2sg_2l8j2x"
  processName QLDNeutLOSPProc_SQSG = "sqsg_2l7j2x"
  processName QLDNeutLOSPProc_2SQ  = "2sq_2l6j2x"

instance ProcessName Sim0 where 
  processName Sim0Proc_2SG  = "2sg_4j2n"
  processName Sim0Proc_SQSG = "sqsg_3j2n"
  processName Sim0Proc_2SQ  = "2sq_2j2n"

data SQCDProcess = GluinoGluino | GluinoSquark | SquarkSquark

class SQCDProcessable a where 
  sqcdProcess :: Proc a -> SQCDProcess 

instance SQCDProcessable QLDNeutLOSP where 
  sqcdProcess QLDNeutLOSPProc_2SG = GluinoGluino 
  sqcdProcess QLDNeutLOSPProc_SQSG = GluinoSquark
  sqcdProcess QLDNeutLOSPProc_2SQ = SquarkSquark 

instance SQCDProcessable Sim0 where 
  sqcdProcess Sim0Proc_2SG = GluinoGluino 
  sqcdProcess Sim0Proc_SQSG = GluinoSquark
  sqcdProcess Sim0Proc_2SQ = SquarkSquark 

-- data Param = P_QLDNeut QLDNeutLOSPParam 

data ProcSet a b = ProcSet { procsetProcess :: Proc a
                           , procsetData    :: b 
                           } 

deriving instance (Show (Param a), Show (Proc a), Show b) => Show (ProcSet a b)
deriving instance Functor (ProcSet a) 

data ParamSet a b = ParamSet (Param a) [ProcSet a b] 

deriving instance (Show (Param a), Show (Proc a), Show b) => Show (ParamSet a b) 
deriving instance Functor (ParamSet a) 

data PreResultSet = PreResult { preresultHist :: HistEType 
                              , preresultXsec :: CrossSectionAndCount 
                              , preresultKFactor :: Double } 
               deriving (Show) 

data ResultSet a = Result { resultXsecNLO :: Double 
                          , resultNumEvent :: Double  
                          , resultNormHist :: a 
                          } 
  deriving (Show) 




class CreateRdirBName a where
  createRdirBName :: (Param a, Proc a, SetNum) -> (WebDAVRemoteDir,String)

instance CreateRdirBName QLDNeutLOSP where 
  -- createRdirBName :: (Param QLDNeutLOSP, Proc QLDNeutLOSP, SetNum) -> (WebDAVRemoteDir,String) 
  createRdirBName (param,proc,SetNum sn) = 
    let procname = processName proc 
        (mg,mq,mn) :: (Double,Double,Double) 
           = ((,,) <$> (fromIntegral.mgluino) <*> (fromIntegral.msquark) <*> (fromIntegral.mneut)) param 
        wdavrdir = WebDAVRemoteDir ("montecarlo/admproject/XQLDdegen/8TeV/neutLOSP/scan_" ++ procname)
        basename = "ADMXQLD111degenMG"++ show mg ++ "MQ" ++ show mq ++ "ML50000.0MN" ++ show mn ++ "_" ++ procname ++ "_LHC8ATLAS_NoMatch_NoCut_AntiKT0.4_NoTau_Set" ++ show sn 
    in (wdavrdir,basename)


instance CreateRdirBName Sim0 where 
  -- createRdirBName :: (Param Sim0, Proc Sim0, SetNum) -> (WebDAVRemoteDir,String) 
  createRdirBName (param,proc,SetNum sn) = 
    let procname = processName proc 
        (mg,mq,mn) :: (Double,Double,Double) 
           = ((,,) <$> (fromIntegral.mgluino) <*> (fromIntegral.msquark) <*> (fromIntegral.mneut)) param 
        wdavrdir = WebDAVRemoteDir ("montecarlo/admproject/SimplifiedSUSY/8TeV/scan_" ++ procname)
        basename = "SimplifiedSUSYMN" ++ show mn ++ "MG"++show mg++ "MSQ" ++ show mq ++ "_" ++ procname ++ "_LHC8ATLAS_NoMatch_NoCut_AntiKT0.4_NoTau_Set" ++ show sn 
    in (wdavrdir,basename)



getKFactor :: (SQCDProcessable a, MGluino (Param a), MSquark (Param a)) => 
              KFactorMap -> (Param a, Proc a) -> Maybe Double
getKFactor kfacmap (param,proc) = 
  let mg = mgluino param 
      mq = msquark param 
      m = case sqcdProcess proc of
            GluinoGluino -> kFactor_2sg kfacmap
            GluinoSquark -> kFactor_sqsg kfacmap 
            SquarkSquark -> kFactor_2sq kfacmap 
  in M.lookup (mg,mq) m


countFor1Set :: (CreateRdirBName a, Show (Param a) ) => 
                     (Param a, Proc a) 
                  -> SetNum  
                  -> EitherT String (ReaderT WebDAVConfig IO) ()
countFor1Set (param,proc) sn = do 
  let (wdavrdir,bname) = createRdirBName (param,proc,sn)
  wdavcfg <- lift ask 
  liftIO (getXSecNCount XSecLHE wdavcfg wdavrdir bname) >>= liftIO . getJSONFileAndUpload wdavcfg wdavrdir bname
  liftIO (atlas_8TeV_0L2to6J_bkgtest ([0],[0]) wdavcfg wdavrdir bname)
  return ()

countParamSet :: (CreateRdirBName a, Show (Param a) ) => 
                 ParamSet a [SetNum] 
              -> EitherT String (ReaderT WebDAVConfig IO) ()
countParamSet (ParamSet param procsets) = do 
  let countProcSet (ProcSet proc ns) = mapM_ (countFor1Set (param,proc)) ns 
  mapM_ countProcSet procsets


-- | 
checkFor1Set :: (CreateRdirBName a, Show (Param a) ) => 
                (Param a, Proc a) 
             -> SetNum  
             -> EitherT String (ReaderT WebDAVConfig IO) ()
checkFor1Set (param,proc) sn = do 
  let (wdavrdir,bname) = createRdirBName (param,proc,sn)
      zerolepfile = bname ++ "_ATLAS8TeV0L2to6JBkgTest.json" 
      totalcountfile = bname ++ "_total_count.json" 
  wdavcfg <- lift ask 
  guardEitherM (show param ++ " not complete") $ 
    (&&) <$> liftIO (doesFileExistInDAV wdavcfg wdavrdir zerolepfile)
         <*> liftIO (doesFileExistInDAV wdavcfg wdavrdir totalcountfile)


checkParamSet :: (CreateRdirBName a, Show (Param a) ) => 
                 ParamSet a [SetNum] 
              -> EitherT String (ReaderT WebDAVConfig IO) ()
checkParamSet (ParamSet param procsets) = do 
  let checkProcSet (ProcSet proc ns) = mapM_ (checkFor1Set (param,proc)) ns 
  mapM_ checkProcSet procsets


-- | 
prepareFilesForSingleSet :: (CreateRdirBName a, MGluino (Param a), MSquark (Param a), SQCDProcessable a
                            , Show (Param a) ) => 
                            KFactorMap 
                            -> (Param a, Proc a) 
                            -> SetNum  
                            -> EitherT String (ReaderT WebDAVConfig IO) PreResultSet
prepareFilesForSingleSet kFacMap (param,proc) sn = do 
  let (wdavrdir,bname) = createRdirBName (param,proc,sn)
      zerolepfile = bname ++ "_ATLAS8TeV0L2to6JBkgTest.json" 
      totalcountfile = bname ++ "_total_count.json" 
  (hist' :: [(JESParam, HistEType)]) <- downloadAndDecodeJSON wdavrdir zerolepfile   
  let hist = (snd.head) hist'
  (xsec :: CrossSectionAndCount) <- downloadAndDecodeJSON wdavrdir totalcountfile 
  (kfactor :: Double) <- (EitherT . return . maybeToEither ("no K Factor for " ++ show param) ) (getKFactor kFacMap (param,proc))
  return (PreResult hist xsec kfactor) 


prepareFiles :: ( CreateRdirBName a, MGluino (Param a), MSquark (Param a), SQCDProcessable a
                , Show (Param a) ) => 
                KFactorMap 
                -> ParamSet a [SetNum] 
                -> EitherT String (ReaderT WebDAVConfig IO) (ParamSet a [PreResultSet])  
prepareFiles kfacmap (ParamSet param procsets) = do 
  let mkResultProcSet (ProcSet proc ns) = do  
        rs <- mapM (prepareFilesForSingleSet kfacmap (param,proc)) ns 
        return (ProcSet proc rs)
  r_procsets <- mapM mkResultProcSet procsets
  return (ParamSet param r_procsets) 

getResultFromPreResult :: PreResultSet -> ResultSet [(EType,Double)]
getResultFromPreResult PreResult {..} =  
  let luminosity = 20300 
      xsecNLO = crossSectionInPb preresultXsec * preresultKFactor 
      nev = (fromIntegral.numberOfEvent) preresultXsec
      weight = xsecNLO * luminosity / nev
      normhist = map (\(x,y) -> (x,fromIntegral y * weight)) preresultHist 
  in Result xsecNLO nev normhist 

combineResultSets :: [ResultSet [(EType,Double)]] -> ResultSet (TotalSR Double) 
combineResultSets rs = 
  let ws = map ((*) <$> resultXsecNLO <*> resultNumEvent) rs 
      sumw = sum ws 
      ws_norm = map (/ sumw) ws  
      apply_weight w Result {..} =
        let hist' = map (\(x,y) -> (x,y*w)) resultNormHist 
        in Result (resultXsecNLO*w) (resultNumEvent*w) hist'
      rs' = zipWith apply_weight ws_norm rs 
  in Result (sum (map resultXsecNLO rs')) 
            (sum (map resultNumEvent rs')) 
            (mkTotalSR (map resultNormHist rs'))

-- | 
getResult :: ParamSet a [PreResultSet] -> ParamSet a [ResultSet [(EType,Double)]]
getResult = fmap (fmap getResultFromPreResult) 

-- | 
combineMultiSets :: ParamSet a [ResultSet [(EType,Double)]] -> ParamSet a (ResultSet (TotalSR Double))
combineMultiSets = fmap combineResultSets

-- | 
combineMultiProcess :: ParamSet a (ResultSet (TotalSR Double)) -> (Param a, TotalSR Double) 
combineMultiProcess (ParamSet param rs) = (param, (sum . map (resultNormHist . procsetData)) rs)


processParamSet :: (CreateRdirBName a, MGluino (Param a), MSquark (Param a), SQCDProcessable a
                   , Show (Param a) ) =>
                   KFactorMap 
                -> ParamSet a [SetNum] 
                -> EitherT String (ReaderT WebDAVConfig IO) (Param a, Double) 
processParamSet kfacmap p = 
  ((,) <$> fst <*> getRFromSR . snd) . combineMultiProcess . combineMultiSets . getResult 
     <$> prepareFiles kfacmap p  


printFormatter :: (MGluino (Param a), MSquark (Param a)) => (Param a, Double) -> String
printFormatter (p,x) = (showdbl . mgluino) p ++ ", " ++ (showdbl . msquark) p ++ ", " ++ show x
  where showdbl = show . fromIntegral



----------
-- TEST -- 
----------

param_qldneutlosp_100 :: [ [ParamSet QLDNeutLOSP [SetNum]] ] 
param_qldneutlosp_100 = map (map (\x->ParamSet x procset)) qparams
  where qparams = [ [ mkQLDNeut g q 100 | q <- [500,600..3000] ] | g <- [500,600..3000]  ]
        procset = [ ProcSet QLDNeutLOSPProc_2SG  [SetNum 1] 
                  , ProcSet QLDNeutLOSPProc_SQSG [SetNum 1]
                  , ProcSet QLDNeutLOSPProc_2SQ  [SetNum 1] ]


n_param_qldneutlosp_100 :: [ [ParamSet QLDNeutLOSP [SetNum]] ]
n_param_qldneutlosp_100 = [ [ ParamSet (mkQLDNeut g q 100) y | q <- [500,600..3000], let y = if q <= 1000 then procset12 else procset1 ] | g <- [500,600..3000]  ]
  where procset1  = [ ProcSet QLDNeutLOSPProc_2SG  [SetNum 1] 
                    , ProcSet QLDNeutLOSPProc_SQSG [SetNum 1]
                    , ProcSet QLDNeutLOSPProc_2SQ  [SetNum 1] ]
        procset12 = [ ProcSet QLDNeutLOSPProc_2SG  [SetNum 1, SetNum 2, SetNum 3] 
                    , ProcSet QLDNeutLOSPProc_SQSG [SetNum 1, SetNum 2, SetNum 3]
                    , ProcSet QLDNeutLOSPProc_2SQ  [SetNum 1, SetNum 2, SetNum 3] ]


param_sim0_100 :: [ [ParamSet Sim0 [SetNum]] ] 
param_sim0_100 = map (map (\x->ParamSet x procset)) qparams
  where qparams = [ [ mkSim0 g q 100 | q <- [500,600..3000] ] | g <- [500,600..3000]  ]
        procset = [ ProcSet Sim0Proc_2SG  [SetNum 1] 
                  , ProcSet Sim0Proc_SQSG [SetNum 1]
                  , ProcSet Sim0Proc_2SQ  [SetNum 1] ]


param_sim0_10 :: [ [ParamSet Sim0 [SetNum]] ] 
param_sim0_10 = map (map (\x->ParamSet x procset)) qparams
  where qparams = [ [ mkSim0 g q 10 | q <- [200,300..3000] ] | g <- [200,300..3000]  ]
        procset = [ ProcSet Sim0Proc_2SG  [SetNum 1] 
                  , ProcSet Sim0Proc_SQSG [SetNum 1]
                  , ProcSet Sim0Proc_2SQ  [SetNum 1] ]

---------------
-- NEW COUNT -- 
---------------

nc_param_qldneutlosp_100 :: [ParamSet QLDNeutLOSP [SetNum]]  
nc_param_qldneutlosp_100 = map (\x->ParamSet x procset) qparams
  where qparams = [ mkQLDNeut g q 100 | g <- [500,600..3000], q <- [500,600..1000] ]
        procset = [ ProcSet QLDNeutLOSPProc_2SG  [SetNum 2] 
                  , ProcSet QLDNeutLOSPProc_SQSG [SetNum 2]
                  , ProcSet QLDNeutLOSPProc_2SQ  [SetNum 2] ]

nc3_param_qldneutlosp_100 :: [ParamSet QLDNeutLOSP [SetNum]]  
nc3_param_qldneutlosp_100 = map (\x->ParamSet x procset) qparams
  where qparams = [ mkQLDNeut g q 100 | g <- [500,600..3000] , q <- [500,600..1000] ]
        procset = [ ProcSet QLDNeutLOSPProc_2SG  [SetNum 3] 
                  , ProcSet QLDNeutLOSPProc_SQSG [SetNum 3]
                  , ProcSet QLDNeutLOSPProc_2SQ  [SetNum 3] ]



main' :: IO ()
main' = do
  putStrLn "prepare for KFactor map" 
  kfacmap <- mkKFactorMap 
  h <- openFile "xqld_neutLOSP100_sqsg_8TeV_0lep_NLO.dat" WriteMode
        -- openFile "sim0_neut10_sqsg_8TeV_0lep_NLO.dat" WriteMode
  emsg <- withDAVConfig "config1.txt" $ do 
            let pass1glu x = do 
                  rs <- mapM (processParamSet kfacmap) x 
                  liftIO $ mapM_ (hPutStrLn h) (map printFormatter rs)
            mapM_ (\x->pass1glu x >> liftIO (hPutStr h "\n")) n_param_qldneutlosp_100  -- param_sim0_10
  print emsg 
  hClose h 
  return ()

main'' :: IO ()
main'' = do 
  emsg <- withDAVConfig "config1.txt" $ do 
            mapM_ countParamSet nc3_param_qldneutlosp_100
  print emsg 
  
main :: IO ()
main = do 
  emsg <- mapM (\x->withDAVConfig "config1.txt" (checkParamSet x)) nc3_param_qldneutlosp_100
  mapM_ print (lefts emsg)
  






