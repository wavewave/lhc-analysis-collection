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

mkQLDNeut :: Int -> Int -> Int -> Param QLDNeutLOSP 
mkQLDNeut = QLDNeutLOSPParam

data family Proc a :: * 

data instance Proc QLDNeutLOSP = QLDNeutLOSPProc_2SG | QLDNeutLOSPProc_SQSG | QLDNeutLOSPProc_2SQ

deriving instance Show (Proc QLDNeutLOSP)

class MGluino p where 
  mgluino :: p -> Int 

class MSquark p where 
  msquark :: p -> Int 

class MNeut p where 
  mneut :: p -> Int 


instance MGluino (Param QLDNeutLOSP) where 
  mgluino (QLDNeutLOSPParam x _ _) = x
 
instance MSquark (Param QLDNeutLOSP) where 
  msquark (QLDNeutLOSPParam _ x _) = x
 
instance MNeut (Param QLDNeutLOSP) where 
  mneut   (QLDNeutLOSPParam _ _ x) = x 


instance Show (Param QLDNeutLOSP) where 
  show p = "(gluino=" ++ show (mgluino p) 
           ++ ",squark=" ++ show (msquark p) 
           ++ ",neut=" ++ show (mneut p) ++ ")"


class ProcessName a where
  processName :: Proc a -> String 

instance ProcessName QLDNeutLOSP where 
  processName QLDNeutLOSPProc_2SG  = "2sg_2l8j2x"
  processName QLDNeutLOSPProc_SQSG = "sqsg_2l7j2x"
  processName QLDNeutLOSPProc_2SQ  = "2sq_2l6j2x"

data SQCDProcess = GluinoGluino | GluinoSquark | SquarkSquark

class SQCDProcessable a where 
  sqcdProcess :: Proc a -> SQCDProcess 

instance SQCDProcessable QLDNeutLOSP where 
  sqcdProcess QLDNeutLOSPProc_2SG = GluinoGluino 
  sqcdProcess QLDNeutLOSPProc_SQSG = GluinoSquark
  sqcdProcess QLDNeutLOSPProc_2SQ = SquarkSquark 


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

 
createRdirBNames :: (CreateRdirBName a) => ParamSet a [SetNum] -> [(WebDAVRemoteDir,String)] 
createRdirBNames (ParamSet param procsets) = 
  concatMap (\(ProcSet x ys) -> map (\y->createRdirBName (param,x,y)) ys) procsets




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
  where qparams = [ [ mkQLDNeut g q 100 | q <- [200,300..3000] ] | g <- [200,300..3000]  ]
        procset = [ ProcSet QLDNeutLOSPProc_2SG  [SetNum 1] 
                  , ProcSet QLDNeutLOSPProc_SQSG [SetNum 1]
                  , ProcSet QLDNeutLOSPProc_2SQ  [SetNum 1] ]


main :: IO ()
main = do
  putStrLn "prepare for KFactor map" 
  kfacmap <- mkKFactorMap 
  {- let result paramsets = do ParamSet param procsets <- paramsets 
                            ProcSet proc _ <- procsets
                            let mk = getKFactor kfacmap (param,proc) 
                            return mk
      rs = result param_qldneutlosp_100  -}
  h <- openFile "xqld_neutLOSP100.0_sqsg_8TeV_0lep_NLO.dat" WriteMode
  withDAVConfig "config1.txt" $ do 
    let pass1glu x = do 
          rs <- mapM (processParamSet kfacmap) x 
          liftIO $ mapM_ (hPutStrLn h) (map printFormatter rs)
    mapM_ (\x->pass1glu x >> liftIO (hPutStr h "\n")) param_qldneutlosp_100 

  hClose h 
  return ()


  {-
    r <- prepareFiles kfacmap (head param_qldneutlosp_100)
    let -- r'@(ParamSet p prsets) = fmap (fmap getResultFromPreResult) r 
        -- prsets' = fmap (fmap combineResultSets) prsets 
        r' = getResult r
        r'' = combineMultiSets r' 
        r''' = combineMultiProcess r'' 
        r4 = (getRFromSR . snd) r'''
    liftIO $ print r' 
    liftIO $ print r'' 
    liftIO $ print r''' 
    liftIO $ print r4 
   -}










