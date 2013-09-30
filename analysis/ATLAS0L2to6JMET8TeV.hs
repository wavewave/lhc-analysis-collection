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

data ProcSet a b = ProcSet (Proc a) [b] 

deriving instance (Show (Param a), Show (Proc a), Show b) => Show (ProcSet a b)
deriving instance Functor (ProcSet a) 

data ParamSet a b = ParamSet (Param a) [ProcSet a b] 

deriving instance (Show (Param a), Show (Proc a), Show b) => Show (ParamSet a b) 
deriving instance Functor (ParamSet a) 

data PreResultSet = PreResult { preresultHist :: HistEType 
                              , preresultXsec :: CrossSectionAndCount 
                              , preresultKFactor :: Double } 
               deriving (Show) 

data ResultSet = Result { resultXsecNLO :: Double 
                        , resultNumEvent :: Int 
                        , resultNormHist :: [(EType,Double)]
                        , resultPreresult :: PreResultSet 
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

 
createRdirBNames :: (CreateRdirBName a) => ParamSet a SetNum -> [(WebDAVRemoteDir,String)] 
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
  -- 
  (hist' :: [(JESParam, HistEType)]) <- downloadAndDecodeJSON wdavrdir zerolepfile   
  let hist = (snd.head) hist'
  (xsec :: CrossSectionAndCount) <- downloadAndDecodeJSON wdavrdir totalcountfile 
  (kfactor :: Double) <- (EitherT . return . maybeToEither ("no K Factor for " ++ show param) ) (getKFactor kFacMap (param,proc))
  return (PreResult hist xsec kfactor) 





prepareFiles :: ( CreateRdirBName a, MGluino (Param a), MSquark (Param a), SQCDProcessable a
                , Show (Param a) ) => 
                KFactorMap 
                -> ParamSet a SetNum 
                -> EitherT String (ReaderT WebDAVConfig IO) (ParamSet a PreResultSet)  
prepareFiles kfacmap (ParamSet param procsets) = do 
  let mkResultProcSet (ProcSet proc ns) = do  
        rs <- mapM (prepareFilesForSingleSet kfacmap (param,proc)) ns 
        return (ProcSet proc rs)
  r_procsets <- mapM mkResultProcSet procsets
  return (ParamSet param r_procsets) 

getResultFromPreResult :: PreResultSet -> ResultSet 
getResultFromPreResult pre@(PreResult {..}) =  
  let luminosity = 20300 
      xsecNLO = crossSectionInPb preresultXsec * preresultKFactor 
      nev = numberOfEvent preresultXsec
      weight = xsecNLO * luminosity / fromIntegral nev
      normhist = map (\(x,y) -> (x,fromIntegral y * weight)) preresultHist 
  in Result xsecNLO nev normhist pre

combineResultSets :: [ResultSet] -> ResultSet 
combineResultSets = undefined 


{- 


      getratio (x,y) = do y' <- lookup x limitOfNBSM 
                          return (y/ y') 
      maxf (x,y) acc = do r <- getratio (x,y)
                          return (max acc r)
  maxratio <- (EitherT . return . maybeToEither "in atlas_xx:foldrM" . foldrM maxf 0) hist 

  return (Result preresultXsec  result, nromhist, maxratio) 
-}




----------
-- TEST -- 
----------

param_qldneutlosp_100 :: [ParamSet QLDNeutLOSP SetNum] 
param_qldneutlosp_100 = map (\x->ParamSet x procset) qparams
  where qparams = [ mkQLDNeut g q 100  
                    | g <- [200,300..3000], q <- [200,300..3000] ]
        procset = [ ProcSet QLDNeutLOSPProc_2SG  [SetNum 1] 
                  , ProcSet QLDNeutLOSPProc_SQSG [SetNum 1]
                  , ProcSet QLDNeutLOSPProc_2SQ  [SetNum 1] ]


 
main = do
  putStrLn "prepare for KFactor map" 
  kfacmap <- mkKFactorMap 
  let result paramsets = do ParamSet param procsets <- paramsets 
                            ProcSet proc _ <- procsets
                            let mk = getKFactor kfacmap (param,proc) 
                            return mk
      rs = result param_qldneutlosp_100 
  withDAVConfig "config1.txt" $ do 
    r <- prepareFiles kfacmap (head param_qldneutlosp_100)
    let r' = (fmap getResultFromPreResult) r 
    liftIO $ print r' 


{- 
  \( ) kfacmap param proc   



  let lst = concatMap createRdirBNames param_qldneutlosp_100 
  mapM_ print lst 
-}


-- String -> (Double, Double) -> (String,String)
{-
procname_qld :: QLDNeutLOSPProc -> String 
procname_qld QLDNeutLOSPProc_2SG  = "2sg_2l8j2x"
procname_qld QLDNeutLOSPProc_SQSG = "sqsg_2l7j2x"
procname_qld QLDNeutLOSPProc_2SQ  = "2sq_2l6j2x"
-}







{- 
  let rdir = "montecarlo/admproject/XQLDdegen/8TeV/neutLOSP/scan_" ++ procname 
      basename = "ADMXQLD111degenMG"++ show mg++ "MQ" ++ show mq ++ "ML50000.0MN" ++ show m_neutralino ++ "_" ++ procname ++ "_LHC8ATLAS_NoMatch_NoCut_AntiKT0.4_NoTau_Set" 
  in (rdir,basename)  

dirset = [ "2sg_2l8j2x"
         , "sqsg_2l7j2x"
         , "2sq_2l6j2x"
         ]
-}


{- 
checkFiles :: DataFileClass -> String -> IO (Either String ())
checkFiles c procname = do 
  rs <- forM datalst (\((x,y),ns) -> doJob ns (checkFileExistInDAV c) (createRdirBName procname (x,y))
                                     >>= return . maybe (show ((x,y),ns)) (const []) . head)
  let missinglst = filter (not.null) rs
      nmiss = length missinglst
  mapM_ (\x -> putStrLn ("  , " ++ x)) missinglst
  if null missinglst then return (Right ()) else return (Left (show nmiss ++ " files are missing"))





atlas_20_3_fbinv_at_8_TeV :: String 
                          -> WebDAVConfig 
                          -> WebDAVRemoteDir 
                          -> String 
                          -> EitherT String IO (CrossSectionAndCount,[(JESParam,HistEType)],[(EType,Double)],Double)
atlas_20_3_fbinv_at_8_TeV proc wdavcfg wdavrdir bname = do 
  let fp1 = bname ++ "_ATLAS8TeV0L2to6JBkgTest.json"
      fp2 = bname ++ "_total_count.json" 
      kfactorfp = bname ++ "_xsecKfactor.json"
  -- 
  guardEitherM (fp1 ++ "not exist!") (doesFileExistInDAV wdavcfg wdavrdir fp1)
  (_,mr1) <- liftIO (downloadFile True wdavcfg wdavrdir fp1)
  r1 <- (liftM LB.pack . EitherT . return . maybeToEither (fp1 ++ " is not downloaded ")) mr1 
  (result :: [(JESParam, HistEType)]) <- (EitherT . return . maybeToEither (fp1 ++ " JSON cannot be decoded") . G.decode) r1 
  -- 
  guardEitherM (fp2 ++ " not exist!") (doesFileExistInDAV wdavcfg wdavrdir fp2)
  (_,mr2) <- liftIO (downloadFile True wdavcfg wdavrdir fp2)
  r2 <- (liftM LB.pack . EitherT . return . maybeToEither (fp2 ++ " is not downloaded ")) mr2 
  (xsec :: CrossSectionAndCount) <- (EitherT . return . maybeToEither (fp2 ++ " JSON cannot be decoded") . G.decode) r2  
  --
  case proc of

  {- 
  guardEitherM (kfactorfp ++ " not exist") (doesFileExistInDAV wdavcfg wdavrdir kfactorfp)
  (_,mrk) <- liftIO (downloadFile True wdavcfg wdavrdir kfactorfp)
  rk <- (liftM LB.pack . EitherT . return . maybeToEither (kfactorfp ++ " is not downloaded ")) mrk 
  liftIO$ print rk 
  (result_kfactor :: CrossSectionResult) 
    <- (EitherT . return . maybeToEither (kfactorfp ++ " JSON cannot be decoded") .G.decode) rk 
  -}
  -- 
  let -- kFactor = xsecKFactor result_kfactor
      kFactor = 1 
      weight = crossSectionInPb xsec * luminosity * kFactor  / fromIntegral (numberOfEvent xsec)
      hist = map (\(x,y) -> (x,fromIntegral y * weight)) ((snd . head) result )

  let getratio (x,y) = do y' <- lookup x limitOfNBSM 
                          return (y/ y') 
      maxf (x,y) acc = do r <- getratio (x,y)
                          return (max acc r)
  maxratio <- (EitherT . return . maybeToEither "in atlas_xx:foldrM" . foldrM maxf 0) hist 

  return (xsec, result, hist, maxratio) 



getResult f ns (rdir,basename) = fileWork f "config1.txt" rdir basename ns  


  -- let nlst = [1]


mainAnalysis = do
  -- outh <- openFile ("xqld_neutLOSP" ++ show m_neutralino ++ "_sqsg_8TeV_0lep.dat") WriteMode 
  let outh = stdout 
  mapM_ (\(mg,msq,r) -> hPutStrLn outh (show mg ++ ", " ++ show msq ++ ", " ++ show r))
    =<< forM datalst ( \((x,y),ns) -> do
          r <- runEitherT $ do
            let analysis x = getResult (atlas_20_3_fbinv_at_8_TeV x) ns . createRdirBName x
                -- simplify = fmap head . fmap catMaybes . EitherT
                takeHist (_,_,h,_) = h
            t_2sg  <- analysis "2sg_2l8j2x"  (x,y)
            t_sqsg <- analysis "sqsg_2l7j2x" (x,y)
            t_2sq  <- analysis "2sq_2l6j2x" (x,y)
            {-
            let h_2sg  = map takeHist t_2sg
                h_sqsg = map takeHist t_sqsg
                h_2sq  = map takeHist t_2sq
                totalsr = mkTotalSR [ h_2sg, h_sqsg, h_2sq ]
                r_ratio = getRFromSR totalsr 
                 
            trace (show (x,y)) $ return (x :: Double, y :: Double, r_ratio) -}
            return (x :: Double, y :: Double, t_2sg)
          case r of 
            Left err -> error err 
            Right result -> return result
      )
  hClose outh 


mainCheck = do 
  r <- runEitherT $ mapM_ (EitherT . checkFiles {- ChanCount -} Prospino ) $ take 1 dirset 
  print r


mainCount str = do 
  -- let str = "2sq_nn_2l2j2x" 
  r <- runEitherT (countEvent str) 
  case r of 
    Left err -> putStrLn err
    Right _ -> return ()


main = do 
  args <- getArgs
  case args !! 0 of 
    "count" -> case args !! 1 of
                 "2sg" -> mainCount "2sg_2l8j2x"
                 "sqsg" -> mainCount "sqsg_2l7j2x" 
                 "2sq" -> mainCount "2sq_2l6j2x" 
    "check" -> mainCheck
    "analysis" -> mainAnalysis


countEvent :: String -> EitherT String IO ()
countEvent str = do 
  EitherT (checkFiles RawData str)

  liftIO $ putStrLn "Proceed 1 or 2 ? (1/2/others)"
  c <- liftIO $ getChar
  if c == '1' 
    then liftIO $ forM_ datalst1of2 (\((x,y),ns) -> getCount ns (createRdirBName str (x,y)))
    else if c == '2' 
           then liftIO $ forM_ datalst2of2 (\((x,y),ns) -> getCount ns (createRdirBName str (x,y)))
           else return ()


    

getCount ns (rdir,basename) = do 
  r1 <- work (\wdavcfg wdavrdir nm -> getXSecNCount XSecLHE wdavcfg wdavrdir nm >>= getJSONFileAndUpload wdavcfg wdavrdir nm)
         "config1.txt" 
         rdir 
         basename 
         ns 
  print r1

  r2 <- work 
         (atlas_8TeV_0L2to6J_bkgtest ([0],[0]))
         "config1.txt"
         rdir
         basename
         ns
  print r2 

-}
