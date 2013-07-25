{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}

import Control.Monad 
import           Control.Monad.Trans (liftIO)
import           Control.Monad.Trans.Either (EitherT(..),left,right)
import           Control.Monad.Trans.Maybe 
-- import Data.Attoparsec.Lazy
import qualified Data.Aeson.Generic as G
import qualified Data.ByteString.Lazy.Char8 as LB
import           Data.Foldable (foldrM)
import           Data.Maybe 
import System.Environment
import System.IO
-- 

import HEP.Storage.WebDAV.CURL
import HEP.Storage.WebDAV.Type
-- import HEP.Storage.WebDAV.Util
import HEP.Util.Either 
-- 
import HEP.Physics.Analysis.ATLAS.Common
import HEP.Physics.Analysis.ATLAS.SUSY.SUSY_1to2L2to6JMET_8TeV
import HEP.Physics.Analysis.Common.XSecNTotNum
import HEP.Util.Work 
--
import Util
import Debug.Trace

m_neutralino :: Double 
m_neutralino = 500

datalst :: [ (Double,Double) ]
datalst = [ (mq,mn) | mq <- [ 200,250..1300], mn <- [ 50,100..mq-50 ] ] 
-- datalst = [ (1300,300) ]

{-
datalst1of2 :: [ (Double,Double) ]
datalst1of2 = [ (g,q) | g <- [m_neutralino+100,m_neutralino+200..1500], q <- [m_neutralino+100,m_neutralino+200..3000] ]

datalst2of2 :: [ (Double,Double) ]
datalst2of2 = [ (g,q) | g <- [1600,1700..3000], q <- [m_neutralino+100,m_neutralino+200..3000] ]
-}


checkFiles :: DataFileClass -> String -> IO (Either String ())
checkFiles c procname = do 
  rs <- forM datalst (\s -> (doJob (checkFileExistInDAV_lep c)  . createRdirBName procname) s 
                                >>= return . maybe (show s) (const []) . head)
  let missinglst = filter (not.null) rs
      nmiss = length missinglst
  mapM_ (\x -> putStrLn ("  , " ++ x)) missinglst
  if null missinglst then return (Right ()) else return (Left (show nmiss ++ " files are missing"))

minfty :: Double
minfty = 50000.0

createRdirBName procname (mq,mn) = 
  let rdir = "montecarlo/admproject/SimplifiedSUSYlep/8TeV/scan_" ++ procname 
      basename = "SimplifiedSUSYlepN" ++ show mn ++ "G"++show minfty++ "QL" ++ show mq ++ "C"++show (0.5*(mn+mq))++ "L" ++ show minfty ++ "NN" ++ show minfty ++ "_" ++ procname ++ "_LHC8ATLAS_NoMatch_NoCut_AntiKT0.4_NoTau_Set"
  in (rdir,basename)  


dirset = [ "1step_2sq" ] 



atlas_20_3_fbinv_at_8_TeV :: WebDAVConfig -> WebDAVRemoteDir -> String 
                          -> EitherT String IO (CrossSectionAndCount,HistEType,[(EType,Double)],Double)
atlas_20_3_fbinv_at_8_TeV wdavcfg wdavrdir bname = do 
  let fp1 = bname ++ "_ATLAS_1to2L2to6JMET_8TeV.json"
      fp2 = bname ++ "_total_count.json" 
  -- 
  guardEitherM (fp1 ++ " not exist!") (doesFileExistInDAV wdavcfg wdavrdir fp1)
  (_,mr1) <- liftIO (downloadFile True wdavcfg wdavrdir fp1)
  r1 <- (liftM LB.pack . EitherT . return . maybeToEither (fp1 ++ " is not downloaded ")) mr1 
  (result :: HistEType) <- (EitherT . return . maybeToEither (fp1 ++ " JSON cannot be decoded") . G.decode) r1 
  -- 
  guardEitherM (fp2 ++ " not exist") (doesFileExistInDAV wdavcfg wdavrdir fp2)
  (_,mr2) <- liftIO (downloadFile True wdavcfg wdavrdir fp2)
  r2 <- (liftM LB.pack . EitherT . return . maybeToEither (fp2 ++ " is not downloaded ")) mr2 
  (xsec :: CrossSectionAndCount) 
    <- (EitherT . return . maybeToEither (fp2 ++ " JSON cannot be decoded") .G.decode) r2  
  --
  let weight = crossSectionInPb xsec * 20300 / fromIntegral (numberOfEvent xsec)
      hist = map (\(x,y) -> (x,fromIntegral y * weight)) result 
      getratio (x,y) = do y' <- lookup x limitOfNBSM 
                          return (y/ y') 
      maxf (x,y) acc = do r <- getratio (x,y)
                          return (max acc r)
  -- 
  maxratio <- (EitherT . return . maybeToEither "in atlas_xx:foldrM" . foldrM maxf 0) hist 
  return (xsec, result, hist, maxratio) 




getResult f (rdir,basename) = do 
  let nlst = [1]
  fileWork f "config1.txt" rdir basename nlst 




mainAnalysis = do
  outh <- openFile ("simplifiedsusylep_1step_2sq_8TeV.dat") WriteMode 
  mapM_ (\(mg,msq,r) -> hPutStrLn outh (show mg ++ ", " ++ show msq ++ ", " ++ show r))
    =<< forM datalst ( \(x,y) -> do
          r <- runEitherT $ do
            let analysis = getResult atlas_20_3_fbinv_at_8_TeV . createRdirBName "1step_2sq"
                -- simplify = fmap head . fmap catMaybes . EitherT
                takeHist (_,_,h,_) = h
            t_2sq  <- (fmap head . analysis) (x,y)
            let h_2sq  = takeHist t_2sq
                totalsr = mkTotalSR [h_2sq]
                r_ratio = getRFromSR totalsr
            return (x :: Double, y :: Double, r_ratio)
          case r of 
            Left err -> error err 
            Right result -> return result 
          )
  hClose outh 

{-
          case r of 
            Left err -> error err 
            Right result -> return result 
       )
-}


mainCheck = do 
  r <- runEitherT $ mapM_ (EitherT . checkFiles ChanCount) dirset
  print r 


mainCount = do 
  r <- runEitherT (countEvent "1step_2sq")
  case r of 
    Left err -> putStrLn err
    Right _ -> return ()

main = do 
  args <- getArgs
  case args !! 0 of 
    "count" -> mainCount 
    "check" -> mainCheck
    "analysis" -> mainAnalysis
 
countEvent :: String -> EitherT String IO ()
countEvent str = do 
  EitherT (checkFiles RawData str)

  liftIO $ forM_ datalst (getCount.createRdirBName str)

      

getCount (rdir,basename) = do 
  let nlst = [1]
  r1 <- work (\wdavcfg wdavrdir nm -> getXSecNCount XSecLHE wdavcfg wdavrdir nm >>= getJSONFileAndUpload wdavcfg wdavrdir nm)
         "config1.txt" 
         rdir 
         basename 
         nlst 
  print r1

  r2 <- work atlas_SUSY_1to2L2to6JMET_8TeV "config1.txt" rdir basename nlst
  print r2 

