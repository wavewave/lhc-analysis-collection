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
import HEP.Physics.Analysis.Common.Prospino
import HEP.Util.Work 
--
import Util
import Debug.Trace

mneut :: Double
mneut = 500.0 


datalst :: [ (Double,Double) ]
datalst = -- [ (2800,1600) ] 
          [ (g,q) | {- g <- [2800,2900,3000], -}  g <- [mneut+100,mneut+200..3000],  q <- [mneut+100,mneut+200..3000] ]

-- [ (g,q) | g <- [200,300..3000], q <- [g,g+100..3000] ]

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

 
-- | in pb^-1 unit
luminosity = 20300


createRdirBName procname (mg,mq) = 
  let rdir = "montecarlo/admproject/XQLDdegen/8TeV/neutLOSP/scan_" ++ procname 
      basename = "ADMXQLD111degenMG" ++ show mg ++ "MQ" ++ show mq ++ "ML" ++ show minfty ++ "MN"++show mneut ++ "_" ++ procname ++ "_LHC8ATLAS_NoMatch_NoCut_AntiKT0.4_NoTau_Set"
  in (rdir,basename)  


dirset = [ "2sg_2l8j2x"
         , "sqsg_2l7j2x"
         , "2sq_2l6j2x" 
         ]


atlas_20_3_fbinv_at_8_TeV :: WebDAVConfig -> WebDAVRemoteDir -> String 
                          -> EitherT String IO (CrossSectionAndCount,HistEType,[(EType,Double)],Double)
atlas_20_3_fbinv_at_8_TeV wdavcfg wdavrdir bname = do 
  let fp1 = bname ++ "_ATLAS_1to2L2to6JMET_8TeV.json"
      fp2 = bname ++ "_total_count.json" 
      kfactorfp = bname ++ "_xsecKfactor.json"
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
  {-
  guardEitherM (kfactorfp ++ " not exist") (doesFileExistInDAV wdavcfg wdavrdir kfactorfp)
  (_,mrk) <- liftIO (downloadFile True wdavcfg wdavrdir kfactorfp)
  rk <- (liftM LB.pack . EitherT . return . maybeToEither (kfactorfp ++ " is not downloaded ")) mrk 
  liftIO$ print rk 
  (result_kfactor :: CrossSectionResult) 
    <- (EitherT . return . maybeToEither (kfactorfp ++ " JSON cannot be decoded") .G.decode) rk
  -}
  -- 
  let kFactor = 1.0 -- xsecKFactor result_kfactor
      weight = crossSectionInPb xsec * luminosity * kFactor / fromIntegral (numberOfEvent xsec)
      hist = map (\(x,y) -> (x,fromIntegral y * weight)) result 
      getratio (x,y) = do y' <- lookup x limitOfNBSM 
                          return (y/ y') 
      maxf (x,y) acc = do r <- getratio (x,y)
                          return (max acc r)
  -- 
  liftIO $ putStrLn $ "kfactor = " ++ show kFactor
  maxratio <- (EitherT . return . maybeToEither "in atlas_xx:foldrM" . foldrM maxf 0) hist 
  return (xsec, result, hist, maxratio) 




getResult f (rdir,basename) = do 
  let nlst = [1]
  fileWork f "config1.txt" rdir basename nlst 




mainAnalysis = do
  outh <- openFile ("xqld_neutLOSP"++show mneut++"_sqsg_8TeV_1lep.dat") WriteMode 
  mapM_ (\(mg,mn,r) -> hPutStrLn outh (show mg ++ ", " ++ show mn ++ ", " ++ show r))
    =<< forM datalst ( \(x,y) -> do
          r <- runEitherT $ do
            let analysis p = getResult atlas_20_3_fbinv_at_8_TeV . createRdirBName p
                simplify = fmap head
                takeHist (_,_,h,_) = h

            t_2sg    <- (simplify . analysis "2sg_2l8j2x")    (x,y)
            t_sqsg   <- (simplify . analysis "sqsg_2l7j2x") (x,y)
            t_2sq    <- (simplify . analysis "2sq_2l6j2x") (x,y)

            let h_2sg  = takeHist t_2sg
                h_sqsg = takeHist t_sqsg
                h_2sq  = takeHist t_2sq

                totalsr = mkTotalSR [h_2sg, h_sqsg, h_2sq]
                r_ratio = getRFromSR totalsr
            return (x :: Double, y :: Double, r_ratio)
          case r of 
            Left err -> error err 
            Right result -> return result 
          )
  hClose outh 



mainCheck = do 
  r <- runEitherT $ mapM_ (EitherT . checkFiles  {- ChanCount -}  Prospino ) dirset
  print r 


mainCount = do 
  r <- runEitherT (mapM_ countEvent . drop 2 . take 3 $ dirset)
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


