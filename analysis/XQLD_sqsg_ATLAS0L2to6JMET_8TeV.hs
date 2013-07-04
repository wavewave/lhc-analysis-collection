{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}

import Control.Monad 
import           Control.Monad.Trans (liftIO)
import           Control.Monad.Trans.Either (EitherT(..))
import           Control.Monad.Trans.Maybe 
-- import Data.Attoparsec.Lazy
import qualified Data.Aeson.Generic as G
import qualified Data.ByteString.Lazy.Char8 as LB
import           Data.Foldable (foldrM)
import           Data.Maybe 
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
import HEP.Util.Work 
--
import Debug.Trace

masslst = [ "100.0"
          , "200.0"
          , "300.0"
          , "400.0"
          , "500.0" 
          , "600.0"
          , "700.0"
          , "800.0"
          , "900.0"
          , "1000.0"
          , "1100.0"
          , "1200.0"
          , "1300.0"
          , "1400.0"
          , "1500.0"
          , "1600.0" 
          , "1700.0"
          , "1800.0"
          , "1900.0"
          , "2000.0" ]

datalst = [ (x,y) | x <- masslst, y <- masslst ] 


takeR [Just (_,_,_,r)] = r 

takeHist [Just (_,_,h,_)] = h

takeResult [Just (_,r,_,_)] = r

data DataFileClass = RawData | TotalCount | ChanCount 

checkFileExistInDAV :: DataFileClass -> WebDAVConfig -> WebDAVRemoteDir -> String -> IO (Maybe (Maybe ()))
checkFileExistInDAV datcls wdavcfg wdavrdir bname = do 
  let fp1 = bname ++ "_ATLAS8TeV0L2to6JBkgTest.json"
      fp2 = bname ++ "_total_count.json" 
      fp3 = bname ++ "_pgs_events.lhco.gz"
      fp = case datcls of 
             RawData -> fp3
             TotalCount -> fp2
             ChanCount -> fp1 
  b <- doesFileExistInDAV wdavcfg wdavrdir fp 
  if b then return (Just (Just ()))  else return Nothing 


doJob wk (rdir,basename) = do
  let nlst = [1]
  Right r1 <- work wk "config1.txt" rdir basename nlst 
  return r1 


checkFiles :: DataFileClass -> String -> IO (Either String ())
checkFiles c procname = do 
  rs <- forM datalst (\s -> (doJob (checkFileExistInDAV c)  . createRdirBName procname) s 
                                >>= return . maybe (show s) (const []) . head)
  let missinglst = filter (not.null) rs
      nmiss = length missinglst
  mapM_ (\x -> putStrLn ("  , " ++ x)) missinglst
  if null missinglst then return (Right ()) else return (Left (show nmiss ++ " files are missing"))

createRdirBName procname (mg,mq) = 
  let rdir = "montecarlo/admproject/XQLDdegen/8TeV/scan_" ++ procname 
      basename = "ADMXQLD111degenMG"++mg++ "MQ" ++ mq ++ "ML50000.0MN50000.0_" ++ procname ++ "_LHC8ATLAS_NoMatch_NoCut_AntiKT0.4_NoTau_Set"
  in (rdir,basename)  

dirset = [ "2sg_2l4j2x"
         , "sqsg_o_2l3j2x"
         , "2sq_no_2l2j2x"
         , "2sq_oo_2l2j2x"
         , "2sq_nn_2l2j2x"
         , "sqsg_n_2l3j2x" ]


mainCount :: String -> EitherT String IO ()
mainCount str = do 
  EitherT (checkFiles RawData str)
  liftIO $ forM_ datalst (getCount.createRdirBName str)

--  r <- runEitherT $ mapM_ (EitherT . checkFiles RawData) dirset 
--  print r
   

fetchXSecNHist :: WebDAVConfig -> WebDAVRemoteDir -> String 
               -> IO (Maybe (CrossSectionAndCount,[(JESParam,HistEType)]))
fetchXSecNHist wdavcfg wdavrdir bname = do 
  let fp1 = bname ++ "_ATLAS8TeV0L2to6JBkgTest.json"
      fp2 = bname ++ "_total_count.json" 
  runMaybeT $ do  
    (_,mr1) <- MaybeT . boolToMaybeM (doesFileExistInDAV wdavcfg wdavrdir fp1) 
                      . downloadFile True wdavcfg wdavrdir $ fp1 
    r1 <- liftM LB.pack (MaybeT . return $ mr1) 
    (result :: [(JESParam, HistEType)]) <- MaybeT . return $ G.decode r1 
   
    (_,mr2) <- MaybeT . boolToMaybeM (doesFileExistInDAV wdavcfg wdavrdir fp2) 
                      . downloadFile True wdavcfg wdavrdir $ fp2
    r2 <- liftM LB.pack (MaybeT . return $ mr2) 
    (xsec :: CrossSectionAndCount) <- MaybeT . return $ G.decode  r2  
    return (xsec,result)

atlas_20_3_fbinv_at_8_TeV :: WebDAVConfig -> WebDAVRemoteDir -> String 
                          -> IO (Maybe (CrossSectionAndCount,[(JESParam,HistEType)],[(EType,Double)],Double))
atlas_20_3_fbinv_at_8_TeV wdavcfg wdavrdir bname = do 
  let fp1 = bname ++ "_ATLAS8TeV0L2to6JBkgTest.json"
      fp2 = bname ++ "_total_count.json" 
  runMaybeT $ do  
    (_,mr1) <- MaybeT . boolToMaybeM (doesFileExistInDAV wdavcfg wdavrdir fp1) 
                      . downloadFile True wdavcfg wdavrdir $ fp1 
    r1 <- liftM LB.pack (MaybeT . return $ mr1) 
    (result :: [(JESParam, HistEType)]) <- MaybeT . return $ G.decode r1 
   
    (_,mr2) <- MaybeT . boolToMaybeM (doesFileExistInDAV wdavcfg wdavrdir fp2) 
                      . downloadFile True wdavcfg wdavrdir $ fp2
    r2 <- liftM LB.pack (MaybeT . return $ mr2) 
    (xsec :: CrossSectionAndCount) <- MaybeT . return $ G.decode  r2  

    let weight = crossSectionInPb xsec * 20300 / fromIntegral (numberOfEvent xsec)
        hist = map (\(x,y) -> (x,fromIntegral y * weight)) ((snd . head) result )

    let getratio (x,y) = do y' <- lookup x limitOfNBSM 
                            return (y/ y') 
        maxf (x,y) acc = do r <- getratio (x,y)
                            return (max acc r)
    maxratio <- MaybeT . return $ foldrM maxf 0 hist 

    return (xsec, result, hist, maxratio) 



getResult f (rdir,basename) = do 
  let nlst = [1]
  work f "config1.txt" rdir basename nlst 




main = do
  outh <- openFile "xqld_sqsg_8TeV_0lep.dat" WriteMode 
  mapM_ (\(mg,msq,r) -> hPutStrLn outh (show mg ++ ", " ++ show msq ++ ", " ++ show r))
    =<< forM datalst ( \(x,y) -> do
          r <- runEitherT $ do
            let analysis x = getResult atlas_20_3_fbinv_at_8_TeV . createRdirBName x
                simplify = fmap head . fmap catMaybes . EitherT
                takeHist (_,_,h,_) = h
            t_2sg    <- (simplify . analysis "2sg_2l4j2x")    (x,y)
            t_sqsg_o <- (simplify . analysis "sqsg_o_2l3j2x") (x,y)
            t_sqsg_n <- (simplify . analysis "sqsg_n_2l3j2x") (x,y)
            t_2sq_oo <- (simplify . analysis "2sq_oo_2l2j2x") (x,y)
            t_2sq_no <- (simplify . analysis "2sq_no_2l2j2x") (x,y)
            t_2sq_nn <- (simplify . analysis "2sq_nn_2l2j2x") (x,y)

            let h_2sg    = takeHist t_2sg
                h_sqsg_o = takeHist t_sqsg_o
                h_sqsg_n = takeHist t_sqsg_n
                h_2sq_oo = takeHist t_2sq_oo
                h_2sq_no = takeHist t_2sq_no
                h_2sq_nn = takeHist t_2sq_nn
                totalsr = mkTotalSR [h_2sg, h_sqsg_o, h_sqsg_n, h_2sq_oo, h_2sq_no, h_2sq_nn]
                r_ratio = getRFromSR totalsr


            trace (show (x,y)) $ return (read x :: Double, read y :: Double, r_ratio)
          case r of 
            Left err -> error err 
            Right result -> return result
      )
  hClose outh 


main' = do 
  r <- runEitherT $ mapM_ (EitherT . checkFiles ChanCount) dirset 
  print r

{-
  let str = "2sq_no_2l2j2x" 
  r <- runEitherT (mainCount str) 
  case r of 
    Left err -> putStrLn err
    Right _ -> return ()
 -}
 

      

getCount (rdir,basename) = do 
  let nlst = [1]
  r1 <- work (\wdavcfg wdavrdir nm -> getXSecNCount XSecLHE wdavcfg wdavrdir nm >>= getJSONFileAndUpload wdavcfg wdavrdir nm)
         "config1.txt" 
         rdir 
         basename 
         nlst 
  print r1

  r2 <- work 
         (atlas_8TeV_0L2to6J_bkgtest ([0],[0]))
         "config1.txt"
         rdir
         basename
         nlst
  print r2 


