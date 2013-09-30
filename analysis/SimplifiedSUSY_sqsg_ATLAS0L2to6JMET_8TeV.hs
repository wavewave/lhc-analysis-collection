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
import HEP.Util.Work 
--
import Util
import Debug.Trace

m_neutralino :: Double 
m_neutralino = 10 -- 500

datalst :: [ (Double,Double) ]
datalst = [ (g,q) | g <- [2500], q <- [200,300..3000] ]

{-
datalst :: [ (Double,Double) ]
datalst = [ (g,q) | g <- [m_neutralino+100,m_neutralino+200..3000], q <- [m_neutralino+100,m_neutralino+200..3000] ]
-}

datalst1of2 :: [ (Double,Double) ]
datalst1of2 = [ (g,q) | g <- [m_neutralino+100,m_neutralino+200..1500], q <- [m_neutralino+100,m_neutralino+200..3000] ]

datalst2of2 :: [ (Double,Double) ]
datalst2of2 = [ (g,q) | g <- [1600,1700..3000], q <- [m_neutralino+100,m_neutralino+200..3000] ]



checkFiles :: DataFileClass -> String -> IO (Either String ())
checkFiles c procname = do 
  rs <- forM datalst (\s -> (doJob (checkFileExistInDAV c)  . createRdirBName procname) s 
                                >>= return . maybe (show s) (const []) . head)
  let missinglst = filter (not.null) rs
      nmiss = length missinglst
  mapM_ (\x -> putStrLn ("  , " ++ x)) missinglst
  if null missinglst then return (Right ()) else return (Left (show nmiss ++ " files are missing"))

createRdirBName procname (mg,mq) = 
  let rdir = "montecarlo/admproject/SimplifiedSUSY/8TeV/scan_" ++ procname 
      basename = "SimplifiedSUSYMN" ++ show m_neutralino ++ "MG"++show mg++ "MSQ" ++ show mq ++ "_" ++ procname ++ "_LHC8ATLAS_NoMatch_NoCut_AntiKT0.4_NoTau_Set"
  in (rdir,basename)  


dirset = [ "2sg_4j2n"
         , "sqsg_3j2n"
         , "2sq_2j2n"
         ] 


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




mainAnalysis = do
  outh <- openFile ("simplifiedsusy" ++ show m_neutralino ++ "_sqsg_8TeV_0lep_temp.dat") WriteMode 
  mapM_ (\(mg,msq,r) -> hPutStrLn outh (show mg ++ ", " ++ show msq ++ ", " ++ show r))
    =<< forM datalst ( \(x,y) -> do
          r <- runEitherT $ do
            let analysis x = getResult atlas_20_3_fbinv_at_8_TeV . createRdirBName x
                simplify = fmap head . fmap catMaybes . EitherT
                takeXSec (x,_,_,_) = crossSectionInPb x * 20300
                takeHist (_,_,h,_) = h
            t_2sg  <- (simplify . analysis "2sg_4j2n")  (x,y)
            t_sqsg <- (simplify . analysis "sqsg_3j2n") (x,y)
            t_2sq  <- (simplify . analysis "2sq_2j2n")  (x,y)

            let h_2sg  = takeHist t_2sg
                h_sqsg = takeHist t_sqsg
                h_2sq  = takeHist t_2sq

                x_2sg  = takeXSec t_2sg
                x_sqsg = takeXSec t_sqsg
                x_2sq  = takeXSec t_2sq

                totalsr = mkTotalSR [h_2sg, h_sqsg, h_2sq]
                totalpr = x_2sg + x_sqsg + x_2sq
                totalsrpr = multiplyScalar (1.0/totalpr) totalsr

                r_ratio = getRFromSR totalsr


            -- trace (show (x,y)) $ return (x :: Double, y :: Double, r_ratio)
            trace (show (x,y,h_2sg)) $ return (x :: Double, y :: Double, totalsrpr) 
          case r of 
            Left err -> error err 
            Right result -> return result
      )
  hClose outh 


mainCheck = do 
  r <- runEitherT $ mapM_ (EitherT . checkFiles ChanCount) (take 1 dirset)
  print r 


mainCount str = do 
  r <- runEitherT (countEvent str) 
  case r of 
    Left err -> putStrLn err
    Right _ -> return ()

main = do 
  args <- getArgs
  case args !! 0 of 
    "count" -> case args !! 1 of
                 "2sg" -> mainCount "2sg_4j2n"
                 "sqsg" -> mainCount "sqsg_3j2n" 
                 "2sq" -> mainCount "2sq_2j2n"
    "check" -> mainCheck
    "analysis" -> mainAnalysis
 
countEvent :: String -> EitherT String IO ()
countEvent str = do 
  EitherT (checkFiles RawData str)
  liftIO $ putStrLn "Proceed 1 or 2 ? (1/2/others)"
  c <- liftIO $ getChar
  if c == '1' 
    then liftIO $ forM_ datalst1of2 (getCount.createRdirBName str)
    else if c == '2' 
           then liftIO $ forM_ datalst2of2 (getCount.createRdirBName str)
           else return ()


      

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


