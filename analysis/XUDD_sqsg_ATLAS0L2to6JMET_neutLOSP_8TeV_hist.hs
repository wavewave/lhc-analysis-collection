{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}

import Control.Lens
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

import HROOT



m_neutralino :: Double 
m_neutralino = 100

luminosity :: Double 
luminosity = 20300 

createRdirBName procname (mg,mq) = 
  let rdir = "montecarlo/admproject/XUDDdegen/8TeV/neutLOSP/scan_" ++ procname 
      basename = "ADMXUDD112degenMG"++ show mg++ "MQ" ++ show mq ++ "ML50000.0MN" ++ show m_neutralino ++ "_" ++ procname ++ "_LHC8ATLAS_NoMatch_NoCut_AntiKT0.4_NoTau_Set"
  in (rdir,basename)  

dirset = [ "2sg_10j2x"
         , "sqsg_9j2x" 
         , "2sq_8j2x"
         ]


mkHistMissingET :: WebDAVConfig -> WebDAVRemoteDir -> String 
                -> EitherT String IO (CrossSectionAndCount,[(JESParam,HistEType)],[(EType,Double)],Double)
mkHistMissingET wdavcfg wdavrdir bname = do 
  let fp1 = bname ++ "_ATLAS8TeV0L2to6JBkgTest.json"
      fp2 = bname ++ "_total_count.json" 
  --
  guardEitherM (fp1 ++ " not exist!") (doesFileExistInDAV wdavcfg wdavrdir fp1)
  (_,mr1) <- liftIO (downloadFile True wdavcfg wdavrdir fp1)
  r1 <- (liftM LB.pack . EitherT . return . maybeToEither (fp1 ++ " is not downloaded ")) mr1 
  (result :: [(JESParam, HistEType)]) <- (EitherT . return . maybeToEither (fp1 ++ " JSON cannot be decoded") . G.decode) r1 
  -- 
  guardEitherM (fp2 ++ " not exist!") (doesFileExistInDAV wdavcfg wdavrdir fp2) 
  (_,mr2) <- liftIO (downloadFile True wdavcfg wdavrdir fp2)
  r2 <- (liftM LB.pack . EitherT . return . maybeToEither (fp2 ++ " is not downloaded ")) mr2 
  (xsec :: CrossSectionAndCount) <- (EitherT . return . maybeToEither (fp2 ++ " JSON cannot be decoded") . G.decode)  r2  

  let weight = crossSectionInPb xsec * luminosity / fromIntegral (numberOfEvent xsec)
      hist = map (\(x,y) -> (x,fromIntegral y * weight)) ((snd . head) result )

  let getratio (x,y) = do y' <- lookup x limitOfNBSM 
                          return (y/ y') 
      maxf (x,y) acc = do r <- getratio (x,y)
                          return (max acc r)
  maxratio <- EitherT . return . maybeToEither "in mkhist:foldrM" $ foldrM maxf 0 hist 

  return (xsec, result, hist, maxratio) 



getResult f (rdir,basename) = do 
  let nlst = [1]
  fileWork f "config1.txt" rdir basename nlst 

main' :: IO ()
main' = do
  let outh = stdout  
      mg = 1000.0 :: Double 
      msq = 1000.0 :: Double
  r <- runEitherT $ do
         let analysis x = getResult mkHistMissingET . createRdirBName x
             simplify = fmap head
             takeHist (_,_,h,_) = h
         t_2sg    <- (simplify . analysis "2sg_10j2x") (mg,msq)
         t_sqsg   <- (simplify . analysis "sqsg_9j2x") (mg,msq)
         t_2sq    <- (simplify . analysis "2sq_8j2x") (mg,msq)

         let h_2sg  = takeHist t_2sg
             h_sqsg = takeHist t_sqsg
             h_2sq  = takeHist t_2sq
             totalsr = mkTotalSR [ h_2sg, h_sqsg, h_2sq ]
             r_ratio = getRFromSR totalsr
         trace (show (mg,msq,h_2sg)) $ return (mg, msq, r_ratio)
  case r of 
    Left err -> error err 
    Right result -> print result
 

  -- hClose outh 


main = do 
  let mg = 600.0 :: Double
      mq = 600.0 :: Double 
  r <- runEitherT (countEvent (mg,mq) "2sg_10j2x")
  case r of 
    Left err -> putStrLn err
    Right _ -> return ()


testroot = do 
  tcanvas <- newTCanvas "test" "test" 640 480
  h1 <- newTH1F "test" "test" 100 1 10
  h2 <- newTH1F "test" "test" 100 1 10 
  
  fill1 h1 5 
  fill1 h2 6
  
  add h1 h2 1.0
  
  draw h1 "" 

  tfile <- newTFile "test.root" "NEW" "" 1   
  write h1 "" 0 0 
  close tfile ""

  delete tfile
  delete h1
  delete h2

  delete tcanvas





countEvent :: (Double,Double) -> String -> EitherT String IO ()
countEvent (mg,mq) str = do 
  tfile <- liftIO $ newTFile "test.root" "NEW" "" 1   
  ha <- liftIO $ newTH1F "2jet" "2jet" 50 0 1000
  hb <- liftIO $ newTH1F "3jet" "3jet" 50 0 1000
  hc <- liftIO $ newTH1F "4jet" "4jet" 50 0 1000
  hd <- liftIO $ newTH1F "5jet" "5jet" 50 0 1000
  he <- liftIO $ newTH1F "6jet" "6jet" 50 0 1000

  let fillfunc :: (Double,(Bool,Bool,Bool,Bool,Bool)) -> IO ()
      fillfunc (x,sr) = do
        when (view _1 sr) (fill1 ha x >> return ())
        when (view _2 sr) (fill1 hb x >> return ())
        when (view _3 sr) (fill1 hc x >> return ())
        when (view _4 sr) (fill1 hd x >> return ())
        when (view _5 sr) (fill1 he x >> return ())
       
  r <- (getCount . createRdirBName str) (mg,mq)

  liftIO ( mapM_ fillfunc r )
  liftIO $ (mapM_ print r )
  liftIO $ mapM_ (\x->write x "" 0 0) [ha,hb,hc,hd,he]
  liftIO $ close tfile ""
      

getCount (rdir,basename) = do 
  let nlst = [1]
  rs <- fileWork atlas_getMissingET "config1.txt" rdir basename nlst
  return (concat rs)
       

