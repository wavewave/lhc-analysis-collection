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
import HEP.Physics.Analysis.Common.Prospino
import HEP.Util.Work 
--
import KFactor
import Util
import Debug.Trace

m_neutralino :: Double 
m_neutralino = 500

-- datalst :: [ ((Double,Double), [Int]) ]
-- datalst = [ ((1000,1000),[1]) ]

datalst :: [ ((Double,Double), [Int]) ]
datalst = [ ((1000,1000),[2]) ]

datalst1of2 = datalst

datalst2of2 = [] 


{-
datalst :: [ ((Double,Double), [Int]) ]
datalst = [ ((g,q),[1]) | g <- [m_neutralino+100,m_neutralino+200..3000], q <- [m_neutralino+100,m_neutralino+200..3000] ]
-}

{-
datalst1of2 :: [ (Double,Double) ]
datalst1of2 = [ (g,q) | g <- [m_neutralino+100,m_neutralino+200..1500], q <- [m_neutralino+100,m_neutralino+200..3000] ]

datalst2of2 :: [ (Double,Double) ]
datalst2of2 = [ (g,q) | g <- [1600,1700..3000], q <- [m_neutralino+100,m_neutralino+200..3000] ]
-}

checkFiles :: DataFileClass -> String -> IO (Either String ())
checkFiles c procname = do 
  rs <- forM datalst (\((x,y),ns) -> doJob ns (checkFileExistInDAV c) (createRdirBName procname (x,y))
                                     >>= return . maybe (show ((x,y),ns)) (const []) . head)
  let missinglst = filter (not.null) rs
      nmiss = length missinglst
  mapM_ (\x -> putStrLn ("  , " ++ x)) missinglst
  if null missinglst then return (Right ()) else return (Left (show nmiss ++ " files are missing"))


createRdirBName :: String -> (Double, Double) -> (String,String)
createRdirBName procname (mg,mq) = 
  let rdir = "montecarlo/admproject/XQLDdegen/8TeV/neutLOSP/scan_" ++ procname 
      basename = "ADMXQLD111degenMG"++ show mg++ "MQ" ++ show mq ++ "ML50000.0MN" ++ show m_neutralino ++ "_" ++ procname ++ "_LHC8ATLAS_NoMatch_NoCut_AntiKT0.4_NoTau_Set" 
  in (rdir,basename)  

dirset = [ "2sg_2l8j2x"
         , "sqsg_2l7j2x"
         , "2sq_2l6j2x"
         ]

luminosity = 20300 

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


