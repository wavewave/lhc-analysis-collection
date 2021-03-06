{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}

import Control.Monad 
import           Control.Monad.Trans.Maybe 
-- import Data.Attoparsec.Lazy
import qualified Data.Aeson.Generic as G
import qualified Data.ByteString.Lazy.Char8 as LB
import           Data.Foldable (foldrM)
import System.IO
-- 
import HEP.Storage.WebDAV.CURL
-- import HEP.Storage.WebDAV.Util
import HEP.Util.Either 
-- 
import HEP.Physics.Analysis.ATLAS.Common
import HEP.Physics.Analysis.ATLAS.SUSY.SUSY_0L2to6J
import HEP.Physics.Analysis.Common.XSecNTotNum
import HEP.Util.Work 

datalst = map ((,) "50000.0") [ "100.0"
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

takeR [Just (_,_,_,r)] = r 

main = do 
  forM_ datalst (uncurry getCount)
  h <- openFile "xqlddata.dat" WriteMode 
  mapM_ (\(a,b)-> hPutStrLn h (a ++ ", " ++ b))
    =<< forM datalst (\(x,y) -> getLHCresult x y >>= \t -> return (y, show (takeR t)))
  hClose h 

  -- mapM_ print datalst 
{-  h <- openFile "simplifiedsquark.dat" WriteMode 
  forM_ datalst $ \(x,y) -> do  
    [Just (_,_,_,r)] <- getLHCresult x y 
    hPutStrLn h $ x ++ ", " ++ y ++ ", " ++ (T.unpack . TB.toLazyText . TF.fixed 2) r  

  hClose h -}

getLHCresult mg mq = do 
  let nlst = [1]
      rdir = "montecarlo/admproject/XQLD/scan" 
      basename = "ADMXQLD111MG"++mg++ "MQ" ++ mq ++ "ML50000.0MN50000.0_2sd_2l2j2x_LHC7ATLAS_NoMatch_NoCut_AntiKT0.4_NoTau_Set"
  Right r1 <- work -- fetchXSecNHist 
                       atlasresult_4_7fb
                       "config1.txt" 
                       rdir 
                       basename 
                       nlst 
  return r1 
  

        

getCount mg mq = do 
  {- args <- getArgs 
  let n1 :: String = (args !! 0) 
      n2 :: String = (args !! 1) 
  -}
  let nlst = [1]
      rdir = "montecarlo/admproject/XQLD/scan" 
      basename = "ADMXQLD111MG"++mg++ "MQ" ++ mq ++ "ML50000.0MN50000.0_2sd_2l2j2x_LHC7ATLAS_NoMatch_NoCut_AntiKT0.4_NoTau_Set"

  r1 <- work (\wdavcfg wdavrdir nm -> getXSecNCount XSecLHE wdavcfg wdavrdir nm >>= getJSONFileAndUpload wdavcfg wdavrdir nm)
         "config1.txt" 
         rdir 
         basename 
         nlst 
  print r1

  r2 <- work 
         (atlas_7TeV_0L2to6J_bkgtest ([5],[2]))
         "config1.txt"
         rdir
         basename
         nlst
  print r2 

{-
work :: (WebDAVConfig -> WebDAVRemoteDir -> String -> IO a) 
     -> FilePath 
     -> FilePath 
     -> FilePath 
     -> [Int] 
     -> IO (Either String [a])
work task cfgfile rdir bname sets = 
  runEitherT $ do 
    cfg <- (EitherT . liftM (maybeToEither "getConfig")) (getConfig cfgfile)
    let priv = evgen_privatekeyfile cfg 
        pass = evgen_passwordstore cfg 
        wdavroot = evgen_webdavroot cfg 
    cr <- (EitherT . liftM (maybeToEither "getCredential")) (getCredential priv pass)
    let wdavcfg = WebDAVConfig cr wdavroot 
        wdavrdir = WebDAVRemoteDir rdir 
        bnames = map (\x -> bname ++ show x) sets
    liftIO $ mapM (task wdavcfg wdavrdir) bnames 
-}


-- atlasresult_4_7fb :: WebDAVConfig -> WebDAVRemoteDir -> String ->IO (Maybe ([ (EType,Double) ], Double))
--  -> IO (Maybe (CrossSectionAndCount,[(JESParam,HistEType)]))
atlasresult_4_7fb wdavcfg wdavrdir bname = do 
  let fp1 = bname ++ "_ATLAS7TeV0L2to6JBkgTest.json"
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

    let weight = crossSectionInPb xsec * 4700 / fromIntegral (numberOfEvent xsec)
        hist = map (\(x,y) -> (x,fromIntegral y * weight)) ((snd . head) result )
    let getratio (x,y) = do y' <- lookup x nbsmlimit 
                            return (y/ y') 
        maxf (x,y) acc = do r <- getratio (x,y)
                            return (max acc r)
    maxratio <- MaybeT . return $ foldrM maxf 0 hist 

    return (xsec,result,hist,maxratio) -- (xsec,result)


nbsmlimit = [ (CL, 51) 
            , (EL, 77) 
            , (AM, 24) 
            , (A'M, 28) 
            , (CM, 17)
            , (EM, 11) 
            , (AT, 3.1) 
            , (BT, 3.0)
            , (CT, 16)
            , (DT, 9.6) 
            , (ET, 12) ] 

