{-# LANGUAGE RecordWildCards, GADTs, ScopedTypeVariables #-}

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Either
import Control.Monad.Trans.Maybe
import           Codec.Compression.GZip
import qualified Data.Aeson.Generic as G
import qualified Data.ByteString.Lazy.Char8 as LB
import           Data.Foldable (foldrM)
import           Data.Maybe
import qualified Data.Text.Lazy as T
import qualified Data.Text.Format as TF
import qualified Data.Text.Lazy.Builder as TB
import           System.Directory
import           System.FilePath
import           System.IO 
-- 
import HEP.Automation.EventGeneration.Config
import HEP.Storage.WebDAV.CURL
import HEP.Storage.WebDAV.Type 
-- import HEP.Storage.WebDAV.Util
import HEP.Util.Either 
import           HEP.Parser.LHCOAnalysis.Parse
-- 
import           HEP.Physics.Analysis.Common.XSecNTotNum
import           HEP.Physics.Analysis.ATLAS.Common
import           HEP.Physics.Analysis.ATLAS.SUSY_MultiLepton
-- import           HEP.Physics.Analysis.ATLAS.SUSY_MultiLepton.PrettyPrint
-- 
import Debug.Trace



datalst_squark = [ "200.0", "300.0", "400.0", "500.0", "600.0"
                 , "700.0", "800.0", "900.0", "1000.0", "1100.0", "1200.0" ] 
datalst = map (\x->("100.0",x)) datalst_squark
          ++ map (\x->("200.0",x)) (drop 1 datalst_squark)
          ++ map (\x->("300.0",x)) (drop 2 datalst_squark)
          ++ map (\x->("400.0",x)) (drop 3 datalst_squark)
          ++ map (\x->("500.0",x)) (drop 4 datalst_squark)
          ++ map (\x->("600.0",x)) (drop 5 datalst_squark)
          ++ map (\x->("700.0",x)) (drop 6 datalst_squark)
          ++ map (\x->("800.0",x)) (drop 7 datalst_squark)
          ++ map (\x->("900.0",x)) (drop 8 datalst_squark)
          ++ map (\x->("1000.0",x)) (drop 9 datalst_squark)
          ++ map (\x->("1100.0",x)) (drop 10 datalst_squark)

main = do 
  h <- openFile "simplifiedsquark_multilep.dat" WriteMode
  forM_ datalst $ \(x,y) -> do  
    getCount x y 
    [(Just (_,_,_,r))] <- getLHCresult x y
    hPutStrLn h $ x ++ ", " ++ y ++ ", " ++ (T.unpack . TB.toLazyText . TF.fixed 2) r
    -- print r 
  hClose h


getLHCresult n1 n2 = do 
  let nlst = [1]
      rdir = "montecarlo/admproject/SimplifiedSUSY/scan2w2j2x" 
      basename = "SimplifiedSUSYMN"++n1++ "MG50000.0MSQ" ++ n2 ++ "_2sq_2w2j2x_LHC7ATLAS_NoMatch_NoCut_AntiKT0.4_NoTau_Set"
  Right r1 <- work atlasresult_4_7fb
                   "config1.txt" 
                   rdir 
                   basename 
                   nlst 
  return r1 


atlasresult_4_7fb wdavcfg wdavrdir bname = do
  let fp1 = bname ++ "_ATLAS7TeVMultiL2to4J.json"
      fp2 = bname ++ "_total_count.json" 
  runMaybeT $ do  
    (_,mr1) <- MaybeT . boolToMaybeM (doesFileExistInDAV wdavcfg wdavrdir fp1) 
                      . downloadFile True wdavcfg wdavrdir $ fp1 
    r1 <- liftM LB.pack (MaybeT . return $ mr1) 
    (result :: [(JESParam,[(EventTypeCode,Int)])]) <- MaybeT . return $ G.decode r1 
   
    (_,mr2) <- MaybeT . boolToMaybeM (doesFileExistInDAV wdavcfg wdavrdir fp2) 
                      . downloadFile True wdavcfg wdavrdir $ fp2
    r2 <- liftM LB.pack (MaybeT . return $ mr2) 
    (xsec :: CrossSectionAndCount) <- MaybeT . return $ G.decode  r2  

    let weight = crossSectionInPb xsec * 4700 / fromIntegral (numberOfEvent xsec)
        hist = map (\(x,y) -> (x,fromIntegral y * weight)) ((snd.head) result)

    let getratio (x,y) = do y' <- lookup x nbsmlimit 
                            return (y/ y') 
        maxf (x,y) acc = do r <- getratio (x,y)
                            return (max acc r)
    maxratio <- MaybeT . return $ foldrM maxf 0 hist 

    return (xsec,result,hist,maxratio) 





getCount n1 n2 = do 
  let nlst = [1]
      rdir = "montecarlo/admproject/SimplifiedSUSY/scan2w2j2x" 
      basename = "SimplifiedSUSYMN"++n1++ "MG50000.0MSQ" ++ n2 ++ "_2sq_2w2j2x_LHC7ATLAS_NoMatch_NoCut_AntiKT0.4_NoTau_Set"
  r1 <- work (\wdavcfg wdavrdir nm -> getXSecNCount XSecLHE wdavcfg wdavrdir nm >>= getJSONFileAndUpload wdavcfg wdavrdir nm)
         "config1.txt" 
         rdir 
         basename 
         nlst 
  print r1
  r2 <- work 
         (atlas_7TeV_MultiL2to4J (JESParam 5 2))
         "config1.txt"
         rdir
         basename
         nlst
  print r2 







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
  
nbsmlimit = [ (SingleHardElec3J, 4.4) 
            , (SingleHardMuon3J, 3.6)
            , (SingleHardElec4J, 5.8)
            , (SingleHardMuon4J, 4.5)
            , (SingleSoftElec  , 8.6)
            , (SingleSoftMuon  , 9.0)
            , (MultiElecElec2J , 3.3)
            , (MultiMuonMuon2J , 3.6) 
            , (MultiElecMuon2J , 3.9)
            , (MultiElecElec4J , 7.2)
            , (MultiMuonMuon4J , 9.1)
            , (MultiElecMuon4J , 10.1)
            ]


{-
(CL, 51) 
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
-}

