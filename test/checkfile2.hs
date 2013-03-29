{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}

import Control.Applicative ((<$>),(<*>),pure)
import           Control.Lens 
import Control.Monad 
import           Control.Monad.Trans 
import           Control.Monad.Trans.Either 
import           Control.Monad.Trans.Maybe 
import Data.Attoparsec.Char8 hiding (take)
-- import Data.Attoparsec.Lazy
import           Data.Aeson 
import qualified Data.Aeson.Generic as G
import qualified Data.ByteString.Char8 as B 
import qualified Data.ByteString.Lazy.Char8 as LB
import Data.Function (on)
import Data.List (lookup, sortBy) 
import Data.Maybe 
import Data.String
import Data.Typeable
import System.Environment (getArgs)
-- 
import HEP.Automation.EventGeneration.Config
import HEP.Storage.WebDAV.CURL
import HEP.Storage.WebDAV.Type 
-- import HEP.Storage.WebDAV.Util
import HEP.Util.Either 
-- 
import HEP.Physics.Analysis.ATLAS.SUSY_0L2to6J
import HEP.Physics.Analysis.Common.XSecNTotNum


data TotalSR = TotalSR { numCL :: Double 
                       , numEL :: Double
                       , numAM :: Double
                       , numA'M :: Double 
                       , numCM :: Double
                       , numEM :: Double 
                       , numAT :: Double 
                       , numBT :: Double
                       , numCT :: Double 
                       , numDT :: Double 
                       , numET :: Double } 
               deriving (Show,Eq)
 

chisquare :: TotalSR -> Double 
chisquare TotalSR {..} = ((numCL - 74)^2) / (14^2)  
                         + ((numEL - 73)^2) / (25^2) 
                         + ((numAM - 6.8)^2) / (4.7^2) 
                         + ((numA'M - 11)^2) / (4.0^2) 
                         + ((numCM - 13)^2) / (5^2) 
                         + ((numEM - 19)^2) / (6^2) 
                         + ((numAT - 0.2)^2) / (0.2^2) 
                         + ((numBT - 0.3)^2) / (0.3^2) 
                         + ((numCT - 2.0)^2) / (1.5^2) 
                         + ((numDT - 2.4)^2) / (1.7^2) 
                         + ((numET - 4.2)^2) / (4.7^2)  


-- (\wdavcfg wdavrdir nm -> getXSecNCount wdavcfg wdavrdir nm >>= getJSONFileAndUpload wdavcfg wdavrdir nm)
-- atlas_7TeV_0L2to6J_bkgtest

            -- testprint 
            -- 

main = do 
  args <- getArgs 
  let n1 :: Int = read (args !! 0) 
      n2 :: Int = read (args !! 1) 
      nlst = (drop (n1-1) . take n2) [1..] 
  r <- work atlas_7TeV_0L2to6J_bkgtest
         "config1.txt" 
         "montecarlo/admproject/smbkg/z0123" 
         "SM_z0123j_LHC7ATLAS_MLM_DefCut_AntiKT0.4_NoTau_Set"
         nlst 
         -- [1000]
         -- [1..100] 
         -- [1..100]
         -- [91..100]
         -- [81..90]
         -- [71..80] 
         -- [61..70]
         -- [51..60]
         -- [41..50]
         -- [31..40]
         -- [24..30]
         -- [11..100]
         -- [1..10] 
         -- [1..1000] 
          -- [21..30]
  case r of 
    Left err -> putStrLn err 
    Right vs -> do return ()
{-      let vs' = catMaybes vs 
      let totevts = (sum . map (numberOfEvent.fst)) vs'
          mul = (*) <$> crossSectionInPb <*> fromIntegral . numberOfEvent
          totcross = (/ (fromIntegral totevts)) . sum . map (mul . fst) $ vs'  
          -- just for test yet 
          weight = 165.0 * 4700 / fromIntegral totevts 
          test a b = let hists = mapMaybe (lookup (JESParam a b)) . map snd $ vs'
                         sumup k = ((*weight) . fromIntegral . sum . mapMaybe (lookup k)) hists
                         totsr = TotalSR { numCL = sumup CL 
                                         , numEL = sumup EL 
                                         , numAM = sumup AM
                                         , numA'M = sumup A'M 
                                         , numCM = sumup CM 
                                         , numEM = sumup EM
                                         , numAT = sumup AT 
                                         , numBT = sumup BT 
                                         , numCT = sumup CT 
                                         , numDT = sumup DT 
                                         , numET = sumup ET } 
                     in (JESParam a b, totsr) 
      -- print weight -- totcross 
      let lst = [ ((,,) <$> fst <*> chisquare. snd <*> snd) (test a b) | a <- [0,1..20], b <- [0,1..10] ]
          lst' = sortBy (compare `on` (view _2)) lst 
      mapM_ print lst'

-}



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


{- "montecarlo/admproject/smbkg/wp0123" -}
-- ..100] -- [1..4500]
 {- "SM_wp0123j_LHC7ATLAS_MLM_DefCut_AntiKT0.4_NoTau_Set" -}
--     liftIO $ mapM (\nm -> getXSecNCount wdavcfg wdavrdir nm >>= getJSONFileAndUpload wdavcfg wdavrdir nm) bnames 


fetchXSecNHist :: WebDAVConfig -> WebDAVRemoteDir -> String -> IO (Maybe (CrossSectionAndCount,[(JESParam,HistEType)]))
fetchXSecNHist wdavcfg wdavrdir bname = do 
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
    return (xsec,result)
