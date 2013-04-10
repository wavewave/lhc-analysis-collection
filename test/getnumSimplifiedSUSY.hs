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
import           Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.Aeson.Generic as G
import qualified Data.ByteString.Char8 as B 
import qualified Data.ByteString.Lazy.Char8 as LB
import           Data.Data
import           Data.Foldable (foldrM)
import Data.Function (on)
import Data.List (lookup, sortBy) 
import Data.Maybe 
import Data.String
import qualified Data.Text.Lazy as T
import qualified Data.Text.Format as TF
import qualified Data.Text.Lazy.Builder as TB
import Data.Typeable
import System.Environment (getArgs)
import System.IO
-- 
import HEP.Automation.EventGeneration.Config
import HEP.Storage.WebDAV.CURL
import HEP.Storage.WebDAV.Type 
-- import HEP.Storage.WebDAV.Util
import HEP.Util.Either 
-- 
import HEP.Physics.Analysis.ATLAS.SUSY_0L2to6J
import HEP.Physics.Analysis.Common.XSecNTotNum





-- (\wdavcfg wdavrdir nm -> getXSecNCount wdavcfg wdavrdir nm >>= getJSONFileAndUpload wdavcfg wdavrdir nm)
-- atlas_7TeV_0L2to6J_bkgtest

            -- testprint 
            -- 
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
  -- mapM_ print datalst 
  h <- openFile "simplifiedsquark.dat" WriteMode 
  forM_ datalst $ \(x,y) -> do  
    [Just (_,_,_,r)] <- getLHCresult x y 
    hPutStrLn h $ x ++ ", " ++ y ++ ", " ++ (T.unpack . TB.toLazyText . TF.fixed 2) r  

  hClose h 

getLHCresult n1 n2 = do 
  let nlst = [1]
      rdir = "montecarlo/admproject/SimplifiedSUSY/scan" 
      basename = "SimplifiedSUSYMN"++n1++ "MG50000.0MSQ" ++ n2 ++ "_2sq_2j2x_LHC7ATLAS_NoMatch_NoCut_AntiKT0.4_NoTau_Set"
  Right r1 <- work -- fetchXSecNHist 
                       atlasresult_4_7fb
                       "config1.txt" 
                       rdir 
                       basename 
                       nlst 
  return r1 
  

        

getCount n1 n2 = do 
  {- args <- getArgs 
  let n1 :: String = (args !! 0) 
      n2 :: String = (args !! 1) 
  -}
  let nlst = [1]
      rdir = "montecarlo/admproject/SimplifiedSUSY/scan" 
      basename = "SimplifiedSUSYMN"++n1++ "MG50000.0MSQ" ++ n2 ++ "_2sq_2j2x_LHC7ATLAS_NoMatch_NoCut_AntiKT0.4_NoTau_Set"

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


{-   case r of 

    Left err -> putStrLn err 
    Right vs -> do {-  return () -}
      let vs' = catMaybes vs 
      let totevts = (sum . map (numberOfEvent.fst)) vs'
          mul = (*) <$> crossSectionInPb <*> fromIntegral . numberOfEvent
          totcross = (/ (fromIntegral totevts)) . sum . map (mul . fst) $ vs'  
          -- just for test yet 
          weight = {- 165.0 -} totcross * 4700 / fromIntegral totevts 
          test a b = let hists = mapMaybe (lookup (JESParam a b)) . map snd $ vs'
                         sumup k = (sum . mapMaybe (lookup k)) hists
                         totsr :: TotalSR Int 
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
      let xsecn = CrossSectionAndCount totcross totevts
          combined = (xsecn,[ test a b | a <- [0,1..20], b <- [0,1..10] ] )
          fn = basename ++ show n1 ++ "to" ++ show n2 ++ "_ATLAS7TeV0L2to6JBkgTest.json" 
          bstr = encodePretty combined 
      --  print (encodePretty combined)
      LB.writeFile fn bstr 
-}



      {- 
      let lst = [ ((,,) <$> fst <*> chisquareZJets . snd <*> snd) (test a b) | a <- [0,1..20], b <- [0,1..10] ]
          lst' = sortBy (compare `on` (view _2)) lst 
      mapM_ print lst'
      -}

-- -}



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

