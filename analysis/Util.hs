{-# LANGUAGE ScopedTypeVariables #-}

module Util where 

import Control.Monad
import Control.Monad.Trans 
import Control.Monad.Trans.Either
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Reader
import qualified Data.Aeson.Generic as G
import qualified Data.ByteString.Lazy.Char8 as LB
import Data.Data 
-- 
import HEP.Storage.WebDAV.CURL
import HEP.Storage.WebDAV.Type
import HEP.Util.Either
--
import HEP.Physics.Analysis.ATLAS.Common 
import HEP.Physics.Analysis.ATLAS.SUSY.SUSY_0L2to6JMET_8TeV
import HEP.Physics.Analysis.Common.XSecNTotNum
import HEP.Util.Work


takeR [Just (_,_,_,r)] = r 

takeHist [Just (_,_,h,_)] = h

takeResult [Just (_,r,_,_)] = r

data DataFileClass = RawData | TotalCount | ChanCount | Prospino

checkFileExistInDAV :: DataFileClass -> WebDAVConfig -> WebDAVRemoteDir -> String -> IO (Maybe (Maybe ()))
checkFileExistInDAV datcls wdavcfg wdavrdir bname = do 
  let fp1 = bname ++ "_ATLAS8TeV0L2to6JBkgTest.json"
      fp2 = bname ++ "_total_count.json" 
      fp3 = bname ++ "_pgs_events.lhco.gz"
      prospinofp = bname ++ "_xsecKfactor.json"
      fp = case datcls of 
             RawData -> fp3
             TotalCount -> fp2
             ChanCount -> fp1 
             Prospino -> prospinofp
  print fp 
  b <- doesFileExistInDAV wdavcfg wdavrdir fp 
  if b then return (Just (Just ()))  else return Nothing 

checkFileExistInDAV_lep :: DataFileClass -> WebDAVConfig -> WebDAVRemoteDir -> String -> IO (Maybe (Maybe ()))
checkFileExistInDAV_lep datcls wdavcfg wdavrdir bname = do 
  let fp1 = bname ++ "_ATLAS_1to2L2to6JMET_8TeV.json"
      fp2 = bname ++ "_total_count.json" 
      fp3 = bname ++ "_pgs_events.lhco.gz"
      prospinofp = bname ++ "_xsecKfactor.json"
      fp = case datcls of 
             RawData -> fp3
             TotalCount -> fp2
             ChanCount -> fp1 
             Prospino -> prospinofp
  print fp 
  print wdavrdir 
  b <- doesFileExistInDAV wdavcfg wdavrdir fp 
  if b then return (Just (Just ()))  else return Nothing 


doJob ns wk (rdir,basename) = do
  Right r1 <- work wk "config1.txt" rdir basename ns 
  return r1 


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


downloadAndDecodeJSON :: (Data a) => 
                         WebDAVRemoteDir -> FilePath -> EitherT String (ReaderT WebDAVConfig IO) a 
downloadAndDecodeJSON wdavrdir fp = do 
  wdavcfg <- lift ask 
  guardEitherM (fp ++ " not exist!") (liftIO (doesFileExistInDAV wdavcfg wdavrdir fp))
  (_,mr) <- liftIO (downloadFile True wdavcfg wdavrdir fp)
  r <-  (liftM LB.pack . EitherT . return . maybeToEither (fp ++ " is not downloaded ")) mr 
  result <- 
    (EitherT . return . maybeToEither (fp ++ " JSON cannot be decoded") . G.decode) r
  return result 
