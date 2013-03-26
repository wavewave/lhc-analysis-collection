{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}

import Codec.Compression.GZip 
import Control.Applicative ((<$>),(<*>),pure)
import Control.Monad 
import Control.Monad.Trans
import Control.Monad.Trans.Either 
import Control.Monad.Trans.Maybe
-- import Data.Attoparsec.Char8
-- import Data.Attoparsec.Lazy
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.Aeson.Types
import qualified Data.Aeson.Generic as G
import qualified Data.ByteString.Char8 as B 
import qualified Data.ByteString.Lazy.Char8 as LB
import Data.Data
import Data.Maybe 
import Data.String
import Data.Typeable
import System.Directory  
import System.FilePath ((</>))
import System.Process 
-- 
import HEP.Automation.EventGeneration.Config
import HEP.Parser.LHCOAnalysis.Parse
import HEP.Storage.WebDAV.CURL 
import HEP.Storage.WebDAV.Type 
import HEP.Util.Either 
-- import HEP.Storage.WebDAV.Util
-- 
import HEP.Parser.XSec

data CrossSectionAndCount = CrossSectionAndCount { crossSectionInPb :: Double
                                                 , numberOfEvent :: Int } 
    deriving (Show,Eq,Data,Typeable)
                           
instance ToJSON CrossSectionAndCount where
  toJSON = G.toJSON


main = do 
  r <- runEitherT $ do 
    cfg <- (EitherT . liftM (maybeToEither "getConfig")) (getConfig "config1.txt")
    let priv = evgen_privatekeyfile cfg 
        pass = evgen_passwordstore cfg 
        wdavroot = evgen_webdavroot cfg 
    cr <- (EitherT . liftM (maybeToEither "getCredential")) (getCredential priv pass)
    let wdavcfg = WebDAVConfig cr wdavroot 
        wdavrdir = WebDAVRemoteDir "montecarlo/admproject/smbkg/wp0123" 
        bnames = map (\x -> "SM_wp0123j_LHC7ATLAS_MLM_DefCut_AntiKT0.4_NoTau_Set" ++ show x) [1..100] -- [1..4500]
    mapM (\nm -> getXSecNCount wdavcfg wdavrdir nm >>= getJSONFileAndUpload wdavcfg wdavrdir nm) bnames 
  print r


getJSONFileAndUpload :: WebDAVConfig -> WebDAVRemoteDir -> String -> Maybe CrossSectionAndCount 
                     -> EitherT String IO ()  
getJSONFileAndUpload wdavcfg wdavrdir basename mr =
 case mr of 
   Nothing -> return ()
   Just r -> do 
     let bstr = encodePretty r
         fn = basename ++ "_total_count.json"
     liftIO $ LB.writeFile fn bstr 
     liftIO $ uploadFile wdavcfg wdavrdir fn 
     liftIO $ removeFile fn 

getXSecNCount :: WebDAVConfig -> WebDAVRemoteDir -> String -> EitherT String IO (Maybe CrossSectionAndCount)
getXSecNCount wdavcfg wdavrdir bname = do 
    liftIO $ print bname 
    runMaybeT $ do 
      x <- MaybeT $ xsec wdavcfg wdavrdir bname
      c <- MaybeT $ count wdavcfg wdavrdir bname 
      return (CrossSectionAndCount x c)




-- | get cross section in pb unit                           
xsec :: WebDAVConfig -> WebDAVRemoteDir -> String -> EitherT String IO (Maybe Double) 
xsec wdavcfg wdavrdir bname = do  
    let fp = bname ++ "_pythia.log"
    b <- (liftIO (doesFileExistInDAV wdavcfg wdavrdir fp))
    if b 
      then do 
        liftIO ( downloadFile wdavcfg wdavrdir fp ) 
        eval <- liftIO $ getXSecFromPythiaLog fp
        case eval of
          Left _ -> return Nothing 
          Right val -> do       
            liftIO ( removeFile fp )
            return (Just val)
      else return Nothing


-- | get number of events
count :: WebDAVConfig -> WebDAVRemoteDir -> String -> EitherT String IO (Maybe Int)
count wdavcfg wdavrdir bname = do  
    let fp = bname ++ "_pgs_events.lhco.gz"
    boolToMaybeM (liftIO (doesFileExistInDAV wdavcfg wdavrdir fp)) $ do 
      liftIO ( downloadFile wdavcfg wdavrdir fp ) 
      bstr <- liftIO $ LB.readFile fp 
      let unzipped =decompress bstr 
          evts = parsestr unzipped 
          totnum = length evts 
      liftIO $ removeFile fp 
      return totnum 

 


