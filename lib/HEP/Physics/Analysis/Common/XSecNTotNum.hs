{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE DeriveDataTypeable #-}

-----------------------------------------------------------------------------
-- |
-- Module      : HEP.Physics.Analysis.Common.XSecNTotNum 
-- Copyright   : (c) 2013 Ian-Woo Kim
--
-- License     : GPL-3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-- Get cross section and total number from ME/PS matched events
-- 
-----------------------------------------------------------------------------

module HEP.Physics.Analysis.Common.XSecNTotNum where

import           Codec.Compression.GZip 
import           Control.Monad.Trans
import           Control.Monad.Trans.Maybe
import           Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.Aeson.Generic as G
import           Data.Aeson.Types
import qualified Data.ByteString.Lazy.Char8 as LB
import           Data.Data
import           System.Directory  
-- 
import HEP.Parser.LHCOAnalysis.Parse
import HEP.Storage.WebDAV.CURL 
import HEP.Storage.WebDAV.Type 
import HEP.Util.Either 
-- 
import HEP.Parser.XSec

data CrossSectionAndCount = CrossSectionAndCount { crossSectionInPb :: Double
                                                 , numberOfEvent :: Int } 
    deriving (Show,Eq,Data,Typeable)
                           
instance ToJSON CrossSectionAndCount where
  toJSON = G.toJSON

getJSONFileAndUpload :: WebDAVConfig -> WebDAVRemoteDir -> String -> Maybe CrossSectionAndCount -> IO ()  
getJSONFileAndUpload wdavcfg wdavrdir basename mr =
 case mr of 
   Nothing -> return ()
   Just r -> do 
     let bstr = encodePretty r
         fn = basename ++ "_total_count.json"
     LB.writeFile fn bstr 
     uploadFile wdavcfg wdavrdir fn 
     removeFile fn 
     return ()

getXSecNCount :: WebDAVConfig -> WebDAVRemoteDir -> String -> IO (Maybe CrossSectionAndCount)
getXSecNCount wdavcfg wdavrdir bname = do 
    print bname 
    runMaybeT $ do 
      x <- MaybeT $ xsec wdavcfg wdavrdir bname
      c <- MaybeT $ count wdavcfg wdavrdir bname 
      return (CrossSectionAndCount x c)




-- | get cross section in pb unit                           
xsec :: WebDAVConfig -> WebDAVRemoteDir -> String -> IO (Maybe Double) 
xsec wdavcfg wdavrdir bname = do  
    let fp = bname ++ "_pythia.log"
    b <- doesFileExistInDAV wdavcfg wdavrdir fp
    if b 
      then do 
        downloadFile False wdavcfg wdavrdir fp  
        eval <- getXSecFromPythiaLog fp
        case eval of
          Left _ -> return Nothing 
          Right val -> do       
            removeFile fp 
            return (Just val)
      else return Nothing


-- | get number of events
count :: WebDAVConfig -> WebDAVRemoteDir -> String -> IO (Maybe Int)
count wdavcfg wdavrdir bname = do  
    let fp = bname ++ "_pgs_events.lhco.gz"
    boolToMaybeM (doesFileExistInDAV wdavcfg wdavrdir fp ) $ do 
      downloadFile False wdavcfg wdavrdir fp  
      bstr <- LB.readFile fp 
      let unzipped =decompress bstr 
          evts = parsestr unzipped 
          totnum = length evts 
      removeFile fp 
      return totnum 

 


