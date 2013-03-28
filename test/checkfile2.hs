{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}

import Control.Applicative ((<$>),(<*>),pure)
import Control.Monad 
import           Control.Monad.Trans 
import           Control.Monad.Trans.Either 
-- import Data.Attoparsec.Char8
-- import Data.Attoparsec.Lazy
import qualified Data.ByteString.Char8 as B 
import Data.Maybe 
import Data.String
import Data.Typeable
-- 
import HEP.Automation.EventGeneration.Config
import HEP.Storage.WebDAV.Type 
-- import HEP.Storage.WebDAV.Util
import HEP.Util.Either 
-- 
import HEP.Physics.Analysis.ATLAS.SUSY_0L2to6J
-- import HEP.Physics.Analysis.Common.XSecNTotNum

 -- (\wdavcfg wdavrdir nm -> getXSecNCount wdavcfg wdavrdir nm >>= getJSONFileAndUpload wdavcfg wdavrdir nm)
main = do 
  r <- work atlas7TeV0L2to6JCount
         "config1.txt" 
         -- "montecarlo/admproject/smbkg/tt012" 
         "./"
         "SM_tt012j_LHC7ATLAS_MLM_DefCut_AntiKT0.4_NoTau_Set" 
         [1..10] 
          -- [21..30]
  case r of 
    Left err -> putStrLn err 
    Right vs -> mapM_ print vs


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
