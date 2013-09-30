-----------------------------------------------------------------------------
-- |
-- Module      : HEP.Util.Work
-- Copyright   : (c) 2013 Ian-Woo Kim
--
-- License     : GPL-3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-- 
-- 
-----------------------------------------------------------------------------

module HEP.Util.Work where 

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Either
import Control.Monad.Trans.Reader
-- 
import HEP.Automation.EventGeneration.Config
import HEP.Storage.WebDAV.Type 
import HEP.Util.Either

-- | 
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


-- | 
singleWork :: (WebDAVConfig -> WebDAVRemoteDir -> String -> EitherT String IO a) 
           -> FilePath 
           -> FilePath 
           -> FilePath 
           -> Int 
           -> EitherT String IO a 
singleWork task cfgfile rdir bname set = do 
    cfg <- (EitherT . liftM (maybeToEither "getConfig")) (getConfig cfgfile)
    let priv = evgen_privatekeyfile cfg 
        pass = evgen_passwordstore cfg 
        wdavroot = evgen_webdavroot cfg 
    cr <- (EitherT . liftM (maybeToEither "getCredential")) (getCredential priv pass)
    let wdavcfg = WebDAVConfig cr wdavroot 
        wdavrdir = WebDAVRemoteDir rdir 
        b = bname ++ show set 
    task wdavcfg wdavrdir b 



-- | 
fileWork :: (WebDAVConfig -> WebDAVRemoteDir -> String -> EitherT String IO a) 
     -> FilePath 
     -> FilePath 
     -> FilePath 
     -> [Int] 
     -> EitherT String IO [a]
fileWork task cfgfile rdir bname sets = do 
    cfg <- (EitherT . liftM (maybeToEither "getConfig")) (getConfig cfgfile)
    let priv = evgen_privatekeyfile cfg 
        pass = evgen_passwordstore cfg 
        wdavroot = evgen_webdavroot cfg 
    cr <- (EitherT . liftM (maybeToEither "getCredential")) (getCredential priv pass)
    let wdavcfg = WebDAVConfig cr wdavroot 
        wdavrdir = WebDAVRemoteDir rdir 
        bnames = map (\x -> bname ++ show x) sets
    mapM (task wdavcfg wdavrdir) bnames 


-- | 
withDAVConfig :: FilePath -> EitherT String (ReaderT WebDAVConfig IO) a -> IO (Either String a) 
withDAVConfig cfgfile task = do 
    mcfg <- getConfig cfgfile 
    case mcfg of 
      Nothing -> return (Left "Error in getConfig") 
      Just cfg -> do 
        let priv = evgen_privatekeyfile cfg 
            pass = evgen_passwordstore cfg 
            wdavroot = evgen_webdavroot cfg 
        mcr <- getCredential priv pass
        case mcr of 
          Nothing -> return (Left "Error in getCredential") 
          Just cr -> do 
            let wdavcfg = WebDAVConfig cr wdavroot 
            runReaderT (runEitherT task) wdavcfg 


