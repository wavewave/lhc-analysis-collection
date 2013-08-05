{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString.Char8 as B

import Control.Applicative
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Either
-- import Data.Attoparsec.Char8
-- import Data.Attoparsec.Combinator
import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.Aeson.Generic as G 
import Data.Aeson.Types
import qualified Data.ByteString.Lazy.Char8 as LB
import qualified Data.Configurator as C
import Data.Data 
import Data.List.Split
import qualified Data.Map as M
import System.Console.CmdArgs
import System.Directory
import System.FilePath
import System.IO 
import System.Process
import Text.Printf
import Text.StringTemplate
import Text.StringTemplate.Helpers
-- 
import qualified HEP.Automation.EventGeneration.Config as EC
import HEP.Storage.WebDAV.CURL
import HEP.Storage.WebDAV.Type
import HEP.Util.Either
-- 
import Paths_lhc_analysis_collection

-------------------------
-- config data defined -- 
-------------------------

data Config = Config { prospinoBaseDir :: FilePath 
                     , prospinoResultFileName :: String 
                     , prospinoRemoteDir :: FilePath 
                     } 

getConfig :: FilePath -> IO (Maybe Config)
getConfig conf = do 
  cfg <- C.load [C.Required conf] 
  mbasedir <- C.lookup cfg "prospino.basedir" 
  mfilename <- C.lookup cfg "prospino.resultFileName"
  mremotedir <- C.lookup cfg "prospino.remoteDir" 
  return (Config <$> mbasedir <*> mfilename <*> mremotedir)


-------------
-- command --
-------------

data RunProspino = Install { installdir :: FilePath }
                 | GluinoPair { evgencfg :: FilePath 
                              , squark :: Double
                              , gluino :: Double
                              , config :: FilePath } 
                 | SquarkGluino { evgencfg :: FilePath 
                                , squark :: Double
                                , gluino :: Double 
                                , config :: FilePath }
                 | SquarkPair { evgencfg :: FilePath
                              , squark :: Double
                              , gluino :: Double 
                              , config :: FilePath }
                 | SquarkAntisquark { evgencfg :: FilePath
                                    , squark :: Double
                                    , gluino :: Double 
                                    , config :: FilePath 
                                    } 
                 deriving (Show, Data, Typeable)

install = Install { installdir = def &= typ "INSTALLDIR" &= argPos 0 }

gluinopair = GluinoPair { evgencfg = def &= typ "EVGENCFG" &= argPos 0
                        , squark = def &= typ "MSQUARK" &= argPos 1
                        , gluino = def &= typ "MGLUINO" &= argPos 2
                        , config = "runprospino.conf" }

squarkgluino = SquarkGluino { evgencfg = def &= typ "EVGENCFG" &= argPos 1
                            , squark = def &= typ "MSQUARK" &= argPos 2
                            , gluino = def &= typ "MGLUINO" &= argPos 3
                            , config = "runprospino.conf" }

squarkpair = SquarkPair { evgencfg = def &= typ "EVGENCFG" &= argPos 0
                        , squark = def &= typ "MSQUARK" &= argPos 1
                        , gluino = def &= typ "MGLUINO" &= argPos 2
                        , config = "runprospino.conf" }

squarkantisquark = SquarkAntisquark { evgencfg = def &= typ "EVGENCFG" &= argPos 0
                                    , squark = def &= typ "MSQUARK" &= argPos 1
                                    , gluino = def &= typ "MGLUINO" &= argPos 2
                                    , config = "runprospino.conf" }


mode = modes [ install, gluinopair, squarkgluino, squarkpair, squarkantisquark ] 

----------------------------------------------
-- data structure needed for defining a job --
----------------------------------------------

data ProcessType = Gluino_Gluino | Squark_Gluino | Squark_Squark | Squark_Antisquark
                 deriving (Show,Eq,Ord)


data MSSMParam = MSSMParam { msquark :: Double 
                           , msbottom  :: Double
                           , mstop :: Double 
                           , mslepton :: Double 
                           , mstau :: Double 
                           , mgluino :: Double
                           , mneutralino :: Double 
                           , mneuttwo :: Double
                           , mchargino :: Double
                           , mcharginotwo :: Double }

data CrossSectionResult = CrossSectionResult { xsecRefLO :: Double 
                                             , xsecRefNLO :: Double
                                             , xsecRefMultiSquarkLO :: Double
                                             , xsecRefMultiSquarkNLO :: Double           
                                             , xsecKFactor :: Double 
                                             }
                        deriving (Show, Eq, Data, Typeable)

instance ToJSON CrossSectionResult where
  toJSON = G.toJSON 

-------------------
-- job processes -- 
-------------------

makeProspinoIn :: MSSMParam -> IO String
makeProspinoIn MSSMParam {..} = do 
  putStrLn "making prospino.in.les_houches"
  tpath <- (</> "template") <$> getDataDir 

  templates <- directoryGroup tpath
  return $ renderTemplateGroup templates 
             [ ("msquark"     , printf "%.4e" msquark      :: String ) 
             , ("msbottom"    , printf "%.4e" msbottom     :: String )
             , ("mstop"       , printf "%.4e" mstop        :: String )
             , ("mslepton"    , printf "%.4e" mslepton     :: String )
             , ("mstau"       , printf "%.4e" mstau        :: String ) 
             , ("mgluino"     , printf "%.4e" mgluino      :: String )
             , ("mneutralino" , printf "%.4e" mneutralino  :: String )
             , ("mneuttwo"    , printf "%.4e" mneuttwo     :: String )
             , ("mchargino"   , printf "%.4e" mchargino    :: String ) 
             , ("mcharginotwo", printf "%.4e" mcharginotwo :: String )
             ] 
             "prospino.in.les_houches"   
             ++ "\n"


makeProspinoMain :: ProcessType -> IO String
makeProspinoMain proc = do 
  putStrLn "make prospino_main.f90"
  tpath <- (</> "template") <$> getDataDir 
  templates <- directoryGroup tpath
  let procstr :: String = case proc of 
        Gluino_Gluino -> "gg" 
        Squark_Gluino -> "sg"
        Squark_Squark -> "ss" 
        Squark_Antisquark -> "sb" 

  return $ renderTemplateGroup templates 
             [ ( "process", procstr) ] 
             "prospino_main.f90"  

elimLastDot :: String -> String 
elimLastDot str = if last str == '.' then init str else str 

readProspinoData :: IO LB.ByteString
readProspinoData = do 
  str <- readFile "prospino.dat"
  let xs = (filter (not.null) . splitOn " " . head . lines) str 
      proc :: String = xs !! 0
      i1 :: Int = read (xs !! 1)
      i2 :: Int = read (xs !! 2)
      dummy0 :: Double = (read . elimLastDot) (xs !! 3)
      dummy1 :: Double = (read . elimLastDot) (xs !! 4)
      scafac :: Double = (read . elimLastDot) (xs !! 5)
      m1 :: Double     = (read . elimLastDot) (xs !! 6)
      m2 :: Double     = (read . elimLastDot) (xs !! 7)
      angle :: Double  = (read . elimLastDot) (xs !! 8)
      xsecLO :: Double = (read . elimLastDot) (xs !! 9) 
      relErrorLO :: Double = (read . elimLastDot) (xs !! 10)
      xsecNLO :: Double = (read . elimLastDot) (xs !! 11)
      relErrorNLO :: Double = (read . elimLastDot) (xs !! 12)
      kFactor :: Double = (read . elimLastDot) (xs !! 13)
      xsecmsLO :: Double = (read . elimLastDot) (xs !! 14)
      xsecmsNLO :: Double = (read . elimLastDot) (xs !! 15)
  -- print xs
  -- print (proc,i1,i2,dummy0,dummy1,scafac,m1,m2)
  -- print (angle,xsecLO,relErrorLO,xsecNLO,relErrorNLO,kFactor,xsecmsLO,xsecmsNLO) 
  let result = CrossSectionResult { xsecRefLO = xsecLO
                                  , xsecRefNLO = xsecNLO
                                  , xsecRefMultiSquarkLO = xsecmsLO
                                  , xsecRefMultiSquarkNLO = xsecmsNLO
                                  , xsecKFactor = kFactor 
                                  }
      bstr = encodePretty result
  return bstr 
   
minfty = 50000

testMSSMParam :: (Double,Double) -> MSSMParam 
testMSSMParam (q,g)
              = MSSMParam { msquark      = q
                          , msbottom     = minfty
                          , mstop        = minfty
                          , mslepton     = minfty
                          , mstau        = minfty
                          , mgluino      = g
                          , mneutralino  = minfty
                          , mneuttwo     = minfty
                          , mchargino    = minfty
                          , mcharginotwo = minfty }

testMSSMParam2 :: (Double,Double) -> MSSMParam 
testMSSMParam2 (q,g)
              = MSSMParam { msquark      = q
                          , msbottom     = q
                          , mstop        = q
                          , mslepton     = minfty
                          , mstau        = minfty
                          , mgluino      = g
                          , mneutralino  = minfty
                          , mneuttwo     = minfty
                          , mchargino    = minfty
                          , mcharginotwo = minfty }


-- prospinodir = "/home2/iankim/repo/src/lhc-analysis-collection/template/prospino_2_1"

-- basedir = "/home2/iankim/repo/src/lhc-analysis-collection/analysis"

installProspino :: FilePath -> IO FilePath 
installProspino bdir  = do 
  -- let bdir = prospinoBaseDir cfg 
  cdir <- getCurrentDirectory 
  setCurrentDirectory bdir
  -- 
  resdir <- (</> "resource") <$> getDataDir
  readProcess "tar" ["xvzf" , resdir </> "prospino_2_1.tar.gz"] ""
  -- 
  setCurrentDirectory (bdir </> "prospino_2_1")
  system "make"
  -- 
  setCurrentDirectory cdir
  return (bdir </> "prospino_2_1")

 

run :: FilePath -> Config -> ProcessType -> (Double,Double) -> IO ()
run mccfgfile cfg' proc (q,g) = do  
  runEitherT $ do 
    cfg <- (EitherT . liftM (maybeToEither "getConfig")) (EC.getConfig mccfgfile)
    let priv = EC.evgen_privatekeyfile cfg 
        pass = EC.evgen_passwordstore cfg 
        wdavroot = EC.evgen_webdavroot cfg 
    cr <- (EitherT . liftM (maybeToEither "getCredential")) (EC.getCredential priv pass)
    let wdavcfg = WebDAVConfig cr wdavroot 
        wdavrdir = WebDAVRemoteDir (prospinoRemoteDir cfg')  

    liftIO $ do 
      cdir <- getCurrentDirectory
      let prospinodir = prospinoBaseDir cfg' </> "prospino_2_1"
      setCurrentDirectory prospinodir 
      str1 <- makeProspinoIn (case proc of 
                                Squark_Antisquark -> testMSSMParam2 (q,g)
                                _ -> testMSSMParam (q,g)
                             )
      str2 <- makeProspinoMain proc
      writeFile "prospino.in.les_houches" str1 
      writeFile "prospino_main.f90" str2
      system "make" 
      system "./prospino_2.run"
      LB.writeFile (prospinoResultFileName cfg') =<< readProspinoData 
      uploadFile wdavcfg wdavrdir (prospinoResultFileName cfg') 
      setCurrentDirectory cdir
  return ()


main :: IO () 
main = do 
  param <- cmdArgs mode 
  case param of 
    Install dir                  -> installProspino dir >> return ()
    GluinoPair evcfg q g cfile -> 
      getConfig cfile >>= (\(Just cfg) -> run evcfg cfg Gluino_Gluino (q,g))
    SquarkGluino evcfg q g cfile -> 
      getConfig cfile >>= (\(Just cfg) -> run evcfg cfg Squark_Gluino (q,g))
    SquarkPair evcfg q g cfile -> 
      getConfig cfile >>= (\(Just cfg) -> run evcfg cfg Squark_Squark (q,g))
    SquarkAntisquark evcfg q g cfile ->
      getConfig cfile >>= (\(Just cfg) -> run evcfg cfg Squark_Antisquark (q,g))



