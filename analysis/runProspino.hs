{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}

import qualified Data.ByteString.Char8 as B

import Control.Applicative
import Data.Attoparsec.Char8
import Data.Attoparsec.Combinator
import Data.List.Split
import qualified Data.Map as M
import System.Directory
import System.FilePath
import System.IO 
import System.Process
import Text.Printf
import Text.StringTemplate
import Text.StringTemplate.Helpers
-- 
import Paths_lhc_analysis_collection

-- pSLHA = many (skipWhile (/= '<')) >> (try (string "slha>") <|> pSLHA) 
{-
pSLHA = (manyTill anyChar (try (string "<slha>")) >> manyTill anyChar (string "</slha>")) <?> "pSLHA"

extractSLHA :: FilePath -> IO (Either String String)
extractSLHA fp = do 
  bstr <- B.readFile fp 
  let c = parseOnly pSLHA bstr 
  return c 
-}

{-
main :: IO ()
main = do 
  putStrLn "run prospino"
  r <- extractSLHA "test.lhe"
  case r of 
    Left err -> putStrLn err 
    Right str -> do 
      writeFile "prospino.in.les_houches" str 
      system "./prospino_2.run"
      return ()
  return ()
-}

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
  let procstr = case proc of 
                  Gluino_Gluino -> "gg"
                  Squark_Gluino -> "gs"
                  Squark_Squark -> "ss" 
                  Squark_Antisquark -> "sb" 

  return $ renderTemplateGroup templates 
             [ ( "process", procstr) ] 
             "prospino_main.f90"  

readProspinoData :: IO ()
readProspinoData = do 
  str <- readFile "prospino.dat"
  let xs = (filter (not.null) . splitOn " " . head . lines) str 
      proc :: String = xs !! 0
      i1 :: Int = read (xs !! 1)
      i2 :: Int = read (xs !! 2)
      dummy0 :: Double = read (xs !! 3)
      dummy1 :: Double = read (xs !! 4)
      scafac :: Double = read (xs !! 5)
      m1 :: Double     = read (xs !! 6)
      m2 :: Double     = read (xs !! 7)
      angle :: Double  = read (xs !! 8)
      xsecLO :: Double = read (xs !! 9) 
      relErrorLO :: Double = read (xs !! 10)
      xsecNLO :: Double = read (xs !! 11)
      relErrorNLO :: Double = read (xs !! 12)
      kFactor :: Double = read (xs !! 13)
      xsecmsLO :: Double = read (xs !! 14)
      xsecmsNLO :: Double = read (xs !! 15)
  print xs
  print (proc,i1,i2,dummy0,dummy1,scafac,m1,m2)
  print (angle,xsecLO,relErrorLO,xsecNLO,relErrorNLO,kFactor,xsecmsLO,xsecmsNLO) 
   
minfty = 50000

testMSSMParam = MSSMParam { msquark      = 1000
                          , msbottom     = minfty
                          , mstop        = minfty
                          , mslepton     = minfty
                          , mstau        = minfty
                          , mgluino      = 1000
                          , mneutralino  = minfty
                          , mneuttwo     = minfty
                          , mchargino    = minfty
                          , mcharginotwo = minfty }


-- prospinodir = "/home2/iankim/repo/src/lhc-analysis-collection/template/prospino_2_1"

basedir = "/home2/iankim/repo/src/lhc-analysis-collection/analysis"

installProspino :: FilePath -> IO FilePath 
installProspino bdir  = do 
  cdir <- getCurrentDirectory 
  setCurrentDirectory bdir
  -- 
  resdir <- (</> "resource") <$> getDataDir
  readProcess "tar" ["xvzf" , resdir </> "prospino_2_1.tar.gz"] ""
  -- 
  setCurrentDirectory cdir
  return (bdir </> "prospino_2_1")



main = do  
  cdir <- getCurrentDirectory
  prospinodir <- installProspino basedir 
  setCurrentDirectory prospinodir 
  str1 <- makeProspinoIn testMSSMParam
  str2 <- makeProspinoMain Gluino_Gluino

  writeFile "prospino.in.les_houches" str1 
  writeFile "prospino_main.f90" str2

  system "make" 
  system "./prospino_2.run"

  readProspinoData 

  setCurrentDirectory cdir
  
