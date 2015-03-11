{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Applicative
import           Control.Monad
import qualified Data.Attoparsec.Text as A
import           Data.Data
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TLIO
import           System.Directory
import           System.FilePath
import           System.Process
import           System.IO
import           Text.Hastache
import           Text.Hastache.Context
import           Text.Printf

data HiggsInput = HiggsInput { higgsTanbeta :: Int  
                             , higgsMA      :: Int
                             , higgsLumi    :: Int

                             }
                deriving (Data,Typeable)

mkVarIn :: HiggsInput -> IO TL.Text
mkVarIn input = hastacheFile defaultConfig "var.in.hastache" (mkGenericContext input)

work :: HiggsInput -> IO (Either String HiggsResult)
work input = do
    cwd <- getCurrentDirectory
    txt <- mkVarIn input
    tmpdir <- getTemporaryDirectory
    let ifile = tmpdir </> "var.in"
    setCurrentDirectory tmpdir 
    TLIO.writeFile ifile txt
    txt <- T.pack <$> readProcess "FeynHiggs" [ifile] ""
    let r = A.parseOnly higgsResult txt
    setCurrentDirectory cwd
    return r

data HiggsResult = HiggsResult
                   { resultHH :: BRXSec
                   , resultA0 :: BRXSec
                   } deriving Show

data BRXSec = BRXSec { branchRatio :: Double
                     , xsection :: Double
		     } deriving Show
		     
dummyBRXS = BRXSec 0 0

dummyResult = HiggsResult dummyBRXS dummyBRXS

higgsResult :: A.Parser HiggsResult
higgsResult = do brHbb <- branchRatio_HH_b_b  -- t_t
                 brAbb <- branchRatio_A0_b_b  -- t_t 
                 xsttH <- xsec_t_t_HH         -- b_b
		 xsttA <- xsec_t_t_A0         -- b_b
                 return (HiggsResult (BRXSec brHbb xsttH) (BRXSec brAbb xsttA))

{- 
branchRatio_HH_t_t :: A.Parser Double
branchRatio_HH_t_t = do
    A.manyTill A.anyChar (A.try (A.string "%| HH-t-t"))
    A.skipSpace
    A.char '='
    A.skipSpace
    A.double
    A.skipSpace
    A.double

branchRatio_A0_t_t :: A.Parser Double
branchRatio_A0_t_t = do
    A.manyTill A.anyChar (A.try (A.string "%| A0-t-t"))
    A.skipSpace
    A.char '='
    A.skipSpace
    A.double
    A.skipSpace
    A.double
-}

branchRatio_HH_b_b :: A.Parser Double
branchRatio_HH_b_b = do
    A.manyTill A.anyChar (A.try (A.string "%| HH-b-b"))
    A.skipSpace
    A.char '='
    A.skipSpace
    A.double
    A.skipSpace
    A.double

branchRatio_A0_b_b :: A.Parser Double
branchRatio_A0_b_b = do
    A.manyTill A.anyChar (A.try (A.string "%| A0-b-b"))
    A.skipSpace
    A.char '='
    A.skipSpace
    A.double
    A.skipSpace
    A.double


xsec_t_t_HH :: A.Parser Double
xsec_t_t_HH = do
    A.manyTill A.anyChar (A.try (A.string "| prod:t-t-HH"))
    A.skipSpace
    A.char '='
    A.skipSpace
    A.double

xsec_t_t_A0 :: A.Parser Double
xsec_t_t_A0 = do
    A.manyTill A.anyChar (A.try (A.string "| prod:t-t-A0"))
    A.skipSpace
    A.char '='
    A.skipSpace
    A.double

{- 
xsec_b_b_HH :: A.Parser Double
xsec_b_b_HH = do
    A.manyTill A.anyChar (A.try (A.string "| prod:b-b-HH"))
    A.skipSpace
    A.char '='
    A.skipSpace
    A.double

xsec_b_b_A0 :: A.Parser Double
xsec_b_b_A0 = do
    A.manyTill A.anyChar (A.try (A.string "| prod:b-b-A0"))
    A.skipSpace
    A.char '='
    A.skipSpace
    A.double
-}

    
testinput = HiggsInput { higgsTanbeta = 1
                       , higgsMA = 500
                       , higgsLumi = 100 }

scansets= [ HiggsInput tb ma 100 | ma <- [400,450,500,550,600,650,700,750,800,850,900,950,1000],
                                   tb <- [1..50] ]

scanpoint input = do
  eresult <- work input
  case eresult of
    Left err -> return (format input dummyResult)
    Right result -> return (format input result)


format input result = 
    printf "  %4d  %4d  %.6f  %.6f  %.6f  %.6f  %.6f  %.6f  %.6f  " (higgsMA input) (higgsTanbeta input) brH xsecH brA xsecA brxsecH brxsecA brxsectot
  where brH       = branchRatio (resultHH result)
        xsecH     = xsection (resultHH result)
        brA       = branchRatio (resultA0 result)
        xsecA     = xsection (resultA0 result)
        brxsecH   = ((*)<$>branchRatio<*>xsection) (resultHH result)
        brxsecA   = ((*)<$>branchRatio<*>xsection) (resultA0 result)
        brxsectot = brxsecH + brxsecA


        -- return (brxsecH, brxsecA, brxsectot)

 
main = mapM_ ((\x -> putStrLn x >> hFlush stdout) <=< scanpoint) scansets

