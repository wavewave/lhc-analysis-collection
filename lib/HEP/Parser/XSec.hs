{-# LANGUAGE OverloadedStrings #-}

-----------------------------------------------------------------------------
-- |
-- Module      : HEP.Parser.XSec
-- Copyright   : (c) 2013 Ian-Woo Kim
--
-- License     : GPL-3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-----------------------------------------------------------------------------

module HEP.Parser.XSec where

import Codec.Compression.GZip
import Control.Applicative
import Data.Attoparsec.Char8
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as LB

p_xsec_lhe :: Parser Double 
p_xsec_lhe = do 
  manyTill anyChar (try (char '#'))
  (try (do string "  Integrated weight (pb)  :" 
           skipSpace
           str <- manyTill anyChar (char '\n')
           let (x:xs) = str
           if x == '-' then return (read (x:'0':xs)) else return (read ('0':str))
       )
 
   <|> p_xsec_lhe )

p_xsec_pythialog :: Parser Double 
p_xsec_pythialog = do 
    manyTill anyChar (try (string "Cross section (pb):"))
    skipSpace 
    str <- manyTill anyChar (try (char ' '))
    return (read str)


getXSecFromLHEGZ :: FilePath -> IO (Either String Double) 
getXSecFromLHEGZ fp = do 
  bstr <- LB.readFile fp
  let bstr' = decompress bstr 
  return $ (parseOnly p_xsec_lhe . B.concat . LB.toChunks) bstr' 


getXSecFromPythiaLog :: FilePath -> IO (Either String Double) 
getXSecFromPythiaLog fp = do 
    bstr <- LB.readFile fp 
    return $ (parseOnly p_xsec_pythialog . B.concat . LB.toChunks ) bstr


