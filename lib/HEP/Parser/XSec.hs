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

p_xsec :: Parser Double 
p_xsec = do 
  manyTill anyChar (try (char '#'))
  (try (do string "  Integrated weight (pb)  :" 
           skipSpace
           str <- manyTill anyChar (char '\n')
           return (read ('0':str))
       )

   <|> p_xsec )

getXSecFromLHEGZ :: FilePath -> IO (Either String Double) 
getXSecFromLHEGZ fp = do 
  bstr <- LB.readFile fp
  let bstr' = decompress bstr 
  return $ (parseOnly p_xsec . B.concat . LB.toChunks) bstr' 



