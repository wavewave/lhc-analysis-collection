{-# LANGUAGE OverloadedStrings #-}

module Main where 

import Codec.Compression.GZip
import Control.Applicative
import Control.Monad 
-- import Data.Attoparsec.Combinator 
import Data.Attoparsec.Char8
-- import qualified Data.Attoparsec.Lazy as AL  
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as LB
import System.Environment (getArgs)

p_xsec :: Parser String
p_xsec = do 
  manyTill anyChar (try (char '#'))
  (try (string "  Integrated weight (pb)  :" >> manyTill anyChar (char '\n') >>= \str -> return str)
   <|> p_xsec )

-- (char '#' >> p_xsec))

main :: IO () 
main = do 
  putStrLn "simple cross section reading"
  args <- getArgs 
  when (length args /= 1) $ error "crosssec filename"
  let filename = args !! 0 
  bstr <- LB.readFile filename 

  let bstr' = decompress bstr 

      r = (parseOnly p_xsec . B.concat . LB.toChunks) bstr' 

  print r
  -- LB.putStrLn (LB.take 1000 bstr') 

  
    
