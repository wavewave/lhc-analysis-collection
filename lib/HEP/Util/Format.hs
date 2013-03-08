{-# LANGUAGE StandaloneDeriving #-}

-----------------------------------------------------------------------------
-- |
-- Module      : HEP.Util.Format
-- Copyright   : (c) 2013 Ian-Woo Kim
--
-- License     : GPL-3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-----------------------------------------------------------------------------

module HEP.Util.Format where

import           Data.Text.Format 
import qualified Data.Text.Lazy as T
import           Data.Text.Lazy.Builder 

log10 x = log x / log 10

getExponent10 x = floor (log10 x)

getBody10 x = 10**(log10 x - fromIntegral (getExponent10 x))

sciformat :: Int -> Maybe Double -> String 
sciformat n (Just x) = 
  let e = getExponent10 x
      b = getBody10 x 
      -- trunced = (fromIntegral (floor (b*100)) / 100.0) * (10.0**fromIntegral e) 
  in if e `elem` [-n,-n+1..n]
     then (T.unpack . toLazyText . fixed (n+(-e))) x
     else "$" ++ ((T.unpack . toLazyText . fixed n . getBody10) x) ++ "\\times 10^{" 
                      ++ (show e) ++ "}$" 
sciformat n (Nothing) = "0"


sciformat_ = sciformat 2