{-# LANGUAGE StandaloneDeriving #-}

-----------------------------------------------------------------------------
-- |
-- Module      : HEP.Util.Table 
-- Copyright   : (c) 2013 Ian-Woo Kim
--
-- License     : GPL-3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-----------------------------------------------------------------------------

module HEP.Util.Table where

import Data.List (intercalate)

data Table a = Table { tableHSize :: Int 
                     , tableVSize :: Int 
                     , tableContents :: [[ Maybe a ]] } 

deriving instance (Show a) => Show (Table a) 

emptyTable :: Table a 
emptyTable = Table 0 0 [] 

singletonTable :: a -> Table a 
singletonTable a = Table 1 1 [[Just a]] 

resizeRow :: Int -> [[a]] -> [[a]] 
resizeRow n xs 
  | length xs > n = take n xs 
  | length xs == n = xs 
  | length xs < n = xs ++ replicate (n-length xs) [] 


resizeCol :: Int -> [ Maybe a ] -> [ Maybe a ] 
resizeCol n ys 
  | length ys > n = take n ys
  | length ys == n = ys
  | length ys < n = ys ++ replicate (n-length ys) Nothing 


(<->) :: Table a -> Table a -> Table a
a <-> b = let (a'_c,b'_c,v) | a_v > b_v = (a_c,resizeRow a_v b_c,a_v) 
                            | a_v == b_v = (a_c,b_c,a_v) 
                            | a_v < b_v = (resizeRow b_v a_c,b_c,b_v)
              c = [ x++y | x <- a'_c , y <- b'_c ] 
          in Table (a_h+b_h) v c
  where a_h = tableHSize a  
        a_v = tableVSize a
        a_c = tableContents a 
        b_h = tableHSize b
        b_v = tableVSize b 
        b_c = tableContents b 
        

(<|>) :: Table a -> Table a -> Table a
a <|> b = let (a'_c,b'_c,h) | a_h > b_h = (a_c,map (resizeCol a_h) b_c,a_h) 
                            | a_h == b_h = (a_c,b_c,a_h) 
                            | a_h < b_h = (map (resizeCol b_h) a_c,b_c,b_h)
          in Table h (a_v+b_v) (a'_c++b'_c)
  where a_h = tableHSize a 
        a_v = tableVSize a
        a_c = tableContents a
        b_h = tableHSize b
        b_v = tableVSize b 
        b_c = tableContents b 



class LaTeXable a where 
  showLaTeX :: a -> String 

instance LaTeXable Int where 
  showLaTeX n = show n 

instance LaTeXable a => LaTeXable (Maybe a) where 
  showLaTeX Nothing = "" 
  showLaTeX (Just x) = showLaTeX x

instance LaTeXable a => LaTeXable (Table a) where
  showLaTeX (Table _ _ c) = 
    (intercalate "\\\\\n" . map (intercalate " & " . map showLaTeX)) c 

