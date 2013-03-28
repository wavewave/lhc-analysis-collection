{-# LANGUAGE RecordWildCards #-}

module Main where

import           Control.Applicative
import           Codec.Compression.GZip 
import qualified Data.ByteString.Lazy.Char8 as LB
import           Data.Maybe 
import qualified Data.Map.Strict as M
-- 
import           HEP.Parser.LHCOAnalysis.Parse
-- 
import           HEP.Physics.Analysis.ATLAS.SUSY_0L2to6J
-- 


main = do 
  xs <- mapM go [1..10]
  let lst = (concat . map srFlag2Num . concatMap snd) xs 
      lst2 = map (\x->(x,1)) lst 
      lst3 = foldr (\(k,v) m->M.insertWith (+) k v m) M.empty lst2 
  -- (print . sum . map fst) xs 
  putStr (showAsATLASPaper . fmap ((*1.979) . fromIntegral )  $ lst3)
  -- print . foldr (\k -> M.insertWith (+) k  ) empty  . map snd $ xs 
  

go :: Int ->  IO (Int,[SRFlag]) 
go n = do 

  let fn = "SM_tt012j_LHC7ATLAS_MLM_DefCut_AntiKT0.4_NoTau_Set" ++ show n 
            ++ "_pgs_events.lhco.gz" 
  bstr <- LB.readFile fn 
  let unzipped = decompress bstr 
      evts = parsestr unzipped 
      triggeredevts = (catMaybes . map analysis) evts 
  
      totnum = length evts  
       
  -- print (length evts )
  -- print (length triggeredevts) 
  -- print triggeredevts 
  return (totnum,triggeredevts) 
  
