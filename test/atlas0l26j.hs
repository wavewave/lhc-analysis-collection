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

data EType = AT | AM | A'M | BT | CT | CM | CL | DT | ET | EM | EL 
             deriving (Show,Eq,Ord)

srFlag2Num :: SRFlag -> [EType] 
srFlag2Num SRFlag {..} = maybe [] fE sr_classE 
                         ++ maybe [] fD sr_classD 
                         ++ maybe [] fC sr_classC 
                         ++ maybe [] fB sr_classB 
                         ++ maybe [] fA' sr_classA'
                         ++ maybe [] fA sr_classA 

fA x | isTightEv x = [AT,AM] 
     | isMediumEv x  = [AM] 
     | otherwise = []

fA' x | isMediumEv x = [A'M]
      | otherwise = [] 
  
fE x | isTightEv x = [ET,EM,EL] 
     | isMediumEv x = [EM,EL] 
     | isLooseEv x= [EL] 
     | otherwise = [] 

fD x | isTightEv x = [DT] 
     | otherwise = [] 

fC x | isTightEv x = [CT,CM,CL] 
     | isMediumEv x = [CM,CL] 
     | isLooseEv x= [CL] 
     | otherwise = [] 


fB x | isTightEv x = [BT] 
     | otherwise = [] 


showAsATLASPaper :: M.Map EType Double -> String
showAsATLASPaper m = 
  "CL :" ++ maybe "0" show (M.lookup CL m)      ++ "\n"
  ++ "EL : " ++ maybe "0" show (M.lookup EL m) ++ "\n"
  ++ "AM : " ++ maybe "0" show (M.lookup AM m) ++ "\n"
  ++ "A'M : " ++ maybe "0" show (M.lookup A'M m) ++ "\n"
  ++ "CM : " ++ maybe "0" show (M.lookup CM m) ++ "\n"
  ++ "EM : " ++ maybe "0" show (M.lookup EM m) ++ "\n"
  ++ "AT : " ++ maybe "0" show (M.lookup AT m) ++ "\n"
  ++ "BT : " ++ maybe "0" show (M.lookup BT m)++ "\n"
  ++ "CT : " ++ maybe "0" show (M.lookup CT m) ++ "\n" 
  ++ "DT : " ++ maybe "0" show (M.lookup DT m) ++ "\n"
  ++ "ET : " ++ maybe "0" show (M.lookup ET m) ++ "\n"


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
  
