module Main where

import           Control.Applicative
import           Data.List.Split
import qualified Data.HashMap.Strict as HM
import           Text.Printf


data EffXsec = EffXsec { mass :: Int
                       , tanb :: Int
                       , xsec :: Double }
             deriving (Show)

sigfile n = "HeavyHiggsMHH"++show n++".0_fourtop_LHC14ATLAS_NoMatch_DefCut_Cone0.4_WithTau_cut_count.dat"

bkgfile = "optcutlepttbar.dat"

parseline :: [String] -> (Int,Int,Double,Double,Double,Double,Double,Double,Double)
parseline [a,b,c,d,e,f,g,h,i] = (read a,read b,read c,read d,read e,read f,read g,read h,read i)
parseline _ = error "parseline"

effXsec :: (Int,Int,Double,Double,Double,Double,Double,Double,Double) -> EffXsec
effXsec (a,b,c,d,e,f,g,h,i) = EffXsec a b i


parseOptOne :: [String] -> (Double,Double,Double,Double,Double,Int,Int,Int,Int,Int,Int,Int,Int,Int)
parseOptOne (x1:x2:x3:x4:x5:y1:y2:y3:y4:y5:y6:y7:y8:y9:[]) = 
 (read x1,read x2,read x3,read x4,read x5,read y1,read y2,read y3,read y4,read y5,read y6,read y7,read y8,read y9)

integerize :: Double -> Int
integerize = round



normalize (xsec,lum) (x1,x2,x3,x4,x5,y1,y2,y3,y4,y5,y6,y7,y8,y9) = 
    (x1,x2,x3,x4,x5
    ,n
    ,fromIntegral y2*n/fromIntegral y1
    ,fromIntegral y3*n/fromIntegral y1
    ,fromIntegral y4*n/fromIntegral y1
    ,fromIntegral y5*n/fromIntegral y1
    ,fromIntegral y6*n/fromIntegral y1
    ,fromIntegral y7*n/fromIntegral y1
    ,fromIntegral y8*n/fromIntegral y1
    ,fromIntegral y9*n/fromIntegral y1)
  where n = xsec*lum

brief (x1,x2,x3,x4,x5,y1,y2,y3,y4,y5,y6,y7,y8,y9) = (integerize x1,integerize x2,integerize x3,integerize x4,integerize x5,y7,y8,y9)




bkg :: Double -> Double -> IO [(Int,Int,Int,Int,Int,Double,Double,Double)]
bkg lum xsec = do
    str <- readFile bkgfile
    let ls = lines str
        rs = map (brief . normalize (xsec,lum) . adjust . parseOptOne . words) ls
    return rs
    -- print $ concatMap getcutnum rs

adjust (x1,x2,x3,x4,x5,y1,y2,y3,y4,y5,y6,y7,y8,y9) = (x1,x2,x3,x4,x5,adj y1,adj y2,adj y3,adj y4,adj y5,adj y6,adj y7,adj y8,adj y9)
  where adj n | n == 0 = 1
              | otherwise = n


tupling :: (Int,Int,Int,Int,Int,Double,Double,Double) -> ((Int,Int,Int,Int,Int),(Double,Double,Double))
tupling (x1,x2,x3,x4,x5,y7,y8,y9) = ((x1,x2,x3,x4,x5),(y7,y8,y9))

joinTable :: [(Int,Int,Int,Int,Int,Double,Double,Double)] -> [(Int,Int,Int,Int,Int,Double,Double,Double)] 
          -> [((Int,Int,Int,Int,Int),((Double,Double,Double),(Double,Double,Double)))]
joinTable tbl1 tbl2 = map f lst2
  where 
    lst1 = map tupling tbl1 
    lst2 = map tupling tbl2
    map1 = HM.fromList lst1
    f (k,v) = case HM.lookup k map1 of
                Nothing -> error "cannot"
                Just v0 -> (k,(v0,v))


sigoversqrtbkg ((b1,b2,b3),(s1,s2,s3)) = (sbf b1 s1, sbf b2 s2, sbf b3 s3)
  where sbf b s = s / sqrt b

maximum3 (x,y,z) = maximum [x,y,z]


work :: [(Int,Int,Int,Int,Int,Double,Double,Double)] -> Double -> EffXsec -> IO (Int,Int,Double)
work bkgtbl lum (EffXsec m tb xsec) = do 
    str <- readFile (sigfile m)
    let ls = lines str
        rs = map (brief . normalize (xsec,lum) . parseOptOne . words) ls
    let r = maximum . map maximum3 . map sigoversqrtbkg . map snd . joinTable bkgtbl $ rs
    print m 
    return (m,tb,r)
    -- print $ concatMap getcutnum rs


tupling3to2 (a,b,c) = ((a,b),c)


findrow :: Int -> HM.HashMap (Int,Int) Double -> [Double] 
findrow tanb m = let lst = [400,450..1000]
                     f x = case HM.lookup (x,tanb) m of
                             Nothing -> error "findrow"
                             Just v -> v
                 in map f lst


main' = do
  let xsecLO_ttbar = 6.50e5
      kfac_ttbar = 1.5
      lum = 1000
  bkgtbl <- bkg lum (xsecLO_ttbar*kfac_ttbar)
  str <- readFile "feynhiggs_mA_tanb_scan.dat"
  let ls = (map effXsec . map parseline . map words . lines) str 
  tbl <- HM.fromList . map tupling3to2 <$> mapM (work bkgtbl lum) ls
  print tbl

triple :: [String] -> ((Int,Int),Double)
triple [x1,x2,x3] = ((read x1,read x2),read x3)

--                tanb 400   450   500   550    600  650   700   750   800   850   900   950   1000
format tanb [x40,x45,x50,x55,x60,x65,x70,x75,x80,x85,x90,x95,x100] = 

  printf " %4d  %.6f  %.6f  %.6f  %.6f  %.6f  %.6f  %.6f  %.6f  %.6f  %.6f  %.6f  %.6f  %.6f"     tanb x40 x45 x50 x55 x60 x65 x70 x75 x80 x85 x90 x85 x100

main = do 
  str <- readFile "mA_tanb_sig.dat"
  let ls = lines str
      tbl = HM.fromList (map (triple . words) ls)
  mapM_ (\x -> putStrLn . format x $ findrow x tbl) [1..50]
  -- print $ findrow 1 tbl
  




--   mapM_ (\(ma,tb,sig) -> putStrLn (printf " %4d  %4d   %.6f" ma tb sig))



