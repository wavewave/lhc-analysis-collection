{-# LANGUAGE FlexibleInstances #-}

module Main where

import           Control.Applicative
import           Control.Lens
import           Control.Monad
import           Data.Function (on)
import qualified Data.Traversable as T
import           Data.List (sortBy)
import           Data.List.Split
import qualified Data.HashMap.Strict as HM
import           Text.Printf
import           System.IO

instance Show ((Double,Double,Double,Double,Double,Double,Int,Int,Int,Int,Int,Int,Int,Int,Int,Int)) where 
  show (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p) = show (a,b,c,d,e,f) ++ show (g,h,i,j,k,l,m,n,o,p)


data EffXsec = EffXsec { mass :: Int
                       , tanb :: Int
                       , xsec :: Double }
             deriving (Show)

sigfile0 n = "HeavyHiggsMHH"++show n++".0_2t2b_innertop_LHC14ATLAS_NoMatch_DefCut_Cone0.4_WithTau_cut_count_wdeta.dat"

sigfile1 n = "HeavyHiggsMHH"++show n++".0_2t2b_innertop_LHC14ATLAS_NoMatch_DefCut_Cone0.4_WithTau_cut_count_wdeta_add1.dat"

bkgfile0 n = "SM_tt012j_LHC14ATLAS_MLM_DefCut_AntiKT0.4_WithTau_set" ++ (show (n*1000 +1)) ++ "to" ++ (show ((n+1)*1000)) ++ "_cut_count_wdeta.dat"

bkgfile1 n = "SM_tt012j_LHC14ATLAS_MLM_DefCut_AntiKT0.4_WithTau_set" ++ (show (n*1000 +1)) ++ "to" ++ (show ((n+1)*1000)) ++ "_cut_count_wdeta_add1.dat"

parseline :: [String] -> (Int,Int,Double,Double,Double,Double,Double,Double,Double)
parseline [a,b,c,d,e,f,g,h,i] = (read a,read b,read c,read d,read e,read f,read g,read h,read i)
parseline _ = error "parseline"

effXsec :: (Int,Int,Double,Double,Double,Double,Double,Double,Double) -> EffXsec
effXsec (a,b,c,d,e,f,g,h,i) = EffXsec a b i


parseOptOne :: [String] -> (Double,Double,Double,Double,Double,Double,Int,Int,Int,Int,Int,Int,Int,Int,Int,Int)
parseOptOne (x1:x2:x3:x4:x5:x6:y1:y2:y3:y4:y5:y6:y7:y8:y9:y10:[]) = 
 (read x1,read x2,read x3,read x4,read x5,read x6,read y1,read y2,read y3,read y4,read y5,read y6,read y7,read y8,read y9,read y10)

integerize :: Double -> Int
integerize = round



normalize (xsec,lum) (x1,x2,x3,x4,x5,x6,y1,y2,y3,y4,y5,y6,y7,y8,y9,y10) = 
    (x1,x2,x3,x4,x5,x6
    ,n
    ,fromIntegral y2*n/fromIntegral y1
    ,fromIntegral y3*n/fromIntegral y1
    ,fromIntegral y4*n/fromIntegral y1
    ,fromIntegral y5*n/fromIntegral y1
    ,fromIntegral y6*n/fromIntegral y1
    ,fromIntegral y7*n/fromIntegral y1
    ,fromIntegral y8*n/fromIntegral y1
    ,fromIntegral y9*n/fromIntegral y1
    ,fromIntegral y10*n/fromIntegral y1
    )
  where n = xsec*lum


brief (x1,x2,x3,x4,x5,x6,y1,y2,y3,y4,y5,y6,y7,y8,y9,y10) = (integerize x1,integerize x2,integerize x3,integerize x4,integerize x5,x6,y7,y8,y9,y10)




bkg :: Double -> Double -> IO [(Int,Int,Int,Int,Int,Double, Double,Double,Double,Double)]
bkg lum xsec = do
    ls <- getCombinedBkg
    let rs = map (brief . normalize (xsec,lum) . adjust) ls
    return rs

-- getCombinedBkg :: IO [(Int,Int,Int,Int,Int,Int,Int,Int)]
getCombinedBkg = do
    rlst0 <- sumAll <$> T.mapM parse1file (map bkgfile0 [0..9])
    rlst1 <- sumAll <$> T.mapM parse1file (map bkgfile1 [0..9])
    return (rlst0 ++ rlst1)

parse1file file = do
    str <- readFile file
    let ls = lines str
        rs = (map (parseOptOne . words) . drop 3) ls
    return rs

sumAll :: (Num a) => [[(Double,Double,Double,Double,Double,Double,a,a,a,a,a,a,a,a,a,a)]] 
       -> [(Double,Double,Double,Double,Double,Double,a,a,a,a,a,a,a,a,a,a)]
sumAll xs | any null xs =  []
          | otherwise = let ys = map head xs
                            rest = map tail xs
                            (a,b,c,d,e,f,_,_,_,_,_,_,_,_,_,_) =  head ys
                            sum1 = sum (map get7 ys)
                            sum2 = sum (map get8 ys)
                            sum3 = sum (map get9 ys)
                            sum4 = sum (map get10 ys)
                            sum5 = sum (map get11 ys)
                            sum6 = sum (map get12 ys)
                            sum7 = sum (map get13 ys)
                            sum8 = sum (map get14 ys)
                            sum9 = sum (map get15 ys)
                            sum10 = sum (map get16 ys)
                        in (a,b,c,d,e,f,sum1,sum2,sum3,sum4,sum5,sum6,sum7,sum8,sum9,sum10): sumAll rest


-- get6  (_,_,_,_,_,a,_,_,_,_,_,_,_,_,_) = a
get7  (_,_,_,_,_,_,a,_,_,_,_,_,_,_,_,_) = a
get8  (_,_,_,_,_,_,_,a,_,_,_,_,_,_,_,_) = a
get9  (_,_,_,_,_,_,_,_,a,_,_,_,_,_,_,_) = a
get10 (_,_,_,_,_,_,_,_,_,a,_,_,_,_,_,_) = a
get11 (_,_,_,_,_,_,_,_,_,_,a,_,_,_,_,_) = a
get12 (_,_,_,_,_,_,_,_,_,_,_,a,_,_,_,_) = a
get13 (_,_,_,_,_,_,_,_,_,_,_,_,a,_,_,_) = a
get14 (_,_,_,_,_,_,_,_,_,_,_,_,_,a,_,_) = a
get15 (_,_,_,_,_,_,_,_,_,_,_,_,_,_,a,_) = a
get16 (_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,a) = a



adjust (x1,x2,x3,x4,x5,x6,y1,y2,y3,y4,y5,y6,y7,y8,y9,y10) = (x1,x2,x3,x4,x5,x6,adj y1,adj y2,adj y3,adj y4,adj y5,adj y6,adj y7,adj y8,adj y9,adj y10)
  where adj n | n == 0 = 1
              | otherwise = n


tupling :: (Int,Int,Int,Int,Int,Double  ,Double,Double,Double,Double) -> ((Int,Int,Int,Int,Int,Double),(Double,Double,Double,Double))
tupling (x1,x2,x3,x4,x5,x6,y7,y8,y9,y10) = ((x1,x2,x3,x4,x5,x6),(y7,y8,y9,y10))

joinTable :: [(Int,Int,Int,Int,Int,Double, Double,Double,Double,Double)] 
          -> [(Int,Int,Int,Int,Int,Double, Double,Double,Double,Double)] 
          -> [((Int,Int,Int,Int,Int,Double),((Double,Double,Double,Double),(Double,Double,Double,Double)))]
joinTable tbl1 tbl2 = map f lst2
  where 
    lst1 = map tupling tbl1 
    lst2 = map tupling tbl2
    map1 = HM.fromList lst1
    f (k,v) = case HM.lookup k map1 of
                Nothing -> error "cannot"
                Just v0 -> (k,(v0,v))


sigoversqrtbkg ((b0,b1,b2,b3),(s0,s1,s2,s3)) = (sbf b0 s0, sbf b1 s1, sbf b2 s2, sbf b3 s3)
  where sbf b s = s / sqrt b

maximum4 (x,y,z,w) = maximum [x,y,z,w]


work :: [(Int,Int,Int,Int,Int,Double, Double,Double,Double,Double)] -> Double -> EffXsec -> IO (Int,Int,Double)
work bkgtbl lum (EffXsec m tb xsec) = do 
    rlst0 <- parse1file (sigfile0 m)
    rlst1 <- parse1file (sigfile1 m)
    let rs = map (brief . normalize (xsec,lum)) (rlst0 ++ rlst1)
    let r = maximum . map maximum4 . map sigoversqrtbkg . map snd . joinTable bkgtbl $ rs
    return (m,tb,r)
{- 
  where 
    parse1file file = do
      str <- readFile file
      let ls = lines str
      (return . map (parseOptOne . words) . drop 3) ls

-}


data BJetChoice = BJet0 | BJet1 | BJet2 | BJet3 deriving Show

work2 :: [(Int,Int,Int,Int,Int,Double, Double,Double,Double,Double)] -> Double -> EffXsec -> IO (Int,Int,[(Double,(Int,Int,Int,Int,Int,Double),BJetChoice)])
work2 bkgtbl lum (EffXsec m tb xsec) = do 
    rlst0 <- parse1file (sigfile0 m)
    rlst1 <- parse1file (sigfile1 m)
    let rs = map (brief . normalize (xsec,lum)) (rlst0 ++ rlst1)
        bkgsigtbl = joinTable bkgtbl rs

    -- print (length rs)
{-     str <- readFile (sigfile m)
    let ls = lines str
        rs = (map (brief . normalize (xsec,lum) . parseOptOne . words) . drop 3) ls -}
    -- (mapM_ print . map (parseOptOne . words) . drop 3 . take 10) ls

    let f x = let bkgsig = snd x
                  (ssqrtb0,ssqrtb1,ssqrtb2,ssqrtb3) = sigoversqrtbkg bkgsig
               in [(ssqrtb0,fst x,BJet0),(ssqrtb1,fst x,BJet1),(ssqrtb2,fst x,BJet2),(ssqrtb3,fst x,BJet3)]
        results = sortBy (flip compare `on` (view _1)) . concatMap f $ bkgsigtbl
    print (m,tb,head results)
    return (m,tb,results)


-- tupling3to2 (a,b,c) = ((a,b),c)


findrow :: Int -> HM.HashMap (Int,Int) Double -> [Double] 
findrow tanb m = let lst = [400,450..1000]
                     f x = case HM.lookup (x,tanb) m of
                             Nothing -> error "findrow"
                             Just v -> v
                 in map f lst


triple :: [String] -> ((Int,Int),Double)
triple [x1,x2,x3] = ((read x1,read x2),read x3)

--                tanb 400   450   500   550    600  650   700   750   800   850   900   950   1000
format tanb [x40,x45,x50,x55,x60,x65,x70,x75,x80,x85,x90,x95,x100] = 
  printf " %4d  %.6f  %.6f  %.6f  %.6f  %.6f  %.6f  %.6f  %.6f  %.6f  %.6f  %.6f  %.6f  %.6f"     tanb x40 x45 x50 x55 x60 x65 x70 x75 x80 x85 x90 x85 x100


detailPrint :: (Int,Int,[(Double,(Int,Int,Int,Int,Int,Double),BJetChoice)]) -> IO ()
detailPrint (m,tb,lst) = do 
    let filename = "HeavyHiggs2T2BInnerTop_mA" ++ show m ++ "tanb" ++ show tb ++ "_sigoversqrtb_cutresult.dat"
    withFile filename WriteMode $ \h -> do
      mapM_ (hPutStrLn h . myformatting) lst 
      hFlush h
  where myformatting (soverrtbkg,(j1,j2,j34,l,ht,deta),bjet) = 
          printf " %10.7e  %5d  %5d  %5d  %5d  %5d  %4.2e  %s " soverrtbkg j1 j2 j34 l ht deta (show bjet)

{- 
mkBestSoverSqrtBkg = do
  let xsecLO_ttbar = 6.50e5
      kfac_ttbar = 1.5
      lum = 1000
  bkgtbl <- bkg lum (xsecLO_ttbar*kfac_ttbar)
  str <- readFile "HeavyHiggs2T2BInnerTop_feynhiggs_mA_tanb_scan.dat"
  let ls = (map effXsec . map parseline . map words . lines) str 
  tbl <- map tupling3to2 <$> mapM (work bkgtbl lum) ls
  mapM_ (\((x,y),z)-> putStrLn (show x ++ " " ++ show y ++ " " ++ show z) >> hFlush stdout) tbl 
-}

formatBestSoverSqrtBkg = do 
  str <- readFile "HeavyHiggs2T2BInnerTop_mA_tanb_sig.dat"
  let ls = lines str
      tbl = HM.fromList (map (triple . words) ls)
  mapM_ (\x -> putStrLn . format x $ findrow x tbl) [1..50]

mkDetailSort = do
  let xsecLO_ttbar = 6.50e5
      kfac_ttbar = 1.5
      lum = 1000
  bkgtbl <- bkg lum (xsecLO_ttbar*kfac_ttbar)
  str <- readFile "HeavyHiggs2T2BInnerTop_feynhiggs_mA_tanb_scan.dat"
  let ls = (map effXsec . map parseline . map words . lines) str 
  print "mkDetailSort"
  mapM_ (detailPrint <=< work2 bkgtbl lum) ls


main = -- mkDetailSort
       -- mkBestSoverSqrtBkg'
       formatBestSoverSqrtBkg    


mkBestSoverSqrtBkg' = do
    let lst = [(m,tb) | m <- [400,450..1000], tb <- [1..50] ]
    mapM_ worker lst
  where
    worker (m,tb) = do 

    let filename = "HeavyHiggs2T2BInnerTop_mA" ++ show m ++ "tanb" ++ show tb ++ "_sigoversqrtb_cutresult.dat"
    withFile filename ReadMode $ \h -> do
      l <- hGetLine h  
      let xs = words l 
      putStrLn (show m ++ " " ++ show tb ++ " " ++ (head xs))
