{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module Main where

import           Control.Applicative
import           Control.Lens
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Either
import qualified Data.Attoparsec.ByteString.Char8 as A
import qualified Data.ByteString.Char8 as B
import qualified Data.Foldable as F
import           Data.Function (on)
import qualified Data.HashMap.Strict as HM
import           Data.List (sortBy)
import           Data.Maybe
import           Data.Monoid
import           Data.Scientific
import           Text.Printf


data Info = Info { info_hT :: Scientific -- Double
                 , info_pT_j1 :: Scientific -- Double
                 , info_j234 :: Scientific --  Double
                 , info_j56 :: Scientific -- Double
                 , info_full :: Int
                 , info_cut1 :: Int
                 , info_cut2 :: Int
                 , info_cut3 :: Int
                 , info_lepeta :: Int
                 , info_htcut :: Int
                 , info_bj1 :: Int
                 , info_bj2 :: Int
                 , info_bj3 :: Int
                 } deriving (Show,Eq,Ord)


normalizedTriple :: Info -> (Double,Double,Double,Int,Int,Int)
normalizedTriple Info {..} = (fromIntegral info_bj1 / full, fromIntegral info_bj2 / full, fromIntegral info_bj3/full, info_bj1, info_bj2, info_bj3 )
  where full = fromIntegral info_full


normalizeInput :: Info -> (Int,Int,Int,Int)
normalizeInput Info {..} = if (hT `mod` 5 /= 0 || j1 `mod` 5 /= 0 || j234 `mod` 5 /= 0 || j56 `mod` 5 /= 0)
                             then error "error in normalizeInput"
                             else (hT,j1,j234,j56)
  where hT = floor info_hT
        j1 = floor info_pT_j1
        j234 = floor info_j234
        j56 = floor info_j56


type OneRow = (First Info,First Info,First Info,First Info) 

data ColumnTag = TTBar | FourTop400 | FourTop750 | FourTop1000

adjustData :: ColumnTag -> ((Int,Int,Int,Int), Info) 
           -> HM.HashMap (Int,Int,Int,Int) OneRow -> HM.HashMap (Int,Int,Int,Int) OneRow
adjustData TTBar       (k,i) = HM.insertWith mappend k (First (Just i), mempty, mempty, mempty)
adjustData FourTop400  (k,i) = HM.insertWith mappend k (mempty, First (Just i), mempty, mempty)
adjustData FourTop750  (k,i) = HM.insertWith mappend k (mempty, mempty, First (Just i), mempty)
adjustData FourTop1000 (k,i) = HM.insertWith mappend k (mempty, mempty, mempty, First (Just i))

isFilled :: OneRow -> Bool
isFilled (First (Just a),First (Just b), First (Just c), First (Just d)) = True
isFilled _ = False

maybeFilled :: OneRow -> Maybe (Info,Info,Info,Info)
maybeFilled (First (Just a),First (Just b), First (Just c), First (Just d)) = Just (a,b,c,d)
maybeFilled _ = Nothing

removeUnfilled :: HM.HashMap (Int,Int,Int,Int) OneRow -> HM.HashMap (Int,Int,Int,Int) (Info,Info,Info,Info)
removeUnfilled = HM.fromList . mapMaybe (\(k,v) -> (k,) <$> maybeFilled v) . HM.toList 


commentline = do A.char '#' >> A.manyTill A.anyChar A.endOfLine 

header = (,,) <$> commentline <*> commentline <*> commentline 

dataline = do
  A.skipSpace
  hT <- A.scientific
  A.skipSpace
  pT_j1 <- A.scientific
  A.skipSpace
  j234 <- A.scientific
  A.skipSpace
  j56 <- A.scientific
  A.skipSpace
  full <- A.decimal
  A.skipSpace
  cut1 <- A.decimal
  A.skipSpace
  cut2 <- A.decimal
  A.skipSpace
  cut3 <- A.decimal
  A.skipSpace
  lepeta <- A.decimal
  A.skipSpace
  htcut <- A.decimal
  A.skipSpace
  bj1 <- A.decimal
  A.skipSpace
  bj2 <- A.decimal
  A.skipSpace
  bj3 <- A.decimal
  A.manyTill A.anyChar A.endOfLine
  return (Info hT pT_j1 j234 j56 full cut1 cut2 cut3 lepeta htcut bj1 bj2 bj3)

-- criterion1 :: (Double,Double,Double) -> (Double,Double,Double) -> Double
criterion l1 l2 blst@(bkg1,bkg2,bkg3,_,_,_) slst@(sig1,sig2,sig3,_,_,_) = (s / b, view l2 slst)
  where bkg = view l1 blst
        sig = view l1 slst
        b = if bkg < 1e-6 then 1e-6 else bkg
        s = sig

formatprint :: ((Int,Int,Int,Int),(Double,Int)) -> String
formatprint ((ht,j1,j234,j56),(eff,nsig)) = printf "%6d %6d %6d %6d   %.1f   %4d" ht j1 j234 j56 eff nsig



main :: IO ()
main = do 
  putStrLn "test"
  bstr_ttbar1 <- B.readFile "result/cutopt20150106_ttbar012.txt" 
  bstr_400 <- B.readFile "result/cutopt20150106_4top400.txt" 
  bstr_750 <- B.readFile "result/cutopt20150106_4top750.txt" 
  bstr_1000 <- B.readFile "result/cutopt20150106_4top1000.txt"

  r <- runEitherT $ do 
    results_ttbar <- hoistEither $ A.parseOnly (header *> many dataline) bstr_ttbar
    results_400   <- hoistEither $ A.parseOnly (header *> many dataline) bstr_400
    results_750   <- hoistEither $ A.parseOnly (header *> many dataline) bstr_750
    results_1000  <- hoistEither $ A.parseOnly (header *> many dataline) bstr_1000
    -- liftIO $ (mapM_ print . map normalizedTriple) results_ttbar
    let [kv_ttbar,kv_400,kv_750,kv_1000] = 
          fmap (map ((,) <$> normalizeInput <*> id)) [results_ttbar,results_400,results_750,results_1000]
    
    let m1 = foldr (adjustData TTBar) HM.empty kv_ttbar
        m2 = foldr (adjustData FourTop400) m1 kv_400
        m3 = foldr (adjustData FourTop750) m2 kv_750
        m4 = foldr (adjustData FourTop1000) m3 kv_1000
        m = removeUnfilled m4
 
        m' = fmap (\(x,y,z,w)->(f x, f y, f z, f w)) m where f = normalizedTriple
    -- liftIO $ print m 
    -- liftIO $ F.mapM_ (print . isFilled) m
    -- liftIO . print . HM.size $ m4
        m400_1  = fmap (\(x,y,z,w)->criterion _1 _4 x y) m'
        m750_1  = fmap (\(x,y,z,w)->criterion _1 _4 x z) m'
        m1000_1 = fmap (\(x,y,z,w)->criterion _1 _4 x w) m'
        m400_2  = fmap (\(x,y,z,w)->criterion _2 _5 x y) m'
        m750_2  = fmap (\(x,y,z,w)->criterion _2 _5 x z) m'
        m1000_2 = fmap (\(x,y,z,w)->criterion _2 _5 x w) m'
        m400_3  = fmap (\(x,y,z,w)->criterion _3 _6 x y) m'
        m750_3  = fmap (\(x,y,z,w)->criterion _3 _6 x z) m'
        m1000_3 = fmap (\(x,y,z,w)->criterion _3 _6 x w) m'


    liftIO . F.mapM_ (putStrLn . formatprint) . sortBy (flip compare `on` snd) $ HM.toList m400_2


    --liftIO $ (mapM_ print . map normalizeInput) results_ttbar
    --liftIO $ (mapM_ print . map normalizeInput) results_400
    --liftIO $ (mapM_ print . map normalizeInput) results_750
    --liftIO $ (mapM_ print . map normalizeInput) results_1000

    return ()
  print r
  return ()


