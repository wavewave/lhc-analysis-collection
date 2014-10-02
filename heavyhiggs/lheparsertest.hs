{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative
import           Control.Monad.Trans
import           Control.Monad.State
import           Data.Conduit 
import qualified Data.Conduit.List as CL
import qualified Data.Conduit.Internal as CI
import qualified Data.Foldable as F
import qualified Data.IntMap as IM
import           Data.Maybe
import qualified Data.Text.IO as TIO
import qualified Data.Traversable as Tr (sequenceA,traverse) 
import           System.IO
import           Text.XML.Conduit.Parse.Util
-- 
-- import Data.Conduit.Internal as CU
import Data.Conduit.Util.Count 
import HEP.Parser.LHE.Conduit
import HEP.Parser.LHE.Type
import HEP.Parser.LHE.DecayTop
import HROOT

counterProgress = ZipSink countIter <* ZipSink countMarkerIter


main = do 
  putStrLn "test"
  h1 <- newTH1F "test" "test" 100 999 1001

  withFile "unweighted_events_4toppart.lhe" ReadMode $ \ih -> do 
    let iter = do
          header <- textLHEHeader 
          liftIO $ mapM_ TIO.putStrLn header 
          parseEvent =$ process 
        process = decayTopConduit
                  -- =$ CL.isolate 100 
                  =$ getZipSink (ZipSink (tester h1 0) <* counterProgress)
    r <- flip runStateT (0 :: Int) (parseXmlFile ih iter)
    putStrLn $ show r 

  tfile <- newTFile "test.root" "NEW" "" 1
  write h1 "" 0 0 
  close tfile ""

tester :: (MonadIO m) => TH1F -> Int -> Sink LHEventTop m Int
tester h1 n = do
  ev <- await
  case ev of 
    Nothing -> return n 
    Just ev -> case matchTest ev of 
                 Nothing -> tester h1 n     
                 Just xs  -> do let Just (px,py,pz,e,m) = (heavyhiggsmom . mkMatchMap) xs 
                                liftIO $ fill1 h1 (realToFrac m)
                                tester h1 (n+1)

{- 
testev :: LHEvent -> IO ()
testev ev@(LHEvent evinfo ptlinfos) = -- print (nup evinfo)
  putStrLn $ smplPrint $ getDecayTop ev
-}

smplPrint :: LHEventTop -> String
smplPrint = show . map (fmap getPDGID ) .  lhet_dtops


hhiggs= [35]
lep = [11,13,-11,-13]
neut = [12,14,-12,-14]
jets = [1,2,3,4,-1,-2,-3,-4]
tq = [6]
tbarq = [-6]
bq = [5]
bbarq= [-5]
wp = [24]
wm = [-24]

patt_t    = Decay (( 1,tbarq), [ Decay (( 2,wm),[Terminal (3,jets),Terminal (4,jets)])
                                              , Terminal (5,bbarq)
                                              ])
patt_tbar = Decay (( 6,tq), [ Decay (( 7,wp),[Terminal (8,jets),Terminal (9,jets)])
                                              , Terminal (10,bq)
                                             ])

patt_hh   = Decay ((11,hhiggs), [ Decay ((12,tq), [ Decay ((13,wp), [ Terminal (14,lep)
                                                                    , Terminal (15,neut)
                                                                    ])
                                                  , Terminal (16,bq) 
                                                  ])
                                , Decay ((17,tbarq), [ Decay ((18,wm), [ Terminal (19,lep)
                                                                       , Terminal (20,neut)
                                                                       , Terminal (21,bbarq)
                                                                       ])
                                                     ])
                                ])


matchTest :: LHEventTop -> Maybe [DecayTop (Int,PtlIDInfo)]
matchTest ev = 
    let m1 = matchDecayTop patt_t
        m2 = matchDecayTop patt_tbar
        m3 = matchDecayTop patt_hh
        r1 = let r = mapMaybe m1 dtops in if null r then Nothing else Just (head r)
        r2 = let r = mapMaybe m2 dtops in if null r then Nothing else Just (head r)
        r3 = let r = mapMaybe m3 dtops in if null r then Nothing else Just (head r)
    in Tr.sequenceA [r1,r2,r3]
  where dtops = lhet_dtops ev

mkMatchMap :: [DecayTop (Int,PtlIDInfo)] -> IM.IntMap PtlIDInfo
mkMatchMap = IM.fromList . concatMap (F.fold . fmap (\x -> [x])) 


heavyhiggsmom :: IM.IntMap PtlIDInfo -> Maybe (Double,Double,Double,Double,Double)
heavyhiggsmom m = pup . ptlinfo <$> (IM.lookup 11 m) 
