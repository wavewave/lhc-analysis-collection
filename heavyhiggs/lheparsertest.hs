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
  c1 <- newTCanvas "test" "test" 1024 768 
  -- h1 <- newTH1F "out" "out" 100 0 3000

  h1 <- newTH1F "jet pt" "jet pt" 100 0 500
  setLineColor h1 1
  h2 <- newTH1F "bjet pt" "bjet pt" 100 0 500
  setLineColor h2 2
  h3 <- newTH1F "bjet pt in" "bjet pt in" 100 0 500
  setLineColor h3 3
  h4 <- newTH1F "bjet pt out" "bjet pt out" 100 0 500
  setLineColor h4 4


  withFile "unweighted_events_4toppart.lhe" ReadMode $ \ih -> do 
    let iter = do
          header <- textLHEHeader 
          liftIO $ mapM_ TIO.putStrLn header 
          parseEvent =$ process 
        process = decayTopConduit
                  -- =$ CL.isolate 100 
                  =$ getZipSink (ZipSink (tester (h1,h2,h3,h4) 0) <* counterProgress)
    r <- flip runStateT (0 :: Int) (parseXmlFile ih iter)
    putStrLn $ show r 

  -- tfile <- newTFile "test.root" "NEW" "" 1
  -- write h1 "" 0 0 
  -- write h2 "" 0 0
  -- close tfile ""
  -- draw h1 "same"
  draw h1 ""
  draw h2 "same"
  draw h3 "same"
  draw h4 "same"
  saveAs c1 "test.pdf" ""

tester :: (MonadIO m) => (TH1F,TH1F,TH1F,TH1F) -> Int -> Sink LHEventTop m Int
tester (h1,h2,h3,h4) n = do
  ev <- await
  case ev of 
    Nothing -> return n 
    Just ev -> case matchTest ev of 
                 Nothing -> tester (h1,h2,h3,h4) n     
                 Just xs  -> do -- let Just (px,py,pz,e,m) = (heavyhiggsmom . mkMatchMap) xs 
                                let match = mkMatchMap xs
                                -- let Just mtt_out = invmass_out_ttbar match
                                --     Just mtt_in  = invmass_in_ttbar match
                                    Just (pt1,pt2,pt3,pt4) = jetpts match
                                    Just (bpt1,bpt2,bpt3,bpt4) = bjetpts match
                                liftIO $ mapM_ (fill1 h1 . realToFrac) [pt1,pt2,pt3,pt4]
                                liftIO $ mapM_ (fill1 h2 . realToFrac) [bpt1,bpt2,bpt3,bpt4]
                                liftIO $ mapM_ (fill1 h3 . realToFrac) [bpt3,bpt4]
                                liftIO $ mapM_ (fill1 h4 . realToFrac) [bpt1,bpt2]


                                -- liftIO $ fill1 h1 (realToFrac mtt_out)
                                
                                -- liftIO $ fill1 h2 (realToFrac mtt_in)
                                tester (h1,h2,h3,h4) (n+1)


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

patt_tbar = Decay (( 1,tbarq), [ Decay (( 2,wm), [ Terminal (3,jets),Terminal (4,jets)])
                                                 , Terminal (5,bbarq)
                                                 ])
patt_t    = Decay (( 6,tq), [ Decay (( 7,wp), [ Terminal (8,jets),Terminal (9,jets)])
                                              , Terminal (10,bq)
                                              ])

patt_hh   = Decay ((11,hhiggs), [ Decay ((12,tq),    [ Decay ((13,wp), [ Terminal (14,lep)
                                                                       , Terminal (15,neut)
                                                                       ])
                                                     , Terminal (16,bq) 
                                                     ])
                                , Decay ((17,tbarq), [ Decay ((18,wm), [ Terminal (19,lep)
                                                                       , Terminal (20,neut)
                                                                       ])
                                                     , Terminal (21,bbarq)
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

invmass (p1x,p1y,p1z,p1t) (p2x,p2y,p2z,p2t) = sqrt ((p1t+p2t)^2 - ((p1x+p2x)^2 + (p1y+p2y)^2 + (p1z+p2z)^2))

invmass_out_ttbar :: IM.IntMap PtlIDInfo -> Maybe Double
invmass_out_ttbar m = do tinfo    <- IM.lookup 6 m
                         tbarinfo <- IM.lookup 1 m
                         let (p1x,p1y,p1z,p1t,m1) = (pup . ptlinfo) tinfo
                             (p2x,p2y,p2z,p2t,m2) = (pup . ptlinfo) tbarinfo
                         return $ invmass (p1x,p1y,p1z,p1t) (p2x,p2y,p2z,p2t)

invmass_in_ttbar :: IM.IntMap PtlIDInfo -> Maybe Double
invmass_in_ttbar m = do tinfo    <- IM.lookup 12 m
                        tbarinfo <- IM.lookup 17 m
                        let (p1x,p1y,p1z,p1t,m1) = (pup . ptlinfo) tinfo
                            (p2x,p2y,p2z,p2t,m2) = (pup . ptlinfo) tbarinfo
                        return $ invmass (p1x,p1y,p1z,p1t) (p2x,p2y,p2z,p2t)

jetpts :: IM.IntMap PtlIDInfo -> Maybe (Double,Double,Double,Double)
jetpts m = do j1 <- IM.lookup 3 m
              j2 <- IM.lookup 4 m
              j3 <- IM.lookup 8 m
              j4 <- IM.lookup 9 m
              let pt x = let (px,py,pz,pt,m) = (pup . ptlinfo) x 
                         in sqrt (px^2+py^2)
              return (pt j1, pt j2, pt j3, pt j4)

bjetpts :: IM.IntMap PtlIDInfo -> Maybe (Double,Double,Double,Double)
bjetpts m = do j1 <- IM.lookup 5 m
               j2 <- IM.lookup 10 m
               j3 <- IM.lookup 16 m
               j4 <- IM.lookup 21 m
               let pt x = let (px,py,pz,pt,m) = (pup . ptlinfo) x 
                          in sqrt (px^2+py^2)
               return (pt j1, pt j2, pt j3, pt j4)
  
