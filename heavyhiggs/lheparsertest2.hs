{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

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
import           Foreign.C.Types
import           System.IO
import           Text.XML.Conduit.Parse.Util
-- 
-- import Data.Conduit.Internal as CU
import Data.Conduit.Util.Count 
import HEP.Parser.LHE.Conduit
import HEP.Parser.LHE.Type
import HEP.Parser.LHE.DecayTop
import HEP.Util.Functions (mom_2_pt_eta_phi,snd3)

import HROOT

counterProgress = ZipSink countIter <* ZipSink countMarkerIter


data HistColl = HistColl { hist1 :: TH1F
                         , hist2 :: TH1F
                         , hist3 :: TH1F
                         , hist4 :: TH1F
                         , hist5 :: TH1F 
                         , hist2d :: TH2F
                         }

prepareHist :: (CInt,CDouble,CDouble) -> IO HistColl 
prepareHist (n,start,end) = do 
    h1 <- newTH1F "h1" "h1" n start end
    setLineColor h1 1
    h2 <- newTH1F "h2" "h2" n start end
    setLineColor h2 2
    h3 <- newTH1F "h3" "h3" n start end
    setLineColor h3 3
    h4 <- newTH1F "h4" "h4" n start end
    setLineColor h4 4
    h5 <- newTH1F "h5" "h5" n start end
    setLineColor h5 5
    h2d <- newTH2F "h2d" "h2d" n start end n start end
    return (HistColl h1 h2 h3 h4 h5 h2d)
    
main = do 
  putStrLn "test"
  c1 <- newTCanvas "test" "test" 1024 768 
  h <- prepareHist (100,0,2500) -- (100,-5,5) -- (100,0,1500) 

  withFile "unweighted_events_4toppart.lhe" ReadMode $ \ih -> do 
    let iter = do
          header <- textLHEHeader 
          liftIO $ mapM_ TIO.putStrLn header 
          parseEvent =$ process 
        process = decayTopConduit
                  -- =$ CL.isolate 100 
                  =$ getZipSink (ZipSink (mkplot h 0) <* counterProgress)
    r <- flip runStateT (0 :: Int) (parseXmlFile ih iter)
    putStrLn $ show r 
  drawTogether [hist2 h,hist1 h,hist3 h]
  -- draw (hist2d h) ""

  saveAs c1 "test.pdf" ""

drawTogether :: (ITH1 a) => [a] -> IO ()
drawTogether [] = return ()
drawTogether (x:xs) = draw x "" >> mapM_ (\x->draw x "same") xs


mkplot :: (MonadIO m) => HistColl -> Int -> Sink LHEventTop m Int
mkplot h@HistColl{..} n = do
  ev <- await
  case ev of 
    Nothing -> return n 
    Just ev -> case matchTest ev of 
                 Nothing -> mkplot h n     
                 Just xs  -> do let match = mkMatchMap xs
                                    -- Just (t1,t2,t3,t4) = topetas match
                                    Just (j1,j2,j3,j4) = jetpts match
                                    Just (b1,b2,b3,b4) = bjetpts match
                                    Just (l1,l2) = leppts match
                                    Just (n1,n2) = neutpts match
                                    -- Just (mout,min) = bbinvm match
                                    meff_out = b1+b2+j1+j2+j3+j4
                                    meff_in = b3+b4+l1+l2+n1+n2
                                    meff_all = meff_out + meff_in
-- +n1
-- +n2
               
                                liftIO $ (fill1 hist1 . realToFrac) meff_out
                                liftIO $ (fill1 hist2 . realToFrac) meff_in
                                liftIO $ (fill1 hist3 . realToFrac) meff_all


                                -- liftIO $ (fill1 hist1 . realToFrac) mout
                                -- liftIO $ (fill1 hist2 . realToFrac) min
                                -- liftIO $ (fill2 hist2d  (realToFrac mout) (realToFrac min))
                                -- liftIO $ (fill2 hist2d (realToFrac (abs (be1-be2))) (realToFrac (abs (be3-be4))))
             
                                -- liftIO $ mapM_ (fill1 hist1 . realToFrac) [be1,be2] -- [(t1-t2)] 
                                -- liftIO $ mapM_ (fill1 hist2 . realToFrac) [be3,be4] -- [(t3-t4)]
                                -- liftIO $ mapM_ (fill1 hist1 . realToFrac) [e1,e2,e3,e4]
                                -- liftIO $ mapM_ (fill1 hist2 . realToFrac) [be1,be2,be3,be4]
                                -- liftIO $ mapM_ (fill1 hist3 . realToFrac) [be3,be4]
                                -- liftIO $ mapM_ (fill1 hist4 . realToFrac) [be1,be2]
                                -- liftIO $ mapM_ (fill1 hist5 . realToFrac) [l1,l2]
                                mkplot h (n+1)


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

leppts :: IM.IntMap PtlIDInfo -> Maybe (Double,Double)
leppts m = do l1 <- IM.lookup 14 m
              l2 <- IM.lookup 19 m
              let pt x = let (px,py,pz,pt,m) = (pup . ptlinfo) x 
                         in sqrt (px^2+py^2)
              return (pt l1, pt l2)

neutpts :: IM.IntMap PtlIDInfo -> Maybe (Double,Double)
neutpts m = do n1 <- IM.lookup 15 m
               n2 <- IM.lookup 20 m
               let pt x = let (px,py,pz,pt,m) = (pup . ptlinfo) x 
                          in sqrt (px^2+py^2)
               return (pt n1, pt n2)


jetEs :: IM.IntMap PtlIDInfo -> Maybe (Double,Double,Double,Double)
jetEs m = do j1 <- IM.lookup 3 m
             j2 <- IM.lookup 4 m
             j3 <- IM.lookup 8 m
             j4 <- IM.lookup 9 m
             let energy x = let (px,py,pz,pt,m) = (pup . ptlinfo) x 
                            in pt -- sqrt (px^2+py^2)
             return (energy j1, energy j2, energy j3, energy j4)

bjetEs :: IM.IntMap PtlIDInfo -> Maybe (Double,Double,Double,Double)
bjetEs m = do j1 <- IM.lookup 5 m
              j2 <- IM.lookup 10 m
              j3 <- IM.lookup 16 m
              j4 <- IM.lookup 21 m
              let energy x = let (px,py,pz,pt,m) = (pup . ptlinfo) x 
                         in pt -- sqrt (px^2+py^2)
              return (energy j1, energy j2, energy j3, energy j4)

jetetas :: IM.IntMap PtlIDInfo -> Maybe (Double,Double,Double,Double)
jetetas m = do j1 <- IM.lookup 3 m
               j2 <- IM.lookup 4 m
               j3 <- IM.lookup 8 m
               j4 <- IM.lookup 9 m
               let eta x = let (px,py,pz,pt,m) = (pup . ptlinfo) x 
                           in snd3 (mom_2_pt_eta_phi (pt,px,py,pz)) -- sqrt (px^2+py^2)
               return (eta j1, eta j2, eta j3, eta j4)

bjetetas :: IM.IntMap PtlIDInfo -> Maybe (Double,Double,Double,Double)
bjetetas m = do j1 <- IM.lookup 5 m
                j2 <- IM.lookup 10 m
                j3 <- IM.lookup 16 m
                j4 <- IM.lookup 21 m
                let eta x = let (px,py,pz,pt,m) = (pup . ptlinfo) x 
                            in snd3 (mom_2_pt_eta_phi (pt,px,py,pz)) -- sqrt (px^2+py^2)
                return (eta j1, eta j2, eta j3, eta j4)

lepetas :: IM.IntMap PtlIDInfo -> Maybe (Double,Double)
lepetas m = do l1 <- IM.lookup 14 m
               l2 <- IM.lookup 19 m
               let eta x = let (px,py,pz,pt,m) = (pup . ptlinfo) x 
                           in snd3 (mom_2_pt_eta_phi (pt,px,py,pz))
               return (eta l1, eta l2)


topetas :: IM.IntMap PtlIDInfo -> Maybe (Double,Double,Double,Double)
topetas m = do t1 <- IM.lookup 6 m
               t2 <- IM.lookup 1 m
               t3 <- IM.lookup 12 m 
               t4 <- IM.lookup 17 m
               let eta x = let (px,py,pz,pt,m) = (pup . ptlinfo) x 
                           in snd3 (mom_2_pt_eta_phi (pt,px,py,pz))
               return (eta t1, eta t2, eta t3, eta t4)

bbinvm :: IM.IntMap PtlIDInfo -> Maybe (Double,Double)
bbinvm m = do j1 <- IM.lookup 5 m
              j2 <- IM.lookup 10 m
              j3 <- IM.lookup 16 m
              j4 <- IM.lookup 21 m
              let invm x1 x2 = let (p1x,p1y,p1z,p1t,m1) = (pup . ptlinfo) x1 
                                   (p2x,p2y,p2z,p2t,m2) = (pup . ptlinfo) x2
                               in invmass (p1x,p1y,p1z,p1t) (p2x,p2y,p2z,p2t) 
              return (invm j1 j2, invm j3 j4)

