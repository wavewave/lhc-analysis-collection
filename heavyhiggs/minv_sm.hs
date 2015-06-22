{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

import Control.Applicative
import           Control.Monad.Trans
import           Control.Monad.State
import           Data.Conduit 
import           Data.Conduit.Binary (sourceHandle)
import qualified Data.Conduit.List as CL
import qualified Data.Conduit.Internal as CI
import           Data.Conduit.Zlib (ungzip)
import qualified Data.Foldable as F
import qualified Data.IntMap as IM
import           Data.Maybe
import qualified Data.Text.IO as TIO
import qualified Data.Traversable as Tr (sequenceA,traverse) 
import           Foreign.C.Types
import           System.IO
import           Text.XML.Stream.Parse
-- import           Text.XML.Conduit.Parse.Util
-- 
-- import Data.Conduit.Internal as CU
import Data.Conduit.Util.Count 
import HEP.Parser.LHE.Conduit
import HEP.Parser.LHE.Type
import HEP.Parser.LHE.DecayTop
import HEP.Util.Functions

import HROOT

counterProgress = ZipSink countIter <* ZipSink countMarkerIter

hhiggs= [35,36]
lep = [11,13,-11,-13]
neut = [12,14,-12,-14]
jets = [1,2,3,4,-1,-2,-3,-4]
tq = [6]
tbarq = [-6]
bq = [5]
bbarq= [-5]
wp = [24]
wm = [-24]
z0 = [23]




patt_tbar = Decay (( 1,tbarq), [ Decay (( 2,wm), [ Terminal (3,jets),Terminal (4,jets)])
                                                 , Terminal (5,bbarq)
                                                 ])

patt_t    = Decay (( 6,tq), [ Decay (( 7,wp), [ Terminal (8,lep),Terminal (9,neut)])
                                              , Terminal (10,bq)
                                              ])

p_had = Decay (( 1, [6,-6]), [ Decay (( 2,[24,-24]), [ Terminal (3,jets),Terminal (4,jets)])
                                                      , Terminal (5,[5,-5])
                                                      ])

p_lep = Decay (( 6, [6,-6]), [ Decay (( 7,[24,-24]), [ Terminal (8,lep),Terminal (9,neut)])
                                                     , Terminal (10,[5,-5])
                                                     ])


patt_hh   = Decay ((12,hhiggs), [ p_had,p_lep ])



matchTestSM :: LHEventTop -> Maybe [DecayTop (Int,PtlIDInfo)]
matchTestSM ev = 
    let m1 = matchDecayTop patt_t
        m2 = matchDecayTop patt_tbar
        m3 = matchDecayTop (Terminal (11,jets))
        r1 = let r = mapMaybe m1 dtops in if null r then Nothing else Just (head r)
        r2 = let r = mapMaybe m2 dtops in if null r then Nothing else Just (head r)
        r3 = let r = mapMaybe m3 dtops in if null r then Nothing else Just (head r)
    in Tr.sequenceA [r1,r2,r3]
  where dtops = lhet_dtops ev

matchTestHH :: LHEventTop -> Maybe [DecayTop (Int,PtlIDInfo)]
matchTestHH ev = 
    let h = matchDecayTop patt_hh
        m3 = matchDecayTop (Terminal (11,bq++bbarq))
        r = let r = mapMaybe h dtops in if null r then Nothing else Just (head r)
        r3 = let r = mapMaybe m3 dtops in if null r then Nothing else Just (head r)
    in Tr.sequenceA [r,r3]
  where dtops = lhet_dtops ev

mkMatchMap :: [DecayTop (Int,PtlIDInfo)] -> IM.IntMap PtlIDInfo
mkMatchMap = IM.fromList . concatMap (F.fold . fmap (\x -> [x])) 

getMom x = (pupTo4mom . pup . ptlinfo) x 




-- whad :: IM.IntMap PtlIDInfo -> Maybe (FourMomentum,FourMomentum)
whad m  = do j1 <- IM.lookup 3 m
             j2 <- IM.lookup 4 m
             return (getMom j1, getMom j2)

wlep m  = do l <- IM.lookup 8 m
             n <- IM.lookup 9 m
             return (getMom l, getMom n)

bjets m = do b1 <- IM.lookup 5 m
             b2 <- IM.lookup 10 m
             return (getMom b1, getMom b2)

ojet m  = do j <- IM.lookup 11 m
             return (getMom j)

ptvec (t,x,y,z) = (x,y)

mass mom = sqrt (sqr4 mom)

transverseMass m1 m2 (x1,y1) (x2,y2) = sqrt (m1*m1 + m2*m2 + 2*(eT m1 (x1,y1))*(eT m2 (x2,y2)) - 2*x1*x2 - 2*y1*y2)
  where eT m (x,y) = sqrt (m*m+x*x+y*y)



mkplotSM :: (MonadIO m) => (TH1F,TH1F) -> Sink LHEventTop m ()
mkplotSM (h1,h2) = do
  ev <- await
  case ev of 
    Nothing -> return ()
    Just ev -> case matchTestSM ev of 
      Nothing -> mkplotSM (h1,h2)    
      Just xs  -> do 
        let match = mkMatchMap xs
            Just (j1,j2)    = whad match
            Just (lep,neut) = wlep match
            Just (b1,b2)    = bjets match
            Just oj         = ojet match 
            cluster1 = j1 `plus` j2 `plus` lep `plus` b1 `plus` b2
            cluster2 = j1 `plus` j2 `plus` lep `plus` b1 `plus` b2 `plus` oj
            trm1 = transverseMass (mass cluster1) 0 (ptvec cluster1) (ptvec neut) 
            trm2 = transverseMass (mass cluster2) 0 (ptvec cluster2) (ptvec neut) 
        liftIO $ (fill1 h1 . realToFrac) trm1
        liftIO $ (fill1 h2 . realToFrac) trm2
        liftIO $ print (trm1,trm2)
        mkplotSM (h1,h2)


mkplotHH :: (MonadIO m) => (TH1F,TH1F) -> Sink LHEventTop m ()
mkplotHH (h1,h2) = do
  ev <- await
  case ev of 
    Nothing -> return ()
    Just ev -> do
     -- liftIO $ mapM_ print (lhet_dtops ev)
     
     case matchTestHH ev of 
      Nothing -> mkplotHH (h1,h2)    
      Just xs  -> do 
        let match = mkMatchMap xs
            Just (j1,j2)    = whad match
            Just (lep,neut) = wlep match
            Just (b1,b2)    = bjets match
            Just oj         = ojet match 
            cluster1 = j1 `plus` j2 `plus` lep `plus` b1 `plus` b2
            cluster2 = j1 `plus` j2 `plus` lep `plus` b1 `plus` b2 `plus` oj
            trm1 = transverseMass (mass cluster1) 0 (ptvec cluster1) (ptvec neut) 
            trm2 = transverseMass (mass cluster2) 0 (ptvec cluster2) (ptvec neut) 
        liftIO $ (fill1 h1 . realToFrac) trm1
        liftIO $ (fill1 h2 . realToFrac) trm2
        liftIO $ print (trm1,trm2)
        mkplotHH (h1,h2)


main = do 
  h1_HH <- newTH1F "ttbarHH" "ttbarHH" 50 0 2000
  setLineColor h1_HH 1
  h2_HH <- newTH1F "ttbar1HH" "ttbar1HH" 50 0 2000
  setLineColor h2_HH 2

  h1_SM <- newTH1F "ttbarSM" "ttbarSM" 50 0 2000
  setLineColor h1_SM 3
  h2_SM <- newTH1F "ttbar1SM" "ttbar1SM" 50 0 2000
  setLineColor h2_SM 4

  withFile "HeavyHiggsMHH1000.0_2t2b_innertop_decayfull_LHC14ATLAS_NoMatch_DefCut_Cone0.4_WithTau_Set1_unweighted_events.lhe.gz" ReadMode $ 
    \ih -> flip runStateT (0::Int) $
      sourceHandle ih =$= ungzip =$= parseBytes def $$ do
        textLHEHeader
        parseEvent =$= decayTopConduit =$= getZipSink (ZipSink (mkplotHH (h1_HH,h2_HH)) <* counterProgress)

  withFile "SM_tt1j_decayfull_LHC14ATLAS_NoMatch_DefCut_Cone0.4_WithTau_Set1_unweighted_events.lhe.gz" ReadMode $ 
    \ih -> flip runStateT (0::Int) $
      sourceHandle ih =$= ungzip =$= parseBytes def $$ do
        textLHEHeader
        parseEvent =$= decayTopConduit =$= getZipSink (ZipSink (mkplotSM (h1_SM,h2_SM)) <* counterProgress)


  c <- newTCanvas "c1" "c1" 640 480
  draw h1_HH ""
  draw h2_HH "same"
  saveAs c "HH.pdf" ""

  c2 <- newTCanvas "c2" "c2" 640 480
  draw h1_SM ""
  draw h2_SM "same"
  saveAs c2 "SM.pdf" ""


