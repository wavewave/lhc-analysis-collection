{-# LANGUAGE RecordWildCards, GADTs, EmptyDataDecls, NoMonomorphismRestriction #-}

-----------------------------------------------------------------------------
-- |
-- Module      : HEP.Physics.Analysis.ATLAS.SUSY_0L2to6J
-- Copyright   : (c) 2013 Ian-Woo Kim
--
-- License     : GPL-3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-- LHC ATLAS SUSY search analysis code 
-- 
-- Based on PRD87,012008 (2013) 
-- 
-----------------------------------------------------------------------------

module HEP.Physics.Analysis.ATLAS.SUSY_0L2to6J where

import Codec.Compression.GZip
import Control.Applicative
import Control.Lens
import Control.Monad
import Control.Monad.Indexed
import Control.Monad.Indexed.State 
import Control.Monad.Indexed.Trans
import Control.Monad.Trans
import qualified Data.ByteString.Lazy.Char8 as LB
import Data.Function (on)
import Data.List
import Data.Maybe
import System.FilePath
import System.Environment
-- 
import HEP.Parser.LHCOAnalysis.PhysObj
import HEP.Parser.LHCOAnalysis.Parse
-- 
import HEP.Physics.Analysis.ATLAS.Common
-- 
import Debug.Trace


-- meffNj 4 

-- | effective mass with N leading jets (used in PRL87,012008 (2008))
meffNj :: Int -> PhyEventClassified -> Double 
meffNj n PhyEventClassified {..} = 
  (sum . map (trd3.etaphipt.snd)) (take n jetlst) 
  + (snd.phiptmet) met

meffinc40 :: PhyEventClassified -> Double 
meffinc40 PhyEventClassified {..} = 
  (sum . map (trd3.etaphipt.snd) . filter ((>40).trd3.etaphipt.snd)) jetlst 
  + (snd.phiptmet) met

jes_correction :: PhyObj Jet -> PhyObj Jet 
jes_correction j = 
  let (j_eta,j_phi,j_pt) = etaphiptjet j 
      j_m = mjet j
      s = (14.23 + 7.53 * j_eta * j_eta) 
          / sqrt ( j_pt*j_pt * (cosh j_eta)^2 + j_m*j_m )  
      deltapt = j_pt * s
      deltam = j_m * s  
  in -- trace (" eta, pt, s = " ++ show j_eta ++ "," ++ show j_pt ++ "," ++ show s) $ 

     j { etaphiptjet = (j_eta,j_phi,j_pt+deltapt) 
       , mjet = j_m + deltam } 






data MoreThan2JEv = Ev2J { firstJet  :: (Int, PhyObj Jet)
                         , secondJet :: (Int, PhyObj Jet) 
                         , remainingEvent :: PhyEventClassified 
                         }

data SRFlag = SRFlag { sr_classA  :: Maybe (Bool,Bool,Bool) 
                     , sr_classA' :: Maybe (Bool,Bool,Bool) 
                     , sr_classB  :: Maybe (Bool,Bool,Bool) 
                     , sr_classC  :: Maybe (Bool,Bool,Bool)
                     , sr_classD  :: Maybe (Bool,Bool,Bool) 
                     , sr_classE  :: Maybe (Bool,Bool,Bool) 
                     } 
            deriving (Show,Eq,Ord)

classA :: Simple Lens SRFlag (Maybe (Bool,Bool,Bool))
classA = lens sr_classA (\f a -> f { sr_classA = a } )

classA' :: Simple Lens SRFlag (Maybe (Bool,Bool,Bool))
classA' = lens sr_classA' (\f a -> f { sr_classA' = a } )

classB :: Simple Lens SRFlag (Maybe (Bool,Bool,Bool))
classB = lens sr_classB (\f a -> f { sr_classB = a } )

classC :: Simple Lens SRFlag (Maybe (Bool,Bool,Bool))
classC = lens sr_classC (\f a -> f { sr_classC = a } )

classD :: Simple Lens SRFlag (Maybe (Bool,Bool,Bool))
classD  = lens sr_classD (\f a -> f { sr_classD = a } )

classE :: Simple Lens SRFlag (Maybe (Bool,Bool,Bool))
classE = lens sr_classE (\f a -> f { sr_classE = a } )


emptySRFlag = SRFlag Nothing Nothing Nothing Nothing Nothing Nothing 


showSR :: SRFlag -> String 
showSR SRFlag {..} = 
  maybe "" (\x->"A:"++show x++":") sr_classA 
  ++ maybe "" (\x->"A':"++show x++":") sr_classA'
  ++ maybe "" (\x->"B:"++show x++":") sr_classB
  ++ maybe "" (\x->"C:"++show x++":") sr_classC
  ++ maybe "" (\x->"D:"++show x++":") sr_classD
  ++ maybe "" (\x->"E:"++show x++":") sr_classE 


isTightEv :: (Bool,Bool,Bool) -> Bool 
isTightEv = fst3 

isMediumEv :: (Bool,Bool,Bool) -> Bool 
isMediumEv = snd3 
 
isLooseEv :: (Bool,Bool,Bool) -> Bool 
isLooseEv = trd3 


instance GetJetMerged MoreThan2JEv where 
  getJetMerged (Ev2J j1 j2 rev@PhyEventClassified {..}) = 
    JetMerged rev {jetlst = j1:j2:jetlst}

{- 
instance GetJetMerged AEv where 
  getJetMerged (AEv j1 j2 ev@PhyEventClassified {..}) = 
    JetMerged ev {jetlst=j1:j2:jetlst}

instance GetJetMerged ChAEv where
  getJetMerged (ChA e) = getJetMerged e 

instance GetJetMerged ChA'Ev where
  getJetMerged (ChA' e) = getJetMerged e 


instance GetJetMerged BEv where
  getJetMerged (BEv j1 j2 j3 ev@PhyEventClassified {..}) = 
    JetMerged ev {jetlst=j1:j2:j3:jetlst}

instance GetJetMerged CEv where
  getJetMerged (CEv j1 j2 j3 j4 ev@PhyEventClassified {..}) = 
    JetMerged ev {jetlst=j1:j2:j3:j4:jetlst}

instance GetJetMerged DEv where
  getJetMerged (DEv j1 j2 j3 j4 j5 ev@PhyEventClassified {..}) = 
    JetMerged ev {jetlst=j1:j2:j3:j4:j5:jetlst}

instance GetJetMerged EEv where
  getJetMerged (EEv j1 j2 j3 j4 j5 j6 ev@PhyEventClassified {..}) = 
    JetMerged ev {jetlst=j1:j2:j3:j4:j5:j6:jetlst}
-}



-- |
objrecon :: (MonadPlus m) => IxStateT m JetMergedEv JetMergedEv ()
objrecon = 
  iget >>>= \(JetMerged ev@PhyEventClassified {..}) -> 
  let elst' = filter (\(_,e)->pt e > 20 && abs (eta e) < 2.47) electronlst 
      mlst' = filter (\(_,m)->pt m > 10 && abs (eta m) < 2.4) muonlst 
      jlst' = filter (\(_,j)->pt j > 20 && abs (eta j) < 2.8) jetlst  
  in iput (JetMerged ev { electronlst = elst', muonlst = mlst', jetlst = jlst' })


-- | trigger event. hardest jet 75 GeV, MET > 55 GeV
trigger :: (MonadPlus m) => IxStateT m JetMergedEv JetMergedEv () 
trigger =
  iget >>>= \(JetMerged PhyEventClassified {..}) -> 
  iguard ((not.null) jetlst) >>>= \_ -> 
  iguard ((pt.snd.head) jetlst > 75) >>>= \_ ->
  iguard ((snd.phiptmet) met > 55) 

-- | lepton veto no electron > 20 GeV, no muon > 10 GeV
leptonVeto :: (MonadPlus m) => IxStateT m JetMergedEv JetMergedEv () 
leptonVeto = 
  iget >>>= \(JetMerged PhyEventClassified {..}) -> 
  let elst = filter ((>20) <$> pt.snd) electronlst 
      llst = filter ((>10) <$> pt.snd) muonlst
  in iguard ((null elst) && (null llst))

-- | missing Et cut > 160 GeV
metCut :: (MonadPlus m) => IxStateT m JetMergedEv JetMergedEv () 
metCut = 
  iget >>>= \(JetMerged PhyEventClassified {..}) ->
  iguard ((snd.phiptmet) met > 160)



mTauBJetMerge :: (MonadPlus m) => IxStateT m RawEv JetMergedEv ()
mTauBJetMerge = imodify taubjetMergeIx 


normalizeDphi :: Double -> Double -> Double 
normalizeDphi phi1 phi2 = let v = abs (phi1 - phi2)
                              nv | v > pi = 2*pi - v
                                 | otherwise = v 
                          in nv 



deltaRdist :: (MomObj a, MomObj b) => a -> b -> Double 
deltaRdist a b = let deta = eta a - eta b 
                     dphi = normalizeDphi (phi a) (phi b)
                 in sqrt (deta*deta + dphi*dphi)



findJetNearElec :: PhyEventClassified -> ([(Int,PhyObj Jet)],[(Int,PhyObj Jet)])
findJetNearElec PhyEventClassified {..} =
    let js = jetlst 
        es = electronlst 
    in pass2 0.2 es js 
 where 
   pass1 v l js = foldr (findInside v l) ([],[]) js 
     where findInside v l j (sel,rem) = 
             if deltaRdist (snd l) (snd j) < v then ((j:sel),rem) else (sel,j:rem) 
   pass2 v ls js = (,) <$> ptordering . fst <*> ptordering . snd $ foldr (pass1proc v) ([],js) ls 
     where pass1proc v l (sel,rem) = 
             let (sel',rem') = pass1 v l rem 
             in (sel++sel',rem')


findLeptonNearJet :: PhyEventClassified 
                  -> (([(Int,PhyObj Electron)],[(Int,PhyObj Electron)])
                     ,([(Int,PhyObj Muon)],[(Int,PhyObj Muon)]))
findLeptonNearJet PhyEventClassified {..} =
    let js = jetlst 
        es = electronlst 
        ms = muonlst 
        (es1,es2) = pass2 0.4 js es
        (ms1,ms2) = pass2 0.4 js ms 
    in ((es1,es2),(ms1,ms2))
  where 
   pass1 v j ls = foldr (findInside v j) ([],[]) ls 
     where findInside v j l (sel,rem) = 
             if deltaRdist (snd j) (snd l) < v then ((l:sel),rem) else (sel,l:rem) 
   pass2 v js ls = (,) <$> ptordering . fst <*> ptordering . snd $ foldr (pass1proc v) ([],ls) js 
     where pass1proc v j (sel,rem) = 
             let (sel',rem') = pass1 v j rem 
             in (sel++sel',rem')




mJetDiscardNearElec = 
  iget >>>= \(JetMerged ev) -> 
  let (sel,rem) = findJetNearElec ev 
  in (iput. JetMerged) ev {jetlst = rem}

mLeptonDiscardNearJet =
  iget >>>= \(JetMerged ev) -> 
  let ((es1,es2),(ms1,ms2)) = findLeptonNearJet ev
  in (iput . JetMerged) ev {electronlst = es2, muonlst = ms2}


{-
if length ms1 /= 0 then trace ((show.length.muonlst) ev ++ "=" ++ (show.length) ms1 ++ "+" ++ (show.length) ms2) $ (iput . JetMerged) ev {electronlst = es2, muonlst = ms2}
-}


mJesCorrection :: (MonadPlus m) => IxStateT m JetMergedEv JetMergedEv ()
mJesCorrection = imodify jes_correctionIx

jes_correctionIx :: JetMergedEv -> JetMergedEv 
jes_correctionIx (JetMerged ev@PhyEventClassified {..}) = 
  let jetlst' = ptordering
                . map ((,) <$> fst <*> jes_correction.snd) 
                $ jetlst  
  in JetMerged (ev { jetlst = jetlst' })



mMoreThan2J :: (MonadPlus m) => IxStateT m JetMergedEv MoreThan2JEv () 
mMoreThan2J = iget >>>= \(JetMerged ev@PhyEventClassified {..}) -> 
              case jetlst of 
                j1:j2:rem -> iput (Ev2J j1 j2 ev { jetlst = rem })
                _ -> imzero 



-- | First Jet > 130 GeV, second jet > 60 GeV
jetCut :: (MonadPlus m) => IxStateT m MoreThan2JEv MoreThan2JEv () 
jetCut = iget >>>= \Ev2J {..} -> 
         iguard ((pt.snd) firstJet > 130 && (pt.snd) secondJet > 60)
         

srcheckE j1 j2 j3 j4 j5 j6 tev = do 
    let c1 = dphiJMETCut1 tev
        c2 = dphiJMETCut2 tev 
        c3 = metMeffRatioCut 6 0.15 tev
    guard ((pt.snd) j6 > 40 && (pt.snd) j4 > 60 && c1 && c2 && c3 ) 
    (return . signalRegion (>1400) (>1200) (>900)) tev

srcheckD j1 j2 j3 j4 j5 tev = do 
    let c1 = dphiJMETCut1 tev
        c2 = dphiJMETCut2 tev 
        c3 = metMeffRatioCut 5 0.2 tev 
    guard ((pt.snd) j5 > 40 && (pt.snd) j4 > 60 && c1 && c2 && c3)
    (return . signalRegion (>1500) (const False) (const False)) tev

srcheckC j1 j2 j3 j4 tev = do 
    let c1 = dphiJMETCut1 tev 
        c2 = dphiJMETCut2 tev
        c3 = metMeffRatioCut 4 0.25 tev
    guard ((pt.snd) j4 > 60 && c1 && c2 && c3)
    (return . signalRegion (>1500) (>1200) (>900)) tev
 
srcheckB j1 j2 j3 tev = do 
    let c1 = dphiJMETCut1 tev
        c3 = metMeffRatioCut 3 0.25 tev 
    guard ((pt.snd) j3 > 60 && c1 && c3)
    (return . signalRegion (>1900) (const False) (const False)) tev

srcheckAandA' j1 j2 tev = 
    let v = metMeffRatioVal 2 tev 
        c1 = dphiJMETCut1 tev 
        a  = if v > 0.3 && c1
               then Just (signalRegion (>1900) (>1400) (const False) tev)
               else Nothing  
        a' = if v > 0.4 && c1 
               then Just (signalRegion (const False) (>1200) (const False) tev)
               else Nothing
    in (a,a') 


classifyChannel :: (MonadPlus m) => IxStateT m MoreThan2JEv MoreThan2JEv SRFlag  
classifyChannel = 
    iget >>>= \tev@Ev2J {..} -> 
    let rev = remainingEvent 
        js = jetlst rev
        j1 = firstJet
        j2 = secondJet 
        (mj3,mj4,mj5,mj6) = case js of 
                              j3:j4:j5:j6:j7s -> (Just j3,Just j4,Just j5,Just j6)
                              j3:j4:j5:[]     -> (Just j3,Just j4,Just j5,Nothing)
                              j3:j4:[]        -> (Just j3,Just j4,Nothing,Nothing)
                              j3:[]           -> (Just j3,Nothing,Nothing,Nothing)
                              []              -> (Nothing,Nothing,Nothing,Nothing) 

    in ireturn $ set classE ( do 
           (j3,j4,j5,j6) <- (,,,) <$> mj3 <*> mj4 <*> mj5 <*> mj6 
           srcheckE j1 j2 j3 j4 j5 j6 tev)
       ---------------------------------------------------------------------
       . set classD ( do 
           (j3,j4,j5) <- (,,) <$> mj3 <*> mj4 <*> mj5 
           srcheckD j1 j2 j3 j4 j5 tev)
       ---------------------------------------------------------------------
       . set classC ( do 
           (j3,j4) <- (,) <$> mj3 <*> mj4 
           srcheckC j1 j2 j3 j4 tev )
       ---------------------------------------------------------------------
       . set classB ( do 
           j3 <- mj3        
           srcheckB j1 j2 j3 tev )
       ---------------------------------------------------------------------
       $ let (a,a') = srcheckAandA' j1 j2 tev  
         in (set classA a . set classA' a' ) emptySRFlag 



metMeffRatioVal :: (GetJetMerged e) => 
                   Int    -- ^ num of jets in M_eff
                -> e -> Double 
metMeffRatioVal n e = 
    let JetMerged ev@PhyEventClassified {..} = getJetMerged e 
        metval = (snd . phiptmet) met 
        meffval = meffNj n ev
    in metval / meffval



metMeffRatioCut :: (GetJetMerged e) => 
                   Int    -- ^ num of jets in M_eff
                -> Double -- ^ cut value 
                -> e -> Bool 
metMeffRatioCut n cut = (>cut). metMeffRatioVal n 


dphiJMETCut1 :: (GetJetMerged e) => e -> Bool  
dphiJMETCut1 e = let JetMerged ev@PhyEventClassified {..} = getJetMerged e 
                     dphi j = normalizeDphi ((fst.phiptmet) met) ((phi.snd) j)
                 in (minimum . map dphi . take 3) jetlst > 0.4 

dphiJMETCut2 :: (GetJetMerged e) => e -> Bool 
dphiJMETCut2 e = let JetMerged ev@PhyEventClassified {..} = getJetMerged e 
                     dphi j = normalizeDphi ((fst.phiptmet) met)  ((phi.snd) j)
                     ptcut j = (pt.snd) j > 40 
                 in (minimum . map dphi . filter ptcut) jetlst > 0.2



signalRegion :: (GetJetMerged e) => 
                (Double -> Bool)   -- ^ Tight condition
             -> (Double -> Bool)   -- ^ Medium condition
             -> (Double -> Bool)   -- ^ Loose condition
             -> e 
             -> (Bool,Bool,Bool)
signalRegion condt condm condl e = 
    let JetMerged ev = getJetMerged e 
        v = meffinc40 ev
        tight  = condt v
        medium = condm v 
        loose  = condl v 
    in (tight,medium,loose)

classify :: MonadPlus m => IxStateT m RawEv MoreThan2JEv SRFlag
classify = 
    mTauBJetMerge >>>
    mJetDiscardNearElec >>> 
    mLeptonDiscardNearJet >>> 
    mJesCorrection >>> 
    objrecon      >>> 
    trigger       >>> 
    leptonVeto    >>> 
    metCut        >>> 
    mMoreThan2J   >>> 
    jetCut        >>>
    classifyChannel 

analysis ev = fst <$> runIxStateT classify (Raw ev)


