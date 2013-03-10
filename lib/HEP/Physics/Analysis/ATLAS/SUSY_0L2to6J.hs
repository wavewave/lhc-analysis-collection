{-# LANGUAGE RecordWildCards, GADTs, EmptyDataDecls #-}

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







data MoreThan2JEv = Ev2J { firstJet  :: (Int, PhyObj Jet)
                         , secondJet :: (Int, PhyObj Jet) 
                         , remainingEvent :: PhyEventClassified 
                         }

data AEv = AEv { chAJ1 :: (Int, PhyObj Jet) 
               , chAJ2 :: (Int, PhyObj Jet) 
               , chARemEv :: PhyEventClassified } 

data BEv = BEv { chBJ1 :: (Int, PhyObj Jet) 
               , chBJ2 :: (Int, PhyObj Jet) 
               , chBJ3 :: (Int, PhyObj Jet) 
               , chBRemEv :: PhyEventClassified } 

data CEv = CEv { chCJ1 :: (Int, PhyObj Jet) 
               , chCJ2 :: (Int, PhyObj Jet) 
               , chCJ3 :: (Int, PhyObj Jet) 
               , chCJ4 :: (Int, PhyObj Jet) 
               , chCRemEv :: PhyEventClassified } 

data DEv = DEv { chDJ1 :: (Int, PhyObj Jet) 
               , chDJ2 :: (Int, PhyObj Jet) 
               , chDJ3 :: (Int, PhyObj Jet) 
               , chDJ4 :: (Int, PhyObj Jet)
               , chDJ5 :: (Int, PhyObj Jet)  
               , chDRemEv :: PhyEventClassified } 

data EEv = EEv { chEJ1 :: (Int, PhyObj Jet) 
               , chEJ2 :: (Int, PhyObj Jet) 
               , chEJ3 :: (Int, PhyObj Jet) 
               , chEJ4 :: (Int, PhyObj Jet) 
               , chEJ5 :: (Int, PhyObj Jet) 
               , chEJ6 :: (Int, PhyObj Jet) 
               , chERemEv :: PhyEventClassified } 

data ChAEv = ChA AEv 

data ChA'Ev = ChA' AEv 

type SRFlag = (Bool,Bool,Bool) 

isTightEv :: SRFlag -> Bool 
isTightEv = fst3 

isMediumEv :: SRFlag -> Bool 
isMediumEv = snd3 
 
isLooseEv :: SRFlag -> Bool 
isLooseEv = trd3 




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


mMoreThan2J :: (MonadPlus m) => IxStateT m JetMergedEv MoreThan2JEv () 
mMoreThan2J = iget >>>= \(JetMerged ev@PhyEventClassified {..}) -> 
              case jetlst of 
                j1:j2:rem -> iput (Ev2J j1 j2 ev { jetlst = rem })
                _ -> imzero 



-- | First Jet > 130 GeV, second jet > 60 GeV
jetCut :: (MonadPlus m) => IxStateT m MoreThan2JEv MoreThan2JEv () 
jetCut = iget >>>= \Ev2J {..} -> 
         iguard ((pt.snd) firstJet > 130 && (pt.snd) secondJet > 60)
         


classifyChannel :: (MonadPlus m) => 
                   IxStateT m AEv j b
                -> IxStateT m BEv j b 
                -> IxStateT m CEv j b 
                -> IxStateT m DEv j b 
                -> IxStateT m EEv j b 
                -> IxStateT m MoreThan2JEv j b 
classifyChannel ma mb mc md me = 
    iget >>>= \Ev2J {..} -> 
    let rev = remainingEvent 
        js = jetlst rev
        j1 = firstJet
        j2 = secondJet 
     in case js of 
         j3:j4:j5:j6:j7s -> 
           let act | (pt.snd) j6 > 40 && (pt.snd) j4 > 60 = 
                       iput (EEv j1 j2 j3 j4 j5 j6 rev {jetlst=j7s}) >>> me
                   | (pt.snd) j6 <= 40 && (pt.snd) j5 > 40 && (pt.snd) j4 > 60 =
                       iput (DEv j1 j2 j3 j4 j5 rev {jetlst=j6:j7s}) >>> md
                   | (pt.snd) j5 <= 40 && (pt.snd) j4 > 60 = 
                       iput (CEv j1 j2 j3 j4 rev {jetlst=j5:j6:j7s}) >>> mc
                   | (pt.snd) j4 <= 60 && (pt.snd) j3 > 60 = 
                       iput (BEv j1 j2 j3 rev {jetlst=j4:j5:j6:j7s}) >>> mb
                   | otherwise =
                       iput (AEv j1 j2 rev) >>> ma 
           in act 
         j3:j4:j5:[] -> 
           let act | (pt.snd) j5 > 40 && (pt.snd) j4 > 60 = 
                       iput (DEv j1 j2 j3 j4 j5 rev {jetlst=[]}) >>> md
                   | (pt.snd) j5 <= 40 && (pt.snd) j4 > 60 = 
                       iput (CEv j1 j2 j3 j4 rev {jetlst=[j5]}) >>> mc 
                   | (pt.snd) j5 <= 40 && (pt.snd) j4 <= 60 = 
                       iput (BEv j1 j2 j3 rev {jetlst=[j4,j5]}) >>> mb
                   | otherwise =
                       iput (AEv j1 j2 rev) >>> ma 
           in act 
         j3:j4:[] -> 
           let act | (pt.snd) j4 > 60 =
                       iput (CEv j1 j2 j3 j4 rev {jetlst=[]}) >>> mc
                   | (pt.snd) j3 > 60 && (pt.snd) j4 <= 60 = 
                       iput (BEv j1 j2 j3 rev {jetlst=[j4]}) >>> mb 
                   | otherwise = 
                       iput (AEv j1 j2 rev) >>> ma
           in act 
         j3:[] -> 
           let act | (pt.snd) j3 > 60 =
                       iput (BEv j1 j2 j3 rev {jetlst=[]}) >>> mb 
                   | otherwise =
                       iput (AEv j1 j2 rev) >>> ma 
           in act 
         [] -> iput (AEv j1 j2 rev) >>> ma 
                  

metMeffRatioVal :: (GetJetMerged e) => 
                   Int    -- ^ num of jets in M_eff
                -> e -> Double 
metMeffRatioVal n e = 
    let JetMerged ev@PhyEventClassified {..} = getJetMerged e 
        metval = (snd . phiptmet) met 
        meffval = meffNj n ev
    in metval / meffval



metMeffRatioCut :: (MonadPlus m, GetJetMerged e) => 
                   Int    -- ^ num of jets in M_eff
                -> Double -- ^ cut value 
                -> IxStateT m e e ()
metMeffRatioCut n cut = iget >>>= iguard . (>cut). metMeffRatioVal n


dphiJMETCut1 :: (MonadPlus m, GetJetMerged e) => IxStateT m e e () 
dphiJMETCut1 = iget >>>= \e -> 
               let JetMerged ev@PhyEventClassified {..} = getJetMerged e 
                   getdphi j = let v = abs ((fst.phiptmet) met - (phi.snd) j)
                                   nv | v > 2.0*pi = v - 2.0*pi
                                      | otherwise = v 
                               in nv 
                   cond1 = (minimum . map getdphi . take 3) jetlst > 0.4 
               in iguard cond1 

dphiJMETCut2 :: (MonadPlus m, GetJetMerged e) => IxStateT m e e () 
dphiJMETCut2 = iget >>>= \e -> 
               let JetMerged ev@PhyEventClassified {..} = getJetMerged e 
                   getdphi j = let v = abs ((fst.phiptmet) met - (phi.snd) j)
                                   nv | v > 2.0*pi = v - 2.0*pi
                                      | otherwise = v 
                               in nv 
                   ptcut j = (pt.snd) j > 40 
                   cond2 = (minimum . map getdphi . filter ptcut) jetlst > 0.2
               in iguard cond2 

               
splitAandA' :: (MonadPlus m) => 
               IxStateT m ChAEv j b 
            -> IxStateT m ChA'Ev j b
            -> IxStateT m AEv j b 
splitAandA' ma ma' = iget >>>= \e -> 
                     let v = metMeffRatioVal 2 e 
                         act | v > 0.4 = iput (ChA' e) >>> ma'
                             | v <= 0.4 && v > 0.3 = iput (ChA e) >>> ma 
                             | otherwise = imzero 
                     in act 
                        

signalRegion :: (MonadPlus m, GetJetMerged e) => 
                (Double -> Bool)   -- ^ Tight condition
             -> (Double -> Bool)   -- ^ Medium condition
             -> (Double -> Bool)   -- ^ Loose condition
             -> IxStateT m e e SRFlag
signalRegion condt condm condl = 
    iget >>>= \e -> 
    let JetMerged ev = getJetMerged e 
        v = meffinc40 ev
        tight  = condt v
        medium = condm v 
        loose  = condl v 
        flag = (tight,medium,loose)
    in ireturn flag 
    -- iguard (flag /= (False,False,False) ) >>> ireturn flag



classify :: MonadPlus m => IxStateT m RawEv String ()
classify = 
    mTauBJetMerge >>> 
    trigger       >>> 
    leptonVeto    >>> 
    metCut        >>> 
    mMoreThan2J   >>> 
    jetCut        >>>
    classifyChannel 
      (dphiJMETCut1 >>> 
       splitAandA' 
         (signalRegion (>1900) (>1400) (const False)>>>= \r->iput ("A:"++show r))
         (signalRegion (const False) (>1200) (const False)>>>= \r->iput ("A':"++show r)))
      (dphiJMETCut1 >>> 
       metMeffRatioCut 3 0.25 >>> 
       signalRegion (>1900) (const False) (const False)>>>= \r->iput ("B:"++show r)) 

      (dphiJMETCut1 >>> 
       dphiJMETCut2 >>> 
       metMeffRatioCut 4 0.25 >>> 
       signalRegion (>1500) (>1200) (>900) >>>= \r->iput ("C:"++show r)) 
      (dphiJMETCut1 >>> 
       dphiJMETCut2 >>> 
       metMeffRatioCut 5 0.2  >>> 
       signalRegion (>1500) (const False) (const False) >>>= \r->iput ("D:"++show r))
      (dphiJMETCut1 >>> 
       dphiJMETCut2 >>> 
       metMeffRatioCut 6 0.15 >>> 
       signalRegion (>1400) (>1200) (>900) >>>= \r->iput ("E:"++show r))


analysis ev = runIxStateT classify (Raw ev)


