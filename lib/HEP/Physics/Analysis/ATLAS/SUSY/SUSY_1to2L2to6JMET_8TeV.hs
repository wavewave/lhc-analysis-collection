{-# LANGUAGE RecordWildCards #-}  
{-# LANGUAGE GADTs #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiWayIf #-}

-----------------------------------------------------------------------------
-- |
-- Module      : HEP.Physics.Analysis.ATLAS.SUSY.SUSY_1to2L2to6JMET_8TeV
-- Copyright   : (c) 2013 Ian-Woo Kim
--
-- License     : GPL-3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-- LHC ATLAS SUSY search analysis code 
-- 
-- based on ATLAS-CONF-2013-062
-- 
-----------------------------------------------------------------------------

module HEP.Physics.Analysis.ATLAS.SUSY.SUSY_1to2L2to6JMET_8TeV where


import Control.Applicative
-- import Control.Arrow ((&&&),(>>>))
-- import Control.Comonad
-- import Control.Comonad.Trans.Store
import Control.Lens hiding ((#))
import Control.Monad
import Control.Monad.Morph
import Control.Monad.State
import Control.Monad.Trans 
import Control.Monad.Trans.Either
import Data.Default
import Data.List
import Data.Maybe
-- 
import HEP.Parser.LHCOAnalysis.PhysObj hiding (FourMomentum)
import HEP.Storage.WebDAV.Type 
-- 
import HEP.Physics.Analysis.ATLAS.Common ((#),tau2Jet, bJet2Jet, deltaRdist)
-- 
import Prelude hiding (subtract)


instance MFunctor (EitherT e) where 
  hoist nat m = EitherT (nat (runEitherT m))


-- meffNj 4 

-------------------
-- monad utility --
-------------------

guardE :: String -> Bool -> Either String ()
guardE msg b = if b then return () else Left msg 

---------------------
-- comonad utility --
--------------------- 

{-
cget :: Store s a -> s   
cget = pos 

cput :: Store s s -> Store s s
cput w = seek (extract w) w 

switchAfter :: (s -> s) -> (Store s s -> b) -> Store s a -> b  
switchAfter f g = (cget >>> f ) =>= cput =>= (extract >>> g) 
-}

-------------------------------
-- Physics Object Definition --
-------------------------------

class LensEventID a where
  eventId :: Simple Lens a Int


class LensPhotons a where
  photons :: Simple Lens a [PhyObj Photon]

class LensElectrons a where
  electrons :: Simple Lens a [PhyObj Electron]

class LensMuons a where
  muons :: Simple Lens a [PhyObj Muon]

class LensJets a where
  jets :: Simple Lens a [PhyObj Jet]
 
class LensBJets a where
  bjets :: Simple Lens a [PhyObj BJet]

class LensMissingET a where
  missingET :: Simple Lens a (PhyObj MET)

class LensJetBJets a where
   jetBJets :: Simple Lens a [JetBJetObj] 
 



{-
data ChanBJet = BJet0 | BJet1 | BJet2

data Chan356Jet = Jet3 | Jet5 | Jet6 

data ChanEMu = ChanE | ChanMu

data ChanDimuon = DimuonJ3 | DimuonJ5

data ChanType = Soft1Lep ChanBJet 
              | Soft2Muon ChanDimuon 
              | Hard1LepBinned ChanEMu Chan356Jet
              | Hard1LepInc ChanEMu Chan356Jet 
-}

data PrunedEvent = Unpruned PhyEventClassified
                 | TauMerged PhyEventNoTau
--                 | BJetMerged (PhyEventNoTau,[PhyObj Jet])
--                  | SoftEv SoftEvent 
--                 | HardEv HardEvent

-- data Soft1L1BEv
-- data Soft1L2BEv
data Soft1L3JEv
data Soft1L5JEv
data Soft2MuonEv

data Soft1L1BLowEv
data Soft1L1BHighEv

data Soft1L2BLowEv
data Soft1L2BHighEv
 
data SoftEvent =   --  SoftUnclassified PhyEventNoTau
                   --   | SoftPreselected PhyEventNoTau
                   --   | SoftIsolated PhyEventNoTau
                   Soft1Lep (Either String Soft1L1B, Either String Soft1L2B, Either String Soft1L)
                 | Soft2Muon Soft2MuonEv 
--               | Soft2Muon (PhyEventNoTau, [PhyObj Jet])
--               | Soft1Lep (PhyEventNoTau, [PhyObj Jet])


data Soft1L = Soft1L3J (Either String Soft1L3JEv) | Soft1L5J (Either String Soft1L5JEv)
data Soft1L1B = Soft1L1B (Either String Soft1L1BLowEv, Either String Soft1L1BHighEv)
data Soft1L2B = Soft1L2B (Either String Soft1L2BLowEv, Either String Soft1L2BHighEv)

data HardEvent = HardEvent (Either String HardInc, Either String HardBin)

data HardInc
data HardBin

-- data SoftEventType = DiMuonEvent | SingleLepEvent



data PhyEventNoTau = PhyEventNoTau { nt_eventId :: Int
                                   , nt_photons :: [PhyObj Photon]
                                   , nt_electrons :: [PhyObj Electron] 
                                   , nt_muons :: [PhyObj Muon]
                                   , nt_jets :: [PhyObj Jet] 
                                   , nt_bjets :: [PhyObj BJet]
                                   , nt_missingET :: PhyObj MET
                                   } 

instance Default PhyEventNoTau where
  def = PhyEventNoTau 0 [] [] [] [] [] (ObjMET (0,0))

instance LensEventID PhyEventNoTau where  
  eventId = lens nt_eventId (\f a -> f { nt_eventId = a })

instance LensPhotons PhyEventNoTau where
  photons = lens nt_photons (\f a -> f { nt_photons = a })

instance LensElectrons PhyEventNoTau where
  electrons = lens nt_electrons (\f a -> f { nt_electrons = a })

instance LensMuons PhyEventNoTau where
  muons = lens nt_muons (\f a -> f { nt_muons = a })

instance LensJets PhyEventNoTau where
  jets = lens nt_jets (\f a -> f { nt_jets = a })

instance LensBJets PhyEventNoTau where
  bjets = lens nt_bjets (\f a -> f { nt_bjets = a })

instance LensMissingET PhyEventNoTau where 
  missingET = lens nt_missingET (\f a -> f { nt_missingET = a })

instance LensJetBJets PhyEventNoTau where
  jetBJets = lens (\f -> let js = (map JO_Jet . nt_jets) f
                             bs = (map JO_BJet . nt_bjets) f 
                         in sortBy (flip ptcompare) (js ++ bs) )
                  (\f a -> let (js,bs) = foldr h ([],[]) a
                           in  f { nt_jets = sortBy (flip ptcompare) js 
                                 , nt_bjets = sortBy (flip ptcompare) bs
                                 })
    where h (JO_Jet x) (accj,accb) = (x:accj,accb)
          h (JO_BJet x) (accj,accb) = (accj,x:accb)



-- | event that already merged taus and b-jets into jets
data PhyEventNoTauNoBJet = PhyEventNoTauNoBJet { ntnb_eventId :: Int
                                               , ntnb_photons :: [PhyObj Photon]
                                               , ntnb_electrons :: [PhyObj Electron]
                                               , ntnb_muons :: [PhyObj Muon]
                                               , ntnb_jets :: [PhyObj Jet]
                                               , ntnb_missingET :: PhyObj MET
                                               }


instance Default PhyEventNoTauNoBJet where
  def = PhyEventNoTauNoBJet 0 [] [] [] [] (ObjMET (0,0))


instance LensEventID PhyEventNoTauNoBJet where  
  eventId = lens ntnb_eventId (\f a -> f { ntnb_eventId = a })

instance LensPhotons PhyEventNoTauNoBJet where
  photons = lens ntnb_photons (\f a -> f { ntnb_photons = a })

instance LensElectrons PhyEventNoTauNoBJet where
  electrons = lens ntnb_electrons (\f a -> f { ntnb_electrons = a })

instance LensMuons PhyEventNoTauNoBJet where
  muons = lens ntnb_muons (\f a -> f { ntnb_muons = a })

instance LensJets PhyEventNoTauNoBJet where
  jets = lens ntnb_jets (\f a -> f { ntnb_jets = a })


instance LensMissingET PhyEventNoTauNoBJet where 
  missingET = lens ntnb_missingET (\f a -> f { ntnb_missingET = a })

instance LensJetBJets PhyEventNoTauNoBJet where
  jetBJets = lens (map JO_Jet . ntnb_jets)
                  (\f a -> f { ntnb_jets = mapMaybe (\case JO_Jet j -> Just j; _ -> Nothing) a })

allJets :: (PhyEventNoTau,[PhyObj Jet]) -> [PhyObj Jet]
allJets = snd

phyEventNoTau :: (PhyEventNoTau, [PhyObj Jet]) -> PhyEventNoTau
phyEventNoTau = fst

mkPhyEventNoTau :: PhyEventClassified -> PhyEventNoTau
mkPhyEventNoTau PhyEventClassified {..} =
    PhyEventNoTau { nt_eventId = eventid 
                  , nt_photons = map snd photonlst
                  , nt_electrons = map snd electronlst
                  , nt_muons = map snd muonlst
                  , nt_jets = (map snd . ptordering . (jetlst ++) . map ((,) <$> fst <*> tau2Jet.snd)) taulst
                  , nt_bjets = map snd bjetlst
                  , nt_missingET = met
                  }


mergeTau :: PrunedEvent -> Either String PrunedEvent
mergeTau (Unpruned ev) = Right (TauMerged (mkPhyEventNoTau ev))
mergeTau _ = Left ("mergeTau: not Unpruned")

{-
mergeBJet :: PrunedEvent -> Either String PrunedEvent 
mergeBJet (TauMerged ev) = (Right . BJetMerged . (,) ev .  sortBy (flip ptcompare)) 
                             (view jets ev ++ map bJet2Jet (view bjets ev))
mergeBJet _ = Left ("mergeBJet: not TauMerged")
-}

proc1ev :: PhyEventClassified -> Either String (Either String SoftEvent, Either String HardEvent)
proc1ev ev = (mergeTau 
              >=> (\case TauMerged e -> Right (mkChannel e)
                         _ -> Left "proc1ev: not TauMerged") )

             (Unpruned ev) 
 


mkChannel :: PhyEventNoTau -> (Either String SoftEvent, Either String HardEvent) 
mkChannel e = (chanSoft e, chanHard e)


-- | soft lepton channel  
chanSoft :: PhyEventNoTau -> Either String SoftEvent
chanSoft = branchSoft. isolateLepton . preselectionSoft 
  
{-
         SoftUnclassified e -> (Right . SoftPreselected . preselectionSoft) e 
         _ -> Left "chanSoft : not SoftUnclassified"
  y <- case x of 
         SoftPreselected e -> (Right . SoftIsolated . isolateLepton) e  
         _ -> Left "chanSoft : not SoftPreselected"
  branchSoft y
-}
          
branchSoft :: PhyEventNoTau -> Either String SoftEvent 
branchSoft e = let (n, m) = countLeptonNumber e 
               in if | n == 1 -> Soft1Lep <$> chanSoft1Lep e 
                     | n == 2 && m == 2 -> Soft2Muon <$> chanSoft2Muon e
                     | otherwise -> Left "branchSoft : not single lep or dimuon"
  
              
  
chanSoft1Lep :: PhyEventNoTau 
             -> Either String ( Either String Soft1L1B 
                              , Either String Soft1L2B
                              , Either String Soft1L)
chanSoft1Lep e = let (_, m) = countLeptonNumber e 
                 in do soft1LepCheckPT e m
                       return (chanSoft1L1B e, chanSoft1L2B e, chanSoft1L e)


chanSoft1L1B :: PhyEventNoTau -> Either String Soft1L1B 
chanSoft1L1B e = do let (nj,nb) = countJetNumber e 
                    guardE "chanSoft1L1B: nj < 3" (nj >= 3)
                    guardE "chanSoft1L1B: nb < 1" (nb >= 1)
                    case head (view jetBJets e) of 
                      JO_BJet _ -> Left "chanSoft1L1B: leading jet is a b-jet"
                      _ -> return ()
                    soft1L1BCheckMT e
                    soft1L1BCheckRatioMETMeff e 
                    soft1L1BCheckDeltaRmin e 
                    (return . Soft1L1B) (chanSoft1L1BLow e, chanSoft1L1BHigh e)
 
chanSoft1L1BLow :: PhyEventNoTau -> Either String Soft1L1BLowEv
chanSoft1L1BLow e = do soft1L1BLowCheckPTJet e
                       soft1L1BLowCheckMET e 
                       return undefined

chanSoft1L1BHigh :: PhyEventNoTau -> Either String Soft1L1BHighEv
chanSoft1L1BHigh e = do soft1L1BHighCheckPTJet e 
                        soft1L1BHighCheckMET e 
                        return undefined




chanSoft1L2B :: PhyEventNoTau -> Either String Soft1L2B
chanSoft1L2B e = do let (nj,nb) = countJetNumber e
                    guardE "chanSoft1L2B: nj < 2" (nj >= 2)
                    guardE "chanSoft1L2B: nb /=2" (nb == 2)
                    soft1L2BCheckPTJet e 
                    soft1L2BCheckDeltaPhi e
                    (return . Soft1L2B) (chanSoft1L2BLow e, chanSoft1L2BHigh e)

chanSoft1L2BLow :: PhyEventNoTau -> Either String Soft1L2BLowEv
chanSoft1L2BLow e = do soft1L2BLowCheckMET e
                       soft1L2BLowCheckMCT e 
                       soft1L2BLowCheckHT2 e
                       return undefined

chanSoft1L2BHigh :: PhyEventNoTau -> Either String Soft1L2BHighEv
chanSoft1L2BHigh e = do soft1L2BHighCheckMET e
                        soft1L2BHighCheckMCT e
                        return undefined 
                      



chanSoft1L :: PhyEventNoTau -> Either String Soft1L
chanSoft1L e = do 
                  soft1LCheckMT e 
                  soft1LCheckRatioMETMeff e
                  let (nj,_) = countJetNumber e
                  if | nj == 3 || nj == 4 -> (return . Soft1L3J . chanSoft1L3J) e
                     | nj >= 5 -> (return . Soft1L5J . chanSoft1L5J) e 
                     | otherwise -> Left "chanSoft1L: nj < 3"

chanSoft1L3J :: PhyEventNoTau -> Either String Soft1L3JEv
chanSoft1L3J e = do soft1L3JCheckPTJet e
                    soft1L3JCheckMET e
                    soft1L3JCheckDeltaR e
                    undefined


chanSoft1L5J :: PhyEventNoTau -> Either String Soft1L5JEv 
chanSoft1L5J e = do soft1L5JCheckPTJet e
                    soft1L5JCheckMET e

                    undefined   
  

chanSoft2Muon :: PhyEventNoTau -> Either String Soft2MuonEv
chanSoft2Muon e = do soft2MuonCheckPT e 
                     soft2MuonCheckMmumu e 
                     soft2MuonCheckNJet e 
                     soft2MuonCheckPTJet e
                     soft2MuonCheckMET e
                     soft2MuonCheckMT e
                     soft2MuonCheckDeltaR e 
                     undefined  


-- | hard lepton channel 
chanHard :: PhyEventNoTau -> Either String HardEvent
chanHard e = do let e' = (isolateLepton . preselectionHard) e
                (return . HardEvent) (chanHardInc e', chanHardBin e')
             
chanHardInc :: PhyEventNoTauNoBJet -> Either String HardInc
chanHardInc = undefined 

chanHardBin :: PhyEventNoTauNoBJet -> Either String HardBin
chanHardBin = undefined












-----------------------
-- counting function -- 
-----------------------

countLeptonNumber :: PhyEventNoTau -> (Int,Int)
countLeptonNumber e = let ne = length (view electrons e)
                          nm = length (view muons e)
                      in (ne+nm,nm)


countJetNumber :: PhyEventNoTau -> (Int,Int)
countJetNumber e = let nj = length (view jets e)
                       nb = length (view bjets e)
                   in (nj+nb, nb)

--------------------
-- soft1Lep check -- 
--------------------

-- | check PT of leptons for soft1Lep channel
soft1LepCheckPT :: PhyEventNoTau -> Int -- ^ number of muons 
                -> Either String ()
soft1LepCheckPT e nmuon
  | nmuon == 0 = case view electrons e of
                   x:[] -> if (pt x > 10 && pt x < 25) then Right () else Left "soft1LepPTCheck: elec energy out of range"
                   _ -> Left "soft1LepPTCheck: no match in elec number" 
  | nmuon == 1 = case view muons e of 
                   x:[] -> if (pt x > 6 && pt x < 25) then Right () else Left "soft1LepPTCheck: muon energy out of range"
                   _ -> Left "soft1LepPTCheck: no match in muon number" 
  | otherwise = Left ("soft1LepPTCheck: nmuon = " ++ show nmuon)

--------------------
-- Soft1L1B check -- 
--------------------

-- | mT > 100
soft1L1BCheckMT :: PhyEventNoTau -> Either String ()
soft1L1BCheckMT = undefined

-- | ratio > 0.35
soft1L1BCheckRatioMETMeff :: PhyEventNoTau -> Either String ()
soft1L1BCheckRatioMETMeff = undefined

-- | DeltaR > 1.0
soft1L1BCheckDeltaRmin :: PhyEventNoTau -> Either String ()
soft1L1BCheckDeltaRmin = undefined

-- | 180 40 40 
soft1L1BLowCheckPTJet :: PhyEventNoTau -> Either String ()
soft1L1BLowCheckPTJet = undefined

-- | 180 25 25
soft1L1BHighCheckPTJet :: PhyEventNoTau -> Either String ()
soft1L1BHighCheckPTJet = undefined

-- | 250
soft1L1BLowCheckMET :: PhyEventNoTau -> Either String ()
soft1L1BLowCheckMET = undefined

-- | 300
soft1L1BHighCheckMET :: PhyEventNoTau -> Either String ()
soft1L1BHighCheckMET = undefined

---------------------
-- Soft1L2B check  -- 
---------------------

-- | > 60, > 60, < 50
soft1L2BCheckPTJet :: PhyEventNoTau -> Either String ()
soft1L2BCheckPTJet = undefined

-- | > 0.4
soft1L2BCheckDeltaPhi :: PhyEventNoTau -> Either String ()
soft1L2BCheckDeltaPhi = undefined 

-- | > 200
soft1L2BLowCheckMET :: PhyEventNoTau -> Either String ()
soft1L2BLowCheckMET = undefined

-- | > 300 
soft1L2BHighCheckMET :: PhyEventNoTau -> Either String ()
soft1L2BHighCheckMET = undefined 


-- | > 150
soft1L2BLowCheckMCT :: PhyEventNoTau -> Either String ()
soft1L2BLowCheckMCT = undefined 

-- | > 200
soft1L2BHighCheckMCT :: PhyEventNoTau -> Either String ()
soft1L2BHighCheckMCT = undefined 

-- | < 50
soft1L2BLowCheckHT2 :: PhyEventNoTau -> Either String ()
soft1L2BLowCheckHT2 = undefined 

--------------------------
-- Soft1L channel check -- 
--------------------------

soft1LCheckMT :: PhyEventNoTau -> Either String ()
soft1LCheckMT = undefined


soft1LCheckRatioMETMeff :: PhyEventNoTau -> Either String ()
soft1LCheckRatioMETMeff = undefined 


-- | > 180, > 25, > 25  
soft1L3JCheckPTJet :: PhyEventNoTau -> Either String ()
soft1L3JCheckPTJet = undefined

 
-- | > 180, > 25, > 25, > 25, > 25
soft1L5JCheckPTJet :: PhyEventNoTau -> Either String ()
soft1L5JCheckPTJet = undefined


-- | > 400
soft1L3JCheckMET :: PhyEventNoTau -> Either String ()
soft1L3JCheckMET = undefined

-- | > 300
soft1L5JCheckMET :: PhyEventNoTau -> Either String ()
soft1L5JCheckMET = undefined

-- | > 1.0
soft1L3JCheckDeltaR :: PhyEventNoTau -> Either String ()
soft1L3JCheckDeltaR = undefined


-----------------------------
-- Soft2Muon channel check -- 
-----------------------------

-- | check PT of leptons for soft2Muon channel
soft2MuonCheckPT :: PhyEventNoTau -> Either String ()
soft2MuonCheckPT e = 
    case view muons e of 
      x:[] -> if (pt x > 6 && pt x < 25) then Right () else Left "soft1LepPTCheck: muon energy out of range"
      _ -> Left "soft1LepPTCheck: no match in muon number" 


-- | > 15, mZ - 10 < < mZ+10
soft2MuonCheckMmumu :: PhyEventNoTau -> Either String ()
soft2MuonCheckMmumu e = undefined


-- | nj >= 2, nb == 0 
soft2MuonCheckNJet :: PhyEventNoTau -> Either String ()
soft2MuonCheckNJet e = undefined
    
-- | > 70, > 25
soft2MuonCheckPTJet :: PhyEventNoTau -> Either String ()
soft2MuonCheckPTJet e = undefined

-- | > 170
soft2MuonCheckMET :: PhyEventNoTau -> Either String ()
soft2MuonCheckMET = undefined

-- | > 80
soft2MuonCheckMT :: PhyEventNoTau -> Either String ()
soft2MuonCheckMT = undefined 

-- | > 1.0
soft2MuonCheckDeltaR :: PhyEventNoTau -> Either String ()
soft2MuonCheckDeltaR = undefined




-- | preselection object for soft lepton analysis
preselectionSoft :: PhyEventNoTau -> PhyEventNoTau
preselectionSoft ev = 
  let es = filter (\e -> pt e > 7  && abs (eta e) < 2.47) . view electrons $ ev  
      ms = filter (\m -> pt m > 6  && abs (eta m) < 2.4)  . view muons     $ ev
      js = filter (\j -> pt j > 20 && abs (eta j) < 2.8)  . view jets      $ ev
      bs = filter (\b -> pt b > 20 && abs (eta b) < 2.8)  . view bjets     $ ev   
  in (set electrons es . set muons ms . set jets js . set bjets bs) ev




-- | preselection object for hard lepton analysis
preselectionHard :: PhyEventNoTau -> PhyEventNoTauNoBJet
preselectionHard ev =
  let alljs = (map (\case JO_Jet j ->j ; JO_BJet b -> bJet2Jet b) . view jetBJets) ev
      es = filter (\e -> pt e > 10  && abs (eta e) < 2.47) . view electrons $ ev  
      ms = filter (\m -> pt m > 10  && abs (eta m) < 2.4)  . view muons     $ ev
      js = filter (\j -> pt j > 20 && abs (eta j) < 2.8) alljs
  in ( set electrons es 
     . set muons ms  
     . set jets js 
     . set eventId (view eventId ev)
     . set photons (view photons ev)
     . set missingET (view missingET ev) ) def


-- | lepton isolation according to ATLAS 
isolateLepton :: (LensElectrons event, LensMuons event, LensJetBJets event) => event -> event
isolateLepton ev = ( set electrons (sortBy (flip ptcompare) es') 
                   . set muons (sortBy (flip ptcompare) ms') 
                   . set jetBJets js' ) ev 
  where es = (map LO_Elec . view electrons) ev 
        ms = (map LO_Muon . view muons) ev 
        js = view jetBJets ev 
        ls = es ++ ms
        lalst = zip [1..] ls 
        jalst = zip [1..] js 
        --
        f = (,,)<$>fst<*>snd<*>flip classifyJetsInLepCone jalst . snd 
        clst = map (mkElimCmd . f) lalst  
        -- 
        g NoElim (accl,accj) = (accl,accj)
        g (ElimJet jis) (accl,accj) = (accl, jis++accj)
        g (ElimLep li) (accl,accj) = ((li:accl), accj)
        --
        (elst_l,elst_j) = (((,)<$>nub.fst<*>nub.snd) . foldr g ([],[])) clst 
        ls' = (map snd . filter (not . flip elem elst_l . fst)) lalst
        js' = (map snd . filter (not . flip elem elst_j . fst)) jalst
        -- 
        h (LO_Elec x) (acce,accm) = (x:acce,accm)
        h (LO_Muon x) (acce,accm) = (acce,x:accm)
        (es',ms') = foldr h ([],[]) ls'
        

data ConePos = InInner | BetweenInnerNOuter | OutOuter
             deriving (Show, Eq, Ord)

data ElimCmd = ElimLep Int | ElimJet [Int] | NoElim

conePos :: (MomObj a, MomObj b) => a -> b -> ConePos
conePos x y | deltaRdist x y < 0.2 = InInner
            | deltaRdist x y >=0.2 && deltaRdist x y < 0.4 = BetweenInnerNOuter 
            | otherwise = OutOuter

classifyJetsInLepCone :: Lepton12Obj -> [(Int,JetBJetObj)] -> ([Int],[Int],[Int])
classifyJetsInLepCone l jlst = foldr g ([],[],[]) jplst    
  where f = (,) <$> fst <*> conePos l . snd  
        -- jplst :: [(Int,ConePos)]
        jplst = map f jlst
        g (i,InInner) (acci,accio,acco) = (i:acci,accio,acco)
        g (i,BetweenInnerNOuter) (acci,accio,acco) = (acci,i:accio,acco)
        g (i,OutOuter) (acci,accio,acco) = (acci,accio,i:acco)

mkElimCmd :: (Int,Lepton12Obj,([Int],[Int],[Int])) -> ElimCmd 
mkElimCmd (i,l,(jeti,jetio,_)) = case l of
                                   LO_Elec _ -> if (not.null) jetio then ElimLep i else ElimJet jeti
                                   LO_Muon _ -> if null jeti && null jetio then NoElim else ElimLep i


                                                        
      



{-
classify :: forall m. Monad m => PhyEventClassified -> EitherT String m ()
classify ev = classifyM' >> return () 
  where -- classifyM' :: (Monad m) => EitherT String m ()
        classifyM' = hoist (flip evalStateT (Unpruned ev)) classifyM

-}

{-
checkSoft :: (Monad m) => PrunedEvent -> EitherT String m Bool 
checkSoft = \case 
               BJetMerged ev -> classifySoft ev
               _ -> left "checkSoft : not BJetMerged" 

softeventtype :: SoftEvent -> Maybe SoftEventType
softeventtype (Soft2Muon _) =  Just DiMuonEvent
softeventtype (Soft1Lep _) = Just SingleLepEvent
softeventtype _ = Nothing 

classifySoft :: SoftEvent -> Maybe SoftEventType
classifySoft e = (store softeventtype e)


cget = pos 

cput w = seek (extract w) w 

switchAfter f g = (cget >>> f ) =>= cput =>= (extract >>> g) 

classifyFlow = (switchAfter objrecon_soft 
                 (dimuon &&& soft1lep))
               &&& 
               (switchAfter objrecon_hard 
                 (binned &&& inclusive))

dimuon = dimuon_3jet ||| dimuon_5jet

singlelep = singlelep_bjet1 ||| singlelep_bjet2 

{- 
softflow = switchAfter objrecon_soft 
             (cget >>> (dimuon &&& singlelep))
-}

{-
hardflow = switchAfter objrecon_hard 
             (cget >>> (binned &&& inclusive))
-}



classifySoft2muon :: SoftEvent -> Bool 
classifySoft2muon e = True

classifySoft1Lep :: SoftEvent -> Bool
classifySoft1Lep e = True 


checkHard :: (Monad m) => PrunedEvent -> EitherT String m Bool
checkHard e = e # \case  
                BJetMerged ev -> return True
                _ -> left "checkHard : not BJetMerged"
   

 
classifyM :: Monad m => EitherT String (StateT PrunedEvent m) (Bool,Bool)
classifyM = get >>= mergeTau >>= mergeBJet >>= put >> do 
              e <- get 
              s <- checkSoft e 
              h <- checkHard e
              return (s,h)

classify :: forall m. Monad m => PhyEventClassified -> EitherT String m ()
classify ev = classifyM' >> return () 
  where -- classifyM' :: (Monad m) => EitherT String m ()
        classifyM' = hoist (flip evalStateT (Unpruned ev)) classifyM

-- runStateT classifyM (Unpruned ev)) >> return ()

-}


{-




-- |
preSelectionSoft :: (MonadPlus m) => IxStateT TauMergedEv TauMergedEv ()
preSelectionSoft = 
  iget >>>= \(TauMerged ev@PhyEventClassified {..}) -> 
  let elst' = filter (\(_,e)->pt e > 7 && abs (eta e) < 2.47) electronlst 
      mlst' = filter (\(_,m)->pt m > 6 && abs (eta m) < 2.4) muonlst 
      jlst' = filter (\(_,j)->pt j > 20 && abs (eta j) < 2.8) jetlst
      blst' = filter (\(_,b)->pt b > 20 && abs (eta b) < 2.8) bjetlst  
  in iput (JetMerged ev { electronlst = elst', muonlst = mlst', jetlst = jlst', bjetlst=blst' })


-- |
preSelectionHard :: (MonadPlus m) => IxStateT TauMergedEv TauMergedEv ()
preSelectionHard = 
  iget >>>= \(TauMerged ev@PhyEventClassified {..}) -> 
  let elst' = filter (\(_,e)->pt e > 10 && abs (eta e) < 2.47) electronlst 
      mlst' = filter (\(_,m)->pt m > 10 && abs (eta m) < 2.4) muonlst 
      jlst' = filter (\(_,j)->pt j > 20 && abs (eta j) < 2.8) jetlst
      blst' = filter (\(_,b)->pt b > 20 && abs (eta b) < 2.8) bjetlst  
  in iput (JetMerged ev { electronlst = elst', muonlst = mlst', jetlst = jlst', bjetlst=blst' })




classifyM :: MonadPlus m => IxStateT m RawEv TauMergedEv ()
classifyM = 
    imodify tauMergeIx >>>
    objrecon     



classify :: (Functor m, MonadPlus m) => PhyEventClassified -> m ()
classify ev = fst <$> runIxStateT classifyM (Raw ev)







------------------- 
-- main analysis --
-------------------

-- | 
atlas_SUSY_1to2L2to6JMET_8TeV :: ([Double],[Double]) 
                              -> WebDAVConfig 
                              -> WebDAVRemoteDir 
                              -> String 
                              -> IO (Maybe ()) 
atlas_SUSY_1to2L2to6JMET_8TeV (as,bs) wdavcfg wdavrdir bname = do return Nothing
{-    print bname 
    let fp = bname ++ "_pgs_events.lhco.gz"
    boolToMaybeM (doesFileExistInDAV wdavcfg wdavrdir fp) $ do 
      downloadFile False wdavcfg wdavrdir fp 
      bstr <- LB.readFile fp 
      let unzipped =decompress bstr 
          evts = parsestr unzipped 
          passed jes = (catMaybes . map (classify jes)) evts  
          asclst jes = mkHistogram (passed jes)
          testlst = [ (trace (show jes) jes, asclst jes) | a <- as, b <- bs, let jes = JESParam a b ]
      let jsonfn = bname ++ "_ATLAS_1to2L2to6JMET_8TeV.json"
      let bstr = encodePretty testlst 
      LB.writeFile jsonfn bstr 
      uploadFile wdavcfg wdavrdir jsonfn 
      removeFile jsonfn
      removeFile fp 

      return ()
 -}

-}