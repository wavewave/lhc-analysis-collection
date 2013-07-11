{-# LANGUAGE RecordWildCards #-}  
{-# LANGUAGE GADTs #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}

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
import Control.Arrow ((&&&),(>>>))
import Control.Comonad
import Control.Comonad.Trans.Store
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

---------------------
-- comonad utility --
--------------------- 

cget :: Store s a -> s   
cget = pos 

cput :: Store s s -> Store s s
cput w = seek (extract w) w 

switchAfter :: (s -> s) -> (Store s s -> b) -> Store s a -> b  
switchAfter f g = (cget >>> f ) =>= cput =>= (extract >>> g) 


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
                 | BJetMerged (PhyEventNoTau,[PhyObj Jet])
--                  | SoftEv SoftEvent 
--                 | HardEv HardEvent

 
data SoftEvent = SoftUnclassified PhyEventNoTau
               | SoftPreselected PhyEventNoTau
               | SoftIsolated PhyEventNoTau

--               | Soft2Muon (PhyEventNoTau, [PhyObj Jet])
--               | Soft1Lep (PhyEventNoTau, [PhyObj Jet])


data HardEvent = HardUnclassified PhyEventNoTau
               | HardPreselected PhyEventNoTauNoBJet
               | HardIsolated PhyEventNoTauNoBJet

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

proc1ev :: PhyEventClassified -> Either String PrunedEvent
proc1ev ev = mergeTau (Unpruned ev) 
 


mkChannel :: PhyEventNoTau -> (Either String SoftEvent, Either String HardEvent) 
mkChannel e = (chanSoft (store id (SoftUnclassified e)), chanHard (store id (HardUnclassified e)))


-- | soft lepton channel  
chanSoft :: Store SoftEvent a -> Either String SoftEvent
chanSoft = cget >>> (\case SoftUnclassified e -> (Right . SoftPreselected . preselectionSoft) e 
                           _ -> Left "chanSoft : not SoftUnclassified")
                >=> (\case SoftPreselected e -> (Right . SoftIsolated . isolateLepton) e  
                           _ -> Left "chanSoft : not SoftPreselected")

-- | hard lepton channel 
chanHard :: Store HardEvent a -> Either String HardEvent
chanHard = cget >>> (\case HardUnclassified e -> (Right . HardPreselected . preselectionHard) e
                           _ -> Left "chanHard : not HardUnclassified")
                >=> (\case HardIsolated e -> (Right . HardIsolated . isolateLepton ) e
                           _ -> Left "chanHard : not HardPreselected")         



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