{-# LANGUAGE LambdaCase #-}

module HEP.Physics.Analysis.Common.PhyEventNoTauNoBJet where

import Control.Lens
import Data.Default
import Data.List (sortBy)
import Data.Maybe (mapMaybe)
--
import HEP.Parser.LHCOAnalysis.PhysObj
--
import HEP.Physics.Analysis.Common.Lens

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


instance LensLeptons PhyEventNoTauNoBJet where
  leptons = lens (\f -> let es = (map LO_Elec . ntnb_electrons) f
                            ms = (map LO_Muon . ntnb_muons) f 
                        in sortBy (flip ptcompare) (es ++ ms) )
                 (\f a -> let (es,ms) = foldr h ([],[]) a
                          in  f { ntnb_electrons = sortBy (flip ptcompare) es 
                                , ntnb_muons = sortBy (flip ptcompare) ms
                                })
    where h (LO_Elec x) (acce,accm) = (x:acce,accm)
          h (LO_Muon x) (acce,accm) = (acce,x:accm)

