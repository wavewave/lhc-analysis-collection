module HEP.Physics.Analysis.Common.PhyEventNoTau where

import Control.Lens
import Data.Default
import Data.List (sortBy)
--
import HEP.Parser.LHCOAnalysis.PhysObj
--
import HEP.Physics.Analysis.Common.Lens

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

instance LensLeptons PhyEventNoTau where
  leptons = lens (\f -> let es = (map LO_Elec . nt_electrons) f
                            ms = (map LO_Muon . nt_muons) f 
                        in sortBy (flip ptcompare) (es ++ ms) )
                 (\f a -> let (es,ms) = foldr h ([],[]) a
                          in  f { nt_electrons = sortBy (flip ptcompare) es 
                                , nt_muons = sortBy (flip ptcompare) ms
                                })
    where h (LO_Elec x) (acce,accm) = (x:acce,accm)
          h (LO_Muon x) (acce,accm) = (acce,x:accm)

