module HEP.Physics.Analysis.Common.Lens where

import Control.Lens
--
import HEP.Parser.LHCOAnalysis.PhysObj

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

class LensLeptons a where 
   leptons :: Simple Lens a [Lepton12Obj]

