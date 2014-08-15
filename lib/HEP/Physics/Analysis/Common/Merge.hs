{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}

module HEP.Physics.Analysis.Common.Merge where

import Control.Applicative
--
import HEP.Parser.LHCOAnalysis.PhysObj
-- 

tau2Jet :: PhyObj Tau -> PhyObj Jet
tau2Jet (ObjTau x _ _) = ObjJet x 1.777 1

bJet2Jet :: PhyObj BJet -> PhyObj Jet
bJet2Jet (ObjBJet x m n) = ObjJet x m n

-- | tau treated as jets
mergeTau :: PhyEventClassified -> PhyEventClassified
mergeTau ev@PhyEventClassified {..} = 
    ev { taulst = []
       , jetlst = ptordering (jetlst ++ map ((,) <$> fst <*> tau2Jet.snd) taulst) }

mergeBJet :: PhyEventClassified -> PhyEventClassified 
mergeBJet ev@PhyEventClassified {..} =  
    ev { jetlst = ptordering (jetlst ++ map ((,) <$> fst <*> bJet2Jet.snd) bjetlst)
       , bjetlst = [] }

