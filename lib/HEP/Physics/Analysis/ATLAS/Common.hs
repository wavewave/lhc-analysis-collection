{-# LANGUAGE RecordWildCards, GADTs #-}

-----------------------------------------------------------------------------
-- |
-- Module      : HEP.Physics.Analysis.ATLAS.Common 
-- Copyright   : (c) 2013 Ian-Woo Kim
--
-- License     : GPL-3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-- LHC ATLAS analysis common routines
-- 
-----------------------------------------------------------------------------

module HEP.Physics.Analysis.ATLAS.Common where 

import Control.Applicative ((<$>),(<*>))
--
import HEP.Parser.LHCOAnalysis.PhysObj


tau2Jet :: PhyObj Tau -> PhyObj Jet
tau2Jet (ObjTau x _ _) = ObjJet x 1.77 1

bJet2Jet :: PhyObj BJet -> PhyObj Jet
bJet2Jet (ObjBJet x m n) = ObjJet x m n

taubjetMerge :: PhyEventClassified -> PhyEventClassified
taubjetMerge PhyEventClassified {..} = 
  PhyEventClassified { eventid = eventid 
                     , photonlst = photonlst
                     , electronlst = electronlst
                     , muonlst = muonlst
                     , taulst = [] 
                     , jetlst = ptordering
                                ( jetlst 
                                  ++ map ((,) <$> fst <*> tau2Jet.snd) taulst 
                                  ++ map ((,) <$> fst <*> bJet2Jet.snd) bjetlst )
                     , bjetlst = []
                     , met = met }


-- | transverse mass 
mt :: (Double,Double) -> (Double,Double) -> Double
mt (pt1x,pt1y) (pt2x,pt2y) = sqrt (2.0*pt1*pt2-2.0*pt1x*pt2x-2.0*pt1y*pt2y) 
  where pt1 = sqrt (pt1x*pt1x+pt1y*pt1y)
        pt2 = sqrt (pt2x*pt2x+pt2y*pt2y)
        -- cosph = (pt1x*pt2x + pt1y*pt2y)/

-- | inclusive effective mass 
meffinc :: PhyEventClassified -> Double 
meffinc PhyEventClassified {..} = 
  (sum . map (trd3.etaphiptelectron.snd)) electronlst 
  + (sum . map (trd3.etaphiptmuon.snd)) muonlst
  + (sum . map (trd3.etaphipt.snd)) jetlst 
  + (snd.phiptmet) met

-- | effective mass with 4 leading jets
meff4 :: PhyEventClassified -> Double
meff4 = meffNj 4 

-- | effective mass with N leading jets (used in PRL87,012008 (2008))
meffNj :: Int -> PhyEventClassified -> Double 
meffNj n PhyEventClassified {..} = 
  (sum . map (trd3.etaphiptelectron.snd)) electronlst 
  + (sum . map (trd3.etaphiptmuon.snd)) muonlst
  + (sum . map (trd3.etaphipt.snd)) (take n jetlst) 
  + (snd.phiptmet) met
