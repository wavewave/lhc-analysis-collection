{-# LANGUAGE RecordWildCards, GADTs, DeriveDataTypeable #-}

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
import Control.Monad.Indexed
import Control.Monad.Indexed.State
import Control.Monad.Indexed.Trans
import qualified Data.Aeson.Generic as G
import           Data.Aeson.Types
import           Data.Data
--
import HEP.Parser.LHCOAnalysis.PhysObj


(#) :: a -> (a -> b) -> b 
(#) = flip ($)

infixr 9 # 

-----------------------------
-- indexed monad utilities --
-----------------------------

(>>>) :: (IxMonad m) => m i j a -> m j k b -> m i k b 
a >>> b = a >>>= \_ -> b

infixr 9 >>> 

-- | guard for indexed monad
iguard :: IxMonadPlus m => Bool -> m i i () 
iguard True = ireturn () 
iguard False = imzero 


-- | similar to when
iwhen :: IxMonad m => Bool ->  m i i () -> m i i () 
iwhen True a = a
iwhen False _ = ireturn () 

---------------------------------------
-- merging taus and b-jets into jets --
---------------------------------------

data RawEv = Raw PhyEventClassified 

data JetMergedEv = JetMerged PhyEventClassified 

class GetJetMerged a where 
  getJetMerged :: a -> JetMergedEv

-- 

taubjetMergeIx :: RawEv -> JetMergedEv
taubjetMergeIx (Raw PhyEventClassified {..}) =  
  JetMerged (PhyEventClassified { eventid = eventid 
                     , photonlst = photonlst
                     , electronlst = electronlst
                     , muonlst = muonlst
                     , taulst = [] 
                     , jetlst = ptordering
                                ( jetlst 
                                  ++ map ((,) <$> fst <*> tau2Jet.snd) taulst 
                                  ++ map ((,) <$> fst <*> bJet2Jet.snd) bjetlst )
                     , bjetlst = []
                     , met = met })



-- 

tau2Jet :: PhyObj Tau -> PhyObj Jet
tau2Jet (ObjTau x _ _) = ObjJet x 1.777 1

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


data JESParam = JESParam { jes_a :: Double 
                         , jes_b :: Double } 
               deriving (Show,Eq,Data,Typeable,Ord)

instance ToJSON JESParam where toJSON = G.toJSON 



-- | jet energy scale correction ( our values from ttbar3 were (14.23,7.53) ) 
jes_correction :: JESParam -> PhyObj Jet -> PhyObj Jet 
jes_correction (JESParam a b) j = 
  let (j_eta,j_phi,j_pt) = etaphiptjet j 
      j_m = mjet j
      s = (a + b * j_eta * j_eta) 
          / sqrt ( j_pt*j_pt * (cosh j_eta)^2 + j_m*j_m )  
      deltapt = j_pt * s
      deltam = j_m * s  
  in -- trace (" eta, pt, s = " ++ show j_eta ++ "," ++ show j_pt ++ "," ++ show s) $ 

     j { etaphiptjet = (j_eta,j_phi,j_pt+deltapt) 
       , mjet = j_m + deltam } 


