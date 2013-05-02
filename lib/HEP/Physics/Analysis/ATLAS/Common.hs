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
import Control.Monad
import Control.Monad.Indexed
import Control.Monad.Indexed.State
import Control.Monad.Indexed.Trans
import qualified Data.Aeson.Generic as G
import           Data.Aeson.Types
import           Data.Data
--
import HEP.Parser.LHCOAnalysis.PhysObj hiding (FourMomentum,fst3,snd3,trd3)
import HEP.Util.Functions
--
import Prelude hiding (subtract) 

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

newtype JetMergedEv = JetMerged { unJetMerged :: PhyEventClassified }

-- | 
data MoreThan2JEv = Ev2J { firstJet  :: (Int, PhyObj Jet)
                         , secondJet :: (Int, PhyObj Jet) 
                         , remainingEvent :: PhyEventClassified 
                         }


class GetJetMerged a where 
  getJetMerged :: a -> JetMergedEv



instance GetJetMerged MoreThan2JEv where 
  getJetMerged (Ev2J j1 j2 rev@PhyEventClassified {..}) = 
    JetMerged rev {jetlst = j1:j2:jetlst}



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

-- |  
mMoreThan2J :: (MonadPlus m) => IxStateT m JetMergedEv MoreThan2JEv () 
mMoreThan2J = iget >>>= \(JetMerged ev@PhyEventClassified {..}) -> 
              case jetlst of 
                j1:j2:rem -> iput (Ev2J j1 j2 ev { jetlst = rem })
                _ -> imzero 


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



normalizeDphi :: Double -> Double -> Double 
normalizeDphi phi1 phi2 = let v = abs (phi1 - phi2)
                              nv | v > pi = 2*pi - v
                                 | otherwise = v 
                          in nv 


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

jes_correctionIx :: JESParam -> JetMergedEv -> JetMergedEv 
jes_correctionIx jes (JetMerged ev@PhyEventClassified {..}) = 
  let jetlst' = ptordering
                . map ((,) <$> fst <*> jes_correction jes . snd) 
                $ jetlst  
  in JetMerged (ev { jetlst = jetlst' })


mJesCorrection :: (MonadPlus m) => JESParam -> IxStateT m JetMergedEv JetMergedEv FourMomentum
mJesCorrection jes = 
   iget >>>= \(JetMerged ev@PhyEventClassified {..}) -> 
   let (phi',pt') = phiptmet met 
       metmom = (0, pt'*cos phi', pt'*sin phi',0)
       photonmomsum = foldr plus (0,0,0,0) (map (fourmom.snd) photonlst) 
       electronmomsum = foldr plus (0,0,0,0) (map (fourmom.snd) electronlst) 
       muonmomsum = foldr plus (0,0,0,0) (map (fourmom.snd) muonlst) 
       jetmomsum = foldr plus (0,0,0,0) (map (fourmom.snd) jetlst)
       momsum = photonmomsum `plus` electronmomsum `plus` muonmomsum `plus` jetmomsum 
       remnant = metmom `plus` momsum
   in  -- trace ("met = " ++ show (phiptmet met) ++ " | remnant = " ++ show remnant) $ 
       imodify (jes_correctionIx jes) >>> return remnant 

mMETRecalculate :: (Monad m) => 
                   FourMomentum   -- ^ remnant 4-momentum from met
                -> IxStateT m JetMergedEv JetMergedEv () 
mMETRecalculate remnant = 
  iget >>>= \(JetMerged ev@PhyEventClassified {..}) -> 
  let photonmomsum = foldr plus (0,0,0,0) (map (fourmom.snd) photonlst) 
      electronmomsum = foldr plus (0,0,0,0) (map (fourmom.snd) electronlst) 
      muonmomsum = foldr plus (0,0,0,0) (map (fourmom.snd) muonlst) 
      jetmomsum = foldr plus (0,0,0,0) (map (fourmom.snd) jetlst)
      momsum = photonmomsum `plus` electronmomsum `plus` muonmomsum `plus` jetmomsum 
      missingphipt = (,) <$> trd3 <*> fst3 $ mom_2_pt_eta_phi (remnant `subtract` momsum) 
      nev = ev { met = ObjMET missingphipt } 
  in -- trace ("met = " ++ show (phiptmet met) ++ " | remnant = " ++ show remnant ++ " | missingphipt = " ++ show missingphipt) $
     
     iput (JetMerged nev) 
