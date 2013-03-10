{-# LANGUAGE RecordWildCards, GADTs #-}

module HEP.Physics.Analysis.ATLAS.SUSY_MultiLepton where

import Codec.Compression.GZip
import Control.Applicative
import Control.Monad
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



data LeptonType = HardLepton | SoftLepton 
                deriving Show 


-- | inclusive effective mass 
meffinc :: PhyEventClassified -> Double 
meffinc PhyEventClassified {..} = 
  (sum . map (trd3.etaphiptelectron.snd)) electronlst 
  + (sum . map (trd3.etaphiptmuon.snd)) muonlst
  + (sum . map (trd3.etaphipt.snd)) jetlst 
  + (snd.phiptmet) met

-- | effective mass up to 4 leading jets
meff4 :: PhyEventClassified -> Double
meff4 PhyEventClassified {..}  = 
  (sum . map (trd3.etaphiptelectron.snd)) electronlst 
  + (sum . map (trd3.etaphiptmuon.snd)) muonlst
  + (sum . map (trd3.etaphipt.snd)) (take 4 jetlst) 
  + (snd.phiptmet) met





canBePreselected :: LeptonType -> PhyObj a -> Bool 
canBePreselected _ (ObjPhoton _) = False
canBePreselected typ (ObjElectron (eta,phi,pt) _) = abs eta < 2.47 && pt > pt0 
  where pt0 = case typ of 
                HardLepton -> 10 
                SoftLepton -> 7 
canBePreselected typ (ObjMuon (eta,phi,pt) _ ) = abs eta < 2.4 && pt > pt0 
  where pt0 = case typ of 
                HardLepton -> 10 
                SoftLepton -> 6 
canBePreselected _ (ObjTau _ _ _) = False 
canBePreselected _ (ObjJet (eta,phi,pt) _ _) = abs eta < 4.5 && pt > 20 
canBePreselected _ (ObjBJet (eta,phi,pt) _ _) = abs eta < 4.5 && pt > 20 
canBePreselected _ (ObjMET (phi,pt)) = True 

preselect :: LeptonType -> PhyEventClassified -> PhyEventClassified 
preselect typ PhyEventClassified {..} = 
  PhyEventClassified { eventid = eventid 
                     , photonlst = filter (canBePreselected typ.snd) photonlst
                     , electronlst = filter (canBePreselected typ.snd) electronlst
                     , muonlst = filter (canBePreselected typ.snd) muonlst
                     , taulst = filter (canBePreselected typ.snd) taulst
                     , jetlst = filter (canBePreselected typ.snd) jetlst
                     , bjetlst = filter (canBePreselected typ.snd) bjetlst
                     , met = met }
 



data JetType = ThreeJets | FourJets
             deriving (Show)

data JetType2 = M2Jet | M4Jet
                deriving (Show)

data SingleLeptonEventType = HardLeptonEvent JetType | SoftLeptonEvent
                           deriving (Show)

data EventType = SingleLeptonEvent SingleLeptonEventType | MultiLeptonEvent JetType2 
               deriving (Show)


-- data SingleLeptonType = Electron | Muon 
classifyEvent :: PhyEventClassified -> Maybe EventType
classifyEvent ev@PhyEventClassified {..} = do 
  let llst = leptonlst ev
  guard ((not.null) llst) 
  (etyp,l) <- do 
      guard (length llst >= 1)
      let (_,l) = head llst 
      etyp <- case l of 
        LO_Elec e -> do
          guard (pt e > 7) 
          if pt e < 25 
            then do guard (all (not.pass2nd SoftLepton) (tail llst))
                    return (Left SoftLepton)
            else if (all (not.pass2nd HardLepton) (tail llst)) 
                   then return (Left HardLepton)
                   else return (Right ())
        LO_Muon m -> do 
          guard (pt m > 6) 
          if pt m < 20 
            then do guard (all (not.pass2nd SoftLepton) (tail llst))
                    return (Left SoftLepton)
            else if (all (not.pass2nd HardLepton) (tail llst))
                   then return (Left HardLepton) 
                   else return (Right ())
      return (etyp,l)
  case etyp of 
    Left HardLepton -> do
      jtyp <- classifyJetsInSingleLepton ev 
      metcheck (SingleLeptonEvent (HardLeptonEvent jtyp)) l ev
      return (SingleLeptonEvent (HardLeptonEvent jtyp))
    Left SoftLepton -> do
      metcheck (SingleLeptonEvent SoftLeptonEvent) l ev
      return (SingleLeptonEvent SoftLeptonEvent)
    Right () -> do 
      jtyp <- classifyJetsInMultiLepton ev 
      metcheck (MultiLeptonEvent jtyp) l ev
      return (MultiLeptonEvent jtyp)

 where pass2nd HardLepton x = ((>10) . pt . snd) x
       pass2nd SoftLepton x = let y = snd x 
                              in case y of 
                                   LO_Elec e -> (pt e > 7) 
                                   LO_Muon m -> (pt m > 6)


metcheck :: EventType -> Lepton12Obj -> PhyEventClassified -> Maybe () 
metcheck (SingleLeptonEvent (HardLeptonEvent ThreeJets)) l ev
    = do let missing = met ev
             etmiss = (snd.phiptmet) missing
         let lpxpy = (pxpyFromPhiPT  . ((,)<$>phi<*>pt)) l
             mpxpy = (pxpyFromPhiPT . phiptmet)  missing
             mtvalue = mt lpxpy mpxpy 
             meffvalue = meff4 ev
             meffincvalue = meffinc ev
         -- trace (show (eventid ev) ++ ":  " ++ show etmiss ++ "    " ++ show mtvalue ++ "    " ++ show meffvalue) $
         guard (etmiss > 250) 
         guard (mtvalue > 100 ) 
         guard (etmiss / meffvalue > 0.3 )
         guard (meffincvalue > 1200 ) 
metcheck (SingleLeptonEvent (HardLeptonEvent FourJets)) l ev
    = do let missing = met ev
             etmiss = (snd.phiptmet) missing
         guard (etmiss > 250) 
         let lpxpy = (pxpyFromPhiPT  . ((,)<$>phi<*>pt)) l
             mpxpy = (pxpyFromPhiPT . phiptmet)  missing
             mtvalue = mt lpxpy mpxpy 
             meffvalue = meff4 ev
             meffincvalue = meffinc ev
         guard (mtvalue > 100 ) 
         guard (etmiss / meffvalue > 0.2 )
         guard (meffincvalue > 800 ) 
metcheck (SingleLeptonEvent SoftLeptonEvent) l ev 
    = do let nj = numofobj Jet ev
         guard (nj >= 2)
         guard ((pt.snd) (jetlst ev !! 0) > 130)
         guard ((pt.snd) (jetlst ev !! 1) > 25) 
         -- 
         let missing = met ev
             etmiss = (snd.phiptmet) missing
         guard (etmiss > 250) 
         let lpxpy = (pxpyFromPhiPT  . ((,)<$>phi<*>pt)) l
             mpxpy = (pxpyFromPhiPT . phiptmet)  missing
             mtvalue = mt lpxpy mpxpy 
             meffvalue = meff4 ev
         guard (mtvalue > 100 ) 
         guard (etmiss / meffvalue > 0.3 )
metcheck (MultiLeptonEvent M2Jet) l ev 
    = do let missing = met ev
             etmiss = (snd.phiptmet) missing
         guard (etmiss > 300)
metcheck (MultiLeptonEvent M4Jet) l ev 
    = do let missing = met ev
             etmiss = (snd.phiptmet) missing
             meffvalue = meff4 ev
             meffincvalue = meffinc ev
         guard (etmiss > 100)
         guard (etmiss / meffvalue > 0.2 )
         guard (meffincvalue > 650 )   

classifyJetsInSingleLepton :: PhyEventClassified -> Maybe JetType
classifyJetsInSingleLepton p@PhyEventClassified {..} = do 
    let nj = numofobj Jet p
    guard (nj >= 3)
    if nj == 3 
      then check3jet
      else if (pt.snd) (jetlst !! 3) > 80 then check4jet else check3jet 
  where check3jet = do 
          guard ((pt.snd) (jetlst !! 0) > 100)
          guard ((pt.snd) (jetlst !! 1) > 25) 
          guard ((pt.snd) (jetlst !! 2) > 25)
          return ThreeJets
        check4jet = do 
          guard ((pt.snd) (jetlst !! 0) > 80)
          guard ((pt.snd) (jetlst !! 1) > 80)
          guard ((pt.snd) (jetlst !! 2) > 80)
          guard ((pt.snd) (jetlst !! 3) > 80)
          return FourJets

classifyJetsInMultiLepton :: PhyEventClassified -> Maybe JetType2
classifyJetsInMultiLepton p@PhyEventClassified {..} = do 
    let nj = numofobj Jet p
    guard (nj >= 2)
    if nj < 4 
      then check2jet
      else if (pt.snd) (jetlst !! 2) > 50 then check4jet else check2jet 
  where check2jet = do 
          guard ((pt.snd) (jetlst !! 0) > 200)
          guard ((pt.snd) (jetlst !! 1) > 200) 
          return M2Jet
        check4jet = do 
          guard ((pt.snd) (jetlst !! 0) > 50)
          guard ((pt.snd) (jetlst !! 1) > 50)
          guard ((pt.snd) (jetlst !! 2) > 50)
          guard ((pt.snd) (jetlst !! 3) > 50)
          return M4Jet



isSingleLep3 :: EventType -> Bool 
isSingleLep3 (SingleLeptonEvent (HardLeptonEvent ThreeJets)) = True 
isSingleLep3 _ = False 

isSingleLep4 :: EventType -> Bool 
isSingleLep4 (SingleLeptonEvent (HardLeptonEvent FourJets)) = True
isSingleLep4 _ = False 

isSingleLepSoft :: EventType -> Bool 
isSingleLepSoft (SingleLeptonEvent SoftLeptonEvent) = True
isSingleLepSoft _ = False 

isMultiLep2 :: EventType -> Bool 
isMultiLep2 (MultiLeptonEvent M2Jet) = True 
isMultiLep2 _ = False 

isMultiLep4 :: EventType -> Bool 
isMultiLep4 (MultiLeptonEvent M4Jet) = True 
isMultiLep4 _ = False 
  
