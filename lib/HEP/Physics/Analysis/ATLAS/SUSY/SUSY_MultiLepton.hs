{-# LANGUAGE RecordWildCards, GADTs, DeriveDataTypeable #-}

module HEP.Physics.Analysis.ATLAS.SUSY.SUSY_MultiLepton where

import Codec.Compression.GZip
import Control.Applicative
import Control.Monad
import           Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.Aeson.Generic as G
import           Data.Aeson.Types
import qualified Data.ByteString.Lazy.Char8 as LB
import Data.Data
import Data.Function (on)
import Data.List
import Data.Maybe
import System.Directory 
import System.FilePath
import System.Environment
-- 
import HEP.Parser.LHCOAnalysis.PhysObj
import HEP.Parser.LHCOAnalysis.Parse
import HEP.Storage.WebDAV.CURL
import HEP.Storage.WebDAV.Type 
import HEP.Util.Either 
import HEP.Util.Functions hiding (fst3,snd3,trd3)
-- 
import HEP.Physics.Analysis.ATLAS.Common 
-- 
import Prelude hiding (subtract)
import Debug.Trace

data EventTypeCode = SingleHardElec3J 
                   | SingleHardMuon3J 
                   | SingleHardElec4J 
                   | SingleHardMuon4J 
                   | SingleSoftElec 
                   | SingleSoftMuon 
                   | MultiElecElec2J 
                   | MultiMuonMuon2J 
                   | MultiElecMuon2J 
                   | MultiElecElec4J 
                   | MultiMuonMuon4J 
                   | MultiElecMuon4J
                   deriving (Show,Eq,Ord,Data,Typeable)

instance ToJSON EventTypeCode where toJSON = G.toJSON 

data LeptonEnergyType = HardLepton | SoftLepton 
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





canBePreselected :: LeptonEnergyType -> PhyObj a -> Bool 
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

preselect :: LeptonEnergyType -> PhyEventClassified -> PhyEventClassified 
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
             deriving (Show,Eq)

data JetType2 = M2Jet | M4Jet
                deriving (Show,Eq)

data SingleLeptonEventType = HardLeptonEvent JetType | SoftLeptonEvent
                           deriving (Show,Eq)

data SLeptonKind = SE | SM 
                deriving (Show,Eq)

data MLeptonKind = MEE | MMM | MEM
                 deriving (Show,Eq)


data EventType = SingleLeptonEvent SingleLeptonEventType SLeptonKind | MultiLeptonEvent JetType2 MLeptonKind
               deriving (Show,Eq)


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
          let rem  = tail llst 
          if pt e < 25 
            then do guard (all (not.pass2nd SoftLepton) rem)
                    return (Left (SoftLepton,SE))
            else if (all (not.pass2nd HardLepton) rem) 
                   then return (Left (HardLepton,SE))
                   else case (snd.head) rem of 
                          LO_Elec _ -> return (Right MEE)
                          LO_Muon _ -> return (Right MEM)
        LO_Muon m -> do 
          guard (pt m > 6) 
          let rem  = tail llst 
          if pt m < 20 
            then do guard (all (not.pass2nd SoftLepton) rem)
                    return (Left (SoftLepton,SM))
            else if (all (not.pass2nd HardLepton) rem)
                   then return (Left (HardLepton,SM)) 
                   else case (snd.head) rem of 
                          LO_Elec _ -> return (Right MEM)
                          LO_Muon _ -> return (Right MMM)
      return (etyp,l)
  case etyp of 
    Left (HardLepton,lepkind) -> do
      jtyp <- classifyJetsInSingleLepton ev 
      metcheck (SingleLeptonEvent (HardLeptonEvent jtyp) lepkind) l ev
      return (SingleLeptonEvent (HardLeptonEvent jtyp) lepkind)
    Left (SoftLepton,lepkind) -> do
      metcheck (SingleLeptonEvent SoftLeptonEvent lepkind) l ev
      return (SingleLeptonEvent SoftLeptonEvent lepkind)
    Right lepkind -> do 
      jtyp <- classifyJetsInMultiLepton ev 
      metcheck (MultiLeptonEvent jtyp lepkind) l ev
      return (MultiLeptonEvent jtyp lepkind)

 where pass2nd HardLepton x = ((>10) . pt . snd) x
       pass2nd SoftLepton x = let y = snd x 
                              in case y of 
                                   LO_Elec e -> (pt e > 7) 
                                   LO_Muon m -> (pt m > 6)


metcheck :: EventType -> Lepton12Obj -> PhyEventClassified -> Maybe () 
metcheck (SingleLeptonEvent (HardLeptonEvent ThreeJets) _) l ev
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
metcheck (SingleLeptonEvent (HardLeptonEvent FourJets) _) l ev
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
metcheck (SingleLeptonEvent SoftLeptonEvent _) l ev 
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
metcheck (MultiLeptonEvent M2Jet _) l ev 
    = do let missing = met ev
             etmiss = (snd.phiptmet) missing
         guard (etmiss > 300)
metcheck (MultiLeptonEvent M4Jet _) l ev 
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



maybeSingleLep3 :: EventType -> Maybe SLeptonKind
maybeSingleLep3 (SingleLeptonEvent (HardLeptonEvent ThreeJets) t) = Just t 
maybeSingleLep3 _ = Nothing

maybeSingleLep4 :: EventType -> Maybe SLeptonKind 
maybeSingleLep4 (SingleLeptonEvent (HardLeptonEvent FourJets) t) = Just t
maybeSingleLep4 _ = Nothing

maybeSingleLepSoft :: EventType -> Maybe SLeptonKind
maybeSingleLepSoft (SingleLeptonEvent SoftLeptonEvent t) = Just t
maybeSingleLepSoft _ = Nothing 

maybeMultiLep2 :: EventType -> Maybe MLeptonKind
maybeMultiLep2 (MultiLeptonEvent M2Jet t) = Just t
maybeMultiLep2 _ = Nothing

maybeMultiLep4 :: EventType -> Maybe MLeptonKind 
maybeMultiLep4 (MultiLeptonEvent M4Jet t) = Just t
maybeMultiLep4 _ = Nothing 



jesCorr jes ev@PhyEventClassified {..} = 
  let jetlst' = ptordering . map ((,) <$> fst <*> jes_correction jes . snd) $ jetlst  
      (phi',pt') = phiptmet met 
      metmom = (0, pt'*cos phi', pt'*sin phi',0)
      photonmomsum = foldr plus (0,0,0,0) (map (fourmom.snd) photonlst) 
      electronmomsum = foldr plus (0,0,0,0) (map (fourmom.snd) electronlst) 
      muonmomsum = foldr plus (0,0,0,0) (map (fourmom.snd) muonlst) 
      jetmomsum = foldr plus (0,0,0,0) (map (fourmom.snd) jetlst')
      momsum = photonmomsum `plus` electronmomsum `plus` muonmomsum `plus` jetmomsum 
      -- remnant = metmom `plus` momsum
      missingphipt = (,) <$> trd3 <*> fst3 $ mom_2_pt_eta_phi ((0,0,0,0) `subtract` momsum)

  in  ev {jetlst = jetlst', met = ObjMET missingphipt }


-- | as was [0..20], bs was [0..10]
atlas_7TeV_MultiL2to4J :: JESParam  
                      -> WebDAVConfig 
                      -> WebDAVRemoteDir 
                      -> String 
                      -> IO (Maybe ()) 
atlas_7TeV_MultiL2to4J jes wdavcfg wdavrdir bname = do 
    print bname 
    let fp = bname ++ "_pgs_events.lhco.gz"
    boolToMaybeM (doesFileExistInDAV wdavcfg wdavrdir fp) $ do 
      downloadFile False wdavcfg wdavrdir fp 
      bstr <- LB.readFile fp 
      let unzipped = decompress bstr 
          evts = parsestr unzipped
          signalevts = map (preselect HardLepton . jesCorr jes . taubjetMerge) evts 
          classified = mapMaybe classifyEvent signalevts 
          -- 
          singlelep3 = mapMaybe maybeSingleLep3 classified
          num_he3 = (length . filter (== SE)) singlelep3
          num_hm3 = (length . filter (== SM)) singlelep3
          singlelep4 = mapMaybe maybeSingleLep4 classified
          num_he4 = (length . filter (== SE)) singlelep4
          num_hm4 = (length . filter (== SM)) singlelep4
          singlelepsoft = mapMaybe maybeSingleLepSoft classified
          num_se = (length . filter (== SE)) singlelepsoft
          num_sm = (length . filter (== SM)) singlelepsoft
          multilep2 = mapMaybe maybeMultiLep2 classified 
          num_mlee2 = (length . filter (== MEE)) multilep2
          num_mlem2 = (length . filter (== MEM)) multilep2
          num_mlmm2 = (length . filter (== MMM)) multilep2
          multilep4 = mapMaybe maybeMultiLep4 classified 
          num_mlee4 = (length . filter (== MEE)) multilep4
          num_mlem4 = (length . filter (== MEM)) multilep4
          num_mlmm4 = (length . filter (== MMM)) multilep4
          result = [ (SingleHardElec3J, num_he3)
                   , (SingleHardMuon3J, num_hm3)
                   , (SingleHardElec4J, num_he4)
                   , (SingleHardMuon4J, num_hm4)
                   , (SingleSoftElec  , num_se)
                   , (SingleSoftMuon  , num_sm)
                   , (MultiElecElec2J , num_mlee2)
                   , (MultiElecMuon2J , num_mlem2)
                   , (MultiMuonMuon2J , num_mlmm2)
                   , (MultiElecElec4J , num_mlee4)
                   , (MultiElecMuon4J , num_mlem4)
                   , (MultiMuonMuon4J , num_mlmm4) ]

      let jsonfn = bname ++ "_ATLAS7TeVMultiL2to4J.json"
      let bstr = encodePretty [(jes,result)]
      -- LB.putStrLn bstr 
      LB.writeFile jsonfn bstr 
      uploadFile wdavcfg wdavrdir jsonfn 
      -- 
      removeFile jsonfn
      removeFile fp 


{-      bstr <- LB.readFile fp 
      let unzipped =decompress bstr 
          evts = parsestr unzipped 
          passed jes = (catMaybes . map (classify jes)) evts  
          asclst jes = mkHistogram (passed jes)
          testlst = [ (trace (show jes) jes, asclst jes) | a <- as, b <- bs, let jes = JESParam a b ]
-}

      -- putStrLn "== result =="
      {-
      -}

  
