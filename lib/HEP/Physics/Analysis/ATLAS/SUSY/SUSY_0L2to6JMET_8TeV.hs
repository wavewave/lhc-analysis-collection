{-# LANGUAGE RecordWildCards #-}  
{-# LANGUAGE GADTs #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE StandaloneDeriving #-}


-----------------------------------------------------------------------------
-- |
-- Module      : HEP.Physics.Analysis.ATLAS.SUSY.SUSY_0L2to6JMET_8TeV
-- Copyright   : (c) 2013 Ian-Woo Kim
--
-- License     : GPL-3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-- LHC ATLAS SUSY search analysis code 
-- 
-- based on ATLAS-CONF-2013-047
-- 
-----------------------------------------------------------------------------

module HEP.Physics.Analysis.ATLAS.SUSY.SUSY_0L2to6JMET_8TeV where

import Codec.Compression.GZip
import Control.Applicative
import Control.Lens
import Control.Monad
import Control.Monad.Indexed
import Control.Monad.Indexed.State 
import Control.Monad.Indexed.Trans
import Control.Monad.Trans
import           Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.Aeson.Generic as G
import           Data.Aeson.Types
import qualified Data.ByteString.Lazy.Char8 as LB
import Data.Data
import Data.Function (on)
import Data.List
import Data.Maybe
import qualified Data.Map.Strict as M
import Data.Typeable 
import System.Directory 
import System.FilePath
import System.Environment
-- 
import HEP.Parser.LHCOAnalysis.PhysObj hiding (FourMomentum)
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


-- meffNj 4 


-- | effective mass with N leading jets (used in PRL87,012008 (2008))
meffNj :: Int -> PhyEventClassified -> Double 
meffNj n PhyEventClassified {..} = 
  (sum . map (trd3.etaphipt.snd)) (take n jetlst) 
  + (snd.phiptmet) met

meffinc40 :: PhyEventClassified -> Double 
meffinc40 PhyEventClassified {..} = 
  (sum . map (trd3.etaphipt.snd) . filter ((>40).trd3.etaphipt.snd)) jetlst 
  + (snd.phiptmet) met


-- | 
data SRFlag = SRFlag { sr_classA  :: Maybe (Bool,Bool,Bool) 
                     , sr_classB  :: Maybe (Bool,Bool,Bool) 
                     , sr_classC  :: Maybe (Bool,Bool,Bool)
                     , sr_classD  :: Maybe (Bool,Bool,Bool) 
                     , sr_classE  :: Maybe (Bool,Bool,Bool) 
                     } 
            deriving (Show,Eq,Ord)

classA :: Simple Lens SRFlag (Maybe (Bool,Bool,Bool))
classA = lens sr_classA (\f a -> f { sr_classA = a } )

classB :: Simple Lens SRFlag (Maybe (Bool,Bool,Bool))
classB = lens sr_classB (\f a -> f { sr_classB = a } )

classC :: Simple Lens SRFlag (Maybe (Bool,Bool,Bool))
classC = lens sr_classC (\f a -> f { sr_classC = a } )

classD :: Simple Lens SRFlag (Maybe (Bool,Bool,Bool))
classD  = lens sr_classD (\f a -> f { sr_classD = a } )

classE :: Simple Lens SRFlag (Maybe (Bool,Bool,Bool))
classE = lens sr_classE (\f a -> f { sr_classE = a } )


emptySRFlag = SRFlag Nothing Nothing Nothing Nothing Nothing 


showSR :: SRFlag -> String 
showSR SRFlag {..} = 
  maybe "" (\x->"A:"++show x++":") sr_classA 
  ++ maybe "" (\x->"B:"++show x++":") sr_classB
  ++ maybe "" (\x->"C:"++show x++":") sr_classC
  ++ maybe "" (\x->"D:"++show x++":") sr_classD
  ++ maybe "" (\x->"E:"++show x++":") sr_classE 


isTightEv :: (Bool,Bool,Bool) -> Bool 
isTightEv = fst3 

isMediumEv :: (Bool,Bool,Bool) -> Bool 
isMediumEv = snd3 
 
isLooseEv :: (Bool,Bool,Bool) -> Bool 
isLooseEv = trd3 




-- |
objrecon :: (MonadPlus m) => IxStateT m JetMergedEv JetMergedEv ()
objrecon = 
  iget >>>= \(JetMerged ev@PhyEventClassified {..}) -> 
  let elst' = filter (\(_,e)->pt e > 10 && abs (eta e) < 2.47) electronlst 
      mlst' = filter (\(_,m)->pt m > 10 && abs (eta m) < 2.4) muonlst 
      jlst' = filter (\(_,j)->pt j > 20 && abs (eta j) < 2.8) jetlst  
  in iput (JetMerged ev { electronlst = elst', muonlst = mlst', jetlst = jlst' })


-- | trigger event. hardest jet 75 GeV, MET > 55 GeV
trigger :: (MonadPlus m) => IxStateT m JetMergedEv JetMergedEv () 
trigger =
  iget >>>= \(JetMerged PhyEventClassified {..}) -> 
  iguard ((not.null) jetlst) >>>= \_ -> 
  iguard ((pt.snd.head) jetlst > 75) >>>= \_ ->
  iguard ((snd.phiptmet) met > 55) 

-- | lepton veto no electron > 20 GeV, no muon > 10 GeV
leptonVeto :: (MonadPlus m) => IxStateT m JetMergedEv JetMergedEv () 
leptonVeto = 
  iget >>>= \(JetMerged PhyEventClassified {..}) -> 
  let elst = filter ((>10) <$> pt.snd) electronlst 
      llst = filter ((>10) <$> pt.snd) muonlst
  in iguard ((null elst) && (null llst))

-- | missing Et cut > 160 GeV
metCut :: (MonadPlus m) => IxStateT m JetMergedEv JetMergedEv () 
metCut = 
  iget >>>= \(JetMerged PhyEventClassified {..}) ->
  iguard ((snd.phiptmet) met > 160)



-- mTauBJetMerge :: (MonadPlus m) => IxStateT m RawEv JetMergedEv ()
-- mTauBJetMerge = imodify taubjetMergeIx 



deltaRdist :: (MomObj a, MomObj b) => a -> b -> Double 
deltaRdist a b = let deta = eta a - eta b 
                     dphi = normalizeDphi (phi a) (phi b)
                 in sqrt (deta*deta + dphi*dphi)



findJetNearElec :: PhyEventClassified -> ([(Int,PhyObj Jet)],[(Int,PhyObj Jet)])
findJetNearElec PhyEventClassified {..} =
    let js = jetlst 
        es = electronlst 
    in pass2 0.2 es js 
 where 
   pass1 v l js = foldr (findInside v l) ([],[]) js 
     where findInside v l j (sel,rem) = 
             if deltaRdist (snd l) (snd j) < v then ((j:sel),rem) else (sel,j:rem) 
   pass2 v ls js = (,) <$> ptordering . fst <*> ptordering . snd $ foldr (pass1proc v) ([],js) ls 
     where pass1proc v l (sel,rem) = 
             let (sel',rem') = pass1 v l rem 
             in (sel++sel',rem')


findLeptonNearJet :: PhyEventClassified 
                  -> (([(Int,PhyObj Electron)],[(Int,PhyObj Electron)])
                     ,([(Int,PhyObj Muon)],[(Int,PhyObj Muon)]))
findLeptonNearJet PhyEventClassified {..} =
    let js = jetlst 
        es = electronlst 
        ms = muonlst 
        (es1,es2) = pass2 0.4 js es
        (ms1,ms2) = pass2 0.4 js ms 
    in ((es1,es2),(ms1,ms2))
  where 
   pass1 v j ls = foldr (findInside v j) ([],[]) ls 
     where findInside v j l (sel,rem) = 
             if deltaRdist (snd j) (snd l) < v then ((l:sel),rem) else (sel,l:rem) 
   pass2 v js ls = (,) <$> ptordering . fst <*> ptordering . snd $ foldr (pass1proc v) ([],ls) js 
     where pass1proc v j (sel,rem) = 
             let (sel',rem') = pass1 v j rem 
             in (sel++sel',rem')




mJetDiscardNearElec = 
  iget >>>= \(JetMerged ev) -> 
  let (sel,rem) = findJetNearElec ev 
  in (iput. JetMerged) ev {jetlst = rem}

mLeptonDiscardNearJet =
  iget >>>= \(JetMerged ev) -> 
  let ((es1,es2),(ms1,ms2)) = findLeptonNearJet ev
  in (iput . JetMerged) ev {electronlst = es2, muonlst = ms2}



-- | First Jet > 130 GeV, second jet > 60 GeV
jetCut :: (MonadPlus m) => IxStateT m MoreThan2JEv MoreThan2JEv () 
jetCut = iget >>>= \Ev2J {..} -> 
         iguard ((pt.snd) firstJet > 130 && (pt.snd) secondJet > 60)
         

srcheckE _j1 _j2 _j3 j4 _j5 j6 tev = do 
    let c1 = dphiJMETCut1 tev
        c2 = dphiJMETCut2 tev 
    guard ((pt.snd) j6 > 60 && c1 && c2 ) 
    (return . signalRegion 6 (0.25,(>1500)) (0.2,(>1200)) (0.15,(>1000))) tev

srcheckD _j1 _j2 _j3 j4 j5 tev = do 
    let c1 = dphiJMETCut1 tev
        c2 = dphiJMETCut2 tev 
    guard ((pt.snd) j5 > 60 && c1 && c2 )
    (return . signalRegion 5 (0.2,(>1600)) (0.2,(const False)) (0.2,(const False))) tev

srcheckC _j1 _j2 _j3 j4 tev = do 
    let c1 = dphiJMETCut1 tev 
        c2 = dphiJMETCut2 tev
    guard ((pt.snd) j4 > 60 && c1 && c2 )
    (return . signalRegion 4 (0.25,(>2200)) (0.25,(>1200)) (0.25,(const False))) tev
 
srcheckB _j1 _j2 j3 tev = do 
    let c1 = dphiJMETCut1 tev
    guard ((pt.snd) j3 > 60 && c1 )
    (return . signalRegion 3 (0.4,(>2200)) (0.3,(>1800)) (0.3,(const False))) tev

srcheckA _j1 _j2 tev = 
    let JetMerged e = getJetMerged tev
        metval  = (snd . phiptmet . met) e  
        ptcut j = (pt.snd) j > 40 
        ht = (sum . map (pt.snd) . filter ptcut . jetlst) e 
        v = metMeffRatioVal 2 tev 
        c1 = dphiJMETCut1 tev 
        meffincval = meffinc40 e
        bl = if v > 0.2 && c1 && meffincval > 1000 then True else False 
        bm = if metval / sqrt ht  > 15 && c1 && meffincval > 1600 then True else False 
    in if bl || bm then Just (False,bm,bl) else Nothing 


classifyChannel :: (MonadPlus m) => IxStateT m MoreThan2JEv MoreThan2JEv SRFlag  
classifyChannel = 
    iget >>>= \tev@Ev2J {..} -> 
    let rev = remainingEvent 
        js = jetlst rev
        j1 = firstJet
        j2 = secondJet 
        (mj3,mj4,mj5,mj6) = case js of 
                              j3:j4:j5:j6:_j7s -> (Just j3,Just j4,Just j5,Just j6)
                              j3:j4:j5:[]     -> (Just j3,Just j4,Just j5,Nothing)
                              j3:j4:[]        -> (Just j3,Just j4,Nothing,Nothing)
                              j3:[]           -> (Just j3,Nothing,Nothing,Nothing)
                              []              -> (Nothing,Nothing,Nothing,Nothing) 

    in ireturn $ set classE ( do 
           (j3,j4,j5,j6) <- (,,,) <$> mj3 <*> mj4 <*> mj5 <*> mj6 
           srcheckE j1 j2 j3 j4 j5 j6 tev)
       ---------------------------------------------------------------------
       . set classD ( do 
           (j3,j4,j5) <- (,,) <$> mj3 <*> mj4 <*> mj5 
           srcheckD j1 j2 j3 j4 j5 tev)
       ---------------------------------------------------------------------
       . set classC ( do 
           (j3,j4) <- (,) <$> mj3 <*> mj4 
           srcheckC j1 j2 j3 j4 tev )
       ---------------------------------------------------------------------
       . set classB ( do 
           j3 <- mj3        
           srcheckB j1 j2 j3 tev )
       ---------------------------------------------------------------------
       $ set classA (srcheckA j1 j2 tev) emptySRFlag 



metMeffRatioVal :: (GetJetMerged e) => 
                   Int    -- ^ num of jets in M_eff
                -> e -> Double 
metMeffRatioVal n e = 
    let JetMerged ev@PhyEventClassified {..} = getJetMerged e 
        metval = (snd . phiptmet) met 
        meffval = meffNj n ev
    in metval / meffval



metMeffRatioCut :: (GetJetMerged e) => 
                   Int    -- ^ num of jets in M_eff
                -> Double -- ^ cut value 
                -> e -> Bool 
metMeffRatioCut n cut = (>cut). metMeffRatioVal n 


dphiJMETCut1 :: (GetJetMerged e) => e -> Bool  
dphiJMETCut1 e = let JetMerged PhyEventClassified {..} = getJetMerged e 
                     dphi j = normalizeDphi ((fst.phiptmet) met) ((phi.snd) j)
                     ptcut j = (pt.snd) j > 40 
                 in (minimum . map dphi . take 3 . filter ptcut ) jetlst > 0.4 

dphiJMETCut2 :: (GetJetMerged e) => e -> Bool 
dphiJMETCut2 e = let JetMerged PhyEventClassified {..} = getJetMerged e 
                     dphi j = normalizeDphi ((fst.phiptmet) met)  ((phi.snd) j)
                     ptcut j = (pt.snd) j > 40 
                 in (minimum . map dphi . filter ptcut) jetlst > 0.2



signalRegion :: (GetJetMerged e) => 
                Int -- ^ num of jets 
             -> (Double,(Double -> Bool))   -- ^ Tight condition
             -> (Double,(Double -> Bool))   -- ^ Medium condition
             -> (Double,(Double -> Bool))   -- ^ Loose condition
             -> e 
             -> (Bool,Bool,Bool)
signalRegion n (cutt,condt) (cutm,condm) (cutl,condl) e = 
    let JetMerged ev = getJetMerged e 
        v = meffinc40 ev
        tight  = metMeffRatioCut n cutt e && condt v
        medium = metMeffRatioCut n cutm e && condm v 
        loose  = metMeffRatioCut n cutl e && condl v 
    in (tight,medium,loose)

classifyM :: MonadPlus m => JESParam -> IxStateT m RawEv MoreThan2JEv SRFlag
classifyM jes = 
    imodify taubjetMergeIx >>>
    mJesCorrection jes >>>= \e -> 
    mJetDiscardNearElec >>> 
    mLeptonDiscardNearJet >>> 
    mMETRecalculate e >>>  
    objrecon      >>> 
    -- trigger       >>> 
    leptonVeto    >>> 
    metCut        >>> 
    mMoreThan2J   >>> 
    jetCut        >>>
    classifyChannel 

classify :: (Functor m, MonadPlus m) => JESParam -> PhyEventClassified -> m SRFlag 
classify jes ev = fst <$> runIxStateT (classifyM jes) (Raw ev)



data EType = AL | AM | BM | BT | CM | CT  | DT | EL | EM | ET 
             deriving (Show,Eq,Ord,Data,Typeable)

instance ToJSON EType where toJSON = G.toJSON 




srFlag2Num :: SRFlag -> [EType] 
srFlag2Num SRFlag {..} = maybe id fE sr_classE 
                         . maybe id fD sr_classD 
                         . maybe id fC sr_classC 
                         . maybe id fB sr_classB 
                         . maybe id fA sr_classA 
                         $ [] 
  
fE x = (if isTightEv x then (ET :) else id) . (if isMediumEv x then (EM :) else id) . (if isLooseEv x then (EL :) else id)

fD x = (if isTightEv x then (DT :) else id) 

fC x = (if isTightEv x then (CT :) else id) . (if isMediumEv x then (CM :) else id)

fB x = (if isTightEv x then (BT :) else id) . (if isMediumEv x then (BM :) else id) 

fA x = (if isLooseEv x then (AL :) else id) . (if isMediumEv x then (AM :) else id)  


showAsATLASPaper :: M.Map EType Double -> String
showAsATLASPaper m = 
  "AL :" ++ maybe "0" show (M.lookup AL m)      ++ "\n"
  ++ "AM : " ++ maybe "0" show (M.lookup AM m) ++ "\n"
  ++ "BM : " ++ maybe "0" show (M.lookup BM m) ++ "\n"
  ++ "BT : " ++ maybe "0" show (M.lookup BT m) ++ "\n"
  ++ "CM : " ++ maybe "0" show (M.lookup CM m) ++ "\n"
  ++ "CT : " ++ maybe "0" show (M.lookup CT m) ++ "\n" 
  ++ "DT : " ++ maybe "0" show (M.lookup DT m) ++ "\n"
  ++ "EL : " ++ maybe "0" show (M.lookup EL m) ++ "\n"
  ++ "EM : " ++ maybe "0" show (M.lookup EM m) ++ "\n"
  ++ "ET : " ++ maybe "0" show (M.lookup ET m) ++ "\n"


--------------------------------------------------------
--------------------------------------------------------
--------------------------------------------------------


type HistEType = [ (EType,Int) ]

data TotalSR a = TotalSR { numAL :: a
                         , numAM :: a
                         , numBM :: a
                         , numBT :: a 
                         , numCM :: a
                         , numCT :: a 
                         , numDT :: a 
                         , numEL :: a
                         , numEM :: a 
                         , numET :: a }


deriving instance (Show a)     => Show (TotalSR a) 
deriving instance (Eq a)       => Eq (TotalSR a)
deriving instance (Data a)     => Data (TotalSR a)
deriving instance Typeable1 TotalSR 



 
instance (Data a) => ToJSON (TotalSR a) where toJSON = G.toJSON

instance (Num a)  => Num (TotalSR a) where
  a + b = TotalSR { numAL = numAL a + numAL b   
                  , numAM = numAM a + numAM b 
                  , numBM = numBM a + numBM b 
                  , numBT = numBT a + numBT b 
                  , numCM = numCM a + numCM b 
                  , numCT = numCT a + numCT b 
                  , numDT = numDT a + numDT b 
                  , numEL = numEL a + numEL b 
                  , numEM = numEM a + numEM b 
                  , numET = numET a + numET b 
                  } 
  a * b = TotalSR { numAL = numAL a * numAL b   
                  , numAM = numAM a * numAM b 
                  , numBM = numBM a * numBM b 
                  , numBT = numBT a * numBT b 
                  , numCM = numCM a * numCM b 
                  , numCT = numCT a * numCT b 
                  , numDT = numDT a * numDT b 
                  , numEL = numEL a * numEL b 
                  , numEM = numEM a * numEM b 
                  , numET = numET a * numET b 
                  } 

  negate = id 
  abs = id
  fromInteger n = TotalSR { numAL = fromInteger n 
                          , numAM = fromInteger n
                          , numBM = fromInteger n
                          , numBT = fromInteger n
                          , numCM = fromInteger n
                          , numCT = fromInteger n
                          , numDT = fromInteger n
                          , numEL = fromInteger n
                          , numEM = fromInteger n
                          , numET = fromInteger n
                          }
  signum _ = fromInteger 1


multiplyScalar c a =  
  TotalSR { numAL = c * numAL a 
          , numAM = c * numAM a 
          , numBM = c * numBM a 
          , numBT = c * numBT a 
          , numCM = c * numCM a 
          , numCT = c * numCT a 
          , numDT = c * numDT a 
          , numEL = c * numEL a 
          , numEM = c * numEM a 
          , numET = c * numET a 
          }

mkHistogram :: [SRFlag] -> HistEType  
mkHistogram passed = 
  let lst = (map (\x->(x,1)) . concat . map srFlag2Num ) passed 
      ascmap = foldr (\(k,v) m->M.insertWith (+) k v m) M.empty lst 
  in M.toAscList ascmap



-- | as was [0..20], bs was [0..10]
atlas_8TeV_0L2to6J_bkgtest :: ([Double],[Double]) 
                           -> WebDAVConfig 
                           -> WebDAVRemoteDir 
                           -> String 
                           -> IO (Maybe ()) 
atlas_8TeV_0L2to6J_bkgtest (as,bs) wdavcfg wdavrdir bname = do 
    print bname 
    let fp = bname ++ "_pgs_events.lhco.gz"
    boolToMaybeM (doesFileExistInDAV wdavcfg wdavrdir fp) $ do 
      downloadFile False wdavcfg wdavrdir fp 
      bstr <- LB.readFile fp 
      let unzipped =decompress bstr 
          evts = parsestr unzipped 
          passed jes = (catMaybes . map (classify jes)) evts  
          asclst jes = mkHistogram (passed jes)
          testlst = [ (trace (show jes) jes, asclst jes) | a <- as, b <- bs, let jes = JESParam a b ]
      -- putStrLn "== result =="
      let jsonfn = bname ++ "_ATLAS8TeV0L2to6JBkgTest.json"
      let bstr = encodePretty testlst 
      LB.writeFile jsonfn bstr 
      uploadFile wdavcfg wdavrdir jsonfn 
      removeFile jsonfn
      removeFile fp 

      return ()
      