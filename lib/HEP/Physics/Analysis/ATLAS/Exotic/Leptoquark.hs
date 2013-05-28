{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE MultiWayIf #-}

-----------------------------------------------------------------------------
-- |
-- Module      : HEP.Physics.Analysis.ATLAS.Exotic.Leptoquark
-- Copyright   : (c) 2013 Ian-Woo Kim
--
-- License     : GPL-3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-- LHC ATLAS Exotic/Leptoquark analysis code
-- 
-- 
-- 
-----------------------------------------------------------------------------

module HEP.Physics.Analysis.ATLAS.Exotic.Leptoquark where 

import Codec.Compression.GZip
import Control.Applicative ((<$>),(<*>))
import Control.Monad (MonadPlus(..))
import Control.Monad.Indexed (ireturn,(>>>=),imzero)
import Control.Monad.Indexed.State (IxStateT(..),imodify,iget,iput)
import qualified Data.ByteString.Lazy.Char8 as LB
import           Data.Data
import           Data.Maybe (catMaybes)
import HROOT.Core
import HROOT.Hist
import HROOT.Graf
-- 
import HEP.Util.Functions
import HEP.Parser.LHCOAnalysis.PhysObj
import HEP.Parser.LHCOAnalysis.Parse (parsestr)
import HEP.Storage.WebDAV.CURL (doesFileExistInDAV,downloadFile)
import HEP.Storage.WebDAV.Type (WebDAVConfig(..), WebDAVRemoteDir(..))
import HEP.Util.Either (boolToMaybeM)
-- 
import HEP.Physics.Analysis.ATLAS.Common 
-- 
import Debug.Trace

-- | 
data EventType = EEJJ | EvJJ
               deriving (Show,Eq,Data,Typeable)

-- |
data EEJJEv = EEJJEv { eejj_e1 :: (Int, PhyObj Electron) 
                     , eejj_e2 :: (Int, PhyObj Electron)
                     , eejj_j1 :: (Int, PhyObj Jet) 
                     , eejj_j2 :: (Int, PhyObj Jet) 
                     , eejj_rev :: PhyEventClassified
                     } 

-- | 
instance GetJetMerged EEJJEv where 
  getJetMerged EEJJEv {..} = JetMerged eejj_rev { electronlst = [eejj_e1,eejj_e2]
                                                , jetlst = eejj_j1 : eejj_j2 : (jetlst eejj_rev)
                                                }

-- |
data EvJJEv = EvJJEv { evjj_e1 :: (Int, PhyObj Electron)
                     , evjj_j1 :: (Int, PhyObj Jet)
                     , evjj_j2 :: (Int, PhyObj Jet)
                     , evjj_rev :: PhyEventClassified
                     } 

-- | 
instance GetJetMerged EvJJEv where
  getJetMerged EvJJEv {..} = JetMerged evjj_rev { electronlst = [evjj_e1]
                                                , jetlst = evjj_j1 : evjj_j2 : (jetlst evjj_rev) }

data Discriminant = DiscrimEEJJ { discEEJJ_mee :: Double  
                                , discEEJJ_ST  :: Double 
                                , discEEJJ_mlq :: Double
                                } 
                  | DiscrimEvJJ { discEvJJ_mT :: Double 
                                , discEvJJ_ST :: Double 
                                , discEvJJ_MLQ :: Double 
                                , discEvJJ_mTLQ :: Double 
                                } 
                  deriving (Show)

isEEJJ :: Discriminant -> Bool 
isEEJJ DiscrimEEJJ {..} = True 
isEEJJ _ = False

isEvJJ :: Discriminant -> Bool 
isEvJJ DiscrimEvJJ {..} = True 
isEvJJ _ = False 

findMee :: EEJJEv -> Double 
findMee EEJJEv {..} = invmass ((fourmom.snd) eejj_e1) ((fourmom.snd) eejj_e2)

findSTfromEEJJ :: EEJJEv -> Double 
findSTfromEEJJ EEJJEv {..} = (pt.snd) eejj_e1 + (pt.snd) eejj_e2 + (pt.snd) eejj_j1 + (pt.snd) eejj_j2

findSTfromEvJJ :: EvJJEv -> Double
findSTfromEvJJ EvJJEv {..} = (pt.snd) evjj_e1 + (pt.snd) evjj_j1 + (pt.snd) evjj_j2 + (snd. phiptmet . met) evjj_rev 

findMLQfromEEJJ :: EEJJEv -> Double
findMLQfromEEJJ EEJJEv {..} = 
  let mlq1_1 = invmass ((fourmom.snd) eejj_e1) ((fourmom.snd) eejj_j1)
      mlq1_2 = invmass ((fourmom.snd) eejj_e2) ((fourmom.snd) eejj_j2)
      mlq2_1 = invmass ((fourmom.snd) eejj_e1) ((fourmom.snd) eejj_j2)
      mlq2_2 = invmass ((fourmom.snd) eejj_e2) ((fourmom.snd) eejj_j1)
      dmlq1 = abs (mlq1_1 - mlq1_2) 
      dmlq2 = abs (mlq2_1 - mlq2_2) 
  in if dmlq1 < dmlq2 then 0.5*(mlq1_1+mlq1_2) else 0.5*(mlq2_1+mlq2_2)

findMT :: EvJJEv -> Double 
findMT EvJJEv {..} = 
  let (_,e1) = evjj_e1  
      (metphi,metpt) = (phiptmet . met) evjj_rev  
      ptmet = (metpt*cos metphi, metpt*sin metphi)
      pte = (pt e1 * cos (phi e1), pt e1 * sin (phi e1))
  in mt pte ptmet


findMLQfromEvJJ :: EvJJEv -> (Double,Double) 
findMLQfromEvJJ EvJJEv {..} = 
  let e1 = snd evjj_e1
      j1 = snd evjj_j1
      j2 = snd evjj_j2
      (metphi,metpt) = (phiptmet . met) evjj_rev
      ptmet = (metpt*cos metphi, metpt*sin metphi)
      ptj1 = (pt j1 * cos (phi j1), pt j1 * sin (phi j1))
      ptj2 = (pt j2 * cos (phi j2), pt j2 * sin (phi j2))
      mlq1 = invmass (fourmom e1) (fourmom j1)
      mtlq1 = mt ptmet ptj2
      mlq2 = invmass (fourmom e1) (fourmom j2)
      mtlq2 = mt ptmet ptj1 
      dmlq1 = abs (mlq1 - mtlq1) 
      dmlq2 = abs (mlq2 - mtlq2) 
  in if dmlq1 < dmlq2 then (mlq1,mtlq1) else (mlq2,mtlq2)


getDiscrim4EEJJ :: EEJJEv -> Discriminant
getDiscrim4EEJJ = DiscrimEEJJ <$> findMee <*> findSTfromEEJJ <*> findMLQfromEEJJ

getDiscrim4EvJJ :: EvJJEv -> Discriminant
getDiscrim4EvJJ = DiscrimEvJJ <$> findMT <*> findSTfromEvJJ <*> fst.findMLQfromEvJJ <*> snd.findMLQfromEvJJ

        



-- | 
twoelectron :: JetMergedEv -> Bool 
twoelectron (JetMerged PhyEventClassified {..}) = length electronlst >= 2  

-- |
objrecon :: (MonadPlus m) => IxStateT m JetMergedEv JetMergedEv ()
objrecon = 
  iget >>>= \(JetMerged ev@PhyEventClassified {..}) -> 
  let elst' = filter (\(_,e)->pt e > 30 && abs (eta e) < 2.47) electronlst 
      mlst' = filter (\(_,m)->pt m > 30 && abs (eta m) < 2.4) muonlst 
      jlst' = filter (\(_,j)->pt j > 30 && abs (eta j) < 2.8) jetlst  
  in iput (JetMerged ev { electronlst = elst', muonlst = mlst', jetlst = jlst' })

muonVeto :: (MonadPlus m) => IxStateT m MoreThan2JEv MoreThan2JEv ()
muonVeto = 
    iget >>>= \ev -> 
    (iguard . null . muonlst . unJetMerged . getJetMerged ) ev

classifyElectron :: (MonadPlus m) => 
                    IxStateT m EvJJEv a () 
                 -> IxStateT m EEJJEv a ()
                 -> IxStateT m MoreThan2JEv a ()
classifyElectron evjj eejj = 
    iget >>>= \ev ->
    let rev = remainingEvent ev
        es = electronlst rev
        j1 = firstJet ev 
        j2 = secondJet ev 
    in case es of 
         e1:[]     -> iput EvJJEv { evjj_e1 = e1 
                                  , evjj_j1 = j1 
                                  , evjj_j2 = j2 
                                  , evjj_rev = rev { electronlst = [] } } 
                      >>> evjj 
         e1:e2:[]  -> iput EEJJEv { eejj_e1 = e1 
                                  , eejj_e2 = e2 
                                  , eejj_j1 = j1 
                                  , eejj_j2 = j2 
                                  , eejj_rev = rev { electronlst = [] } } 
                      >>> eejj
         _ -> imzero 

dphiJMETCut :: (GetJetMerged e) => e -> Bool  
dphiJMETCut e = let JetMerged PhyEventClassified {..} = getJetMerged e 
                    metpt = (snd.phiptmet) met 
                    dphi j = normalizeDphi ((fst.phiptmet) met) ((phi.snd) j)
                in if metpt >= 45 
                   then True 
                   else all (\j -> dphi j  > 4.5 * (1 - metpt / 45)) jetlst   


checkEEJJ :: (MonadPlus m) => IxStateT m EEJJEv Discriminant ()
checkEEJJ = iget >>>= \ev -> 
            let (_,e1) = eejj_e1 ev 
                (_,e2) = eejj_e2 ev 
                mom1 = fourmom e1 
                mom2 = fourmom e2
            in iguard (invmass mom1 mom2 > 40) >>> 
               iput (getDiscrim4EEJJ ev)


checkEvJJ :: (MonadPlus m) => IxStateT m EvJJEv Discriminant () 
checkEvJJ = iget >>>= \ev -> 
            let (_,metpt) = (phiptmet . met . evjj_rev) ev 
            in iguard (metpt > 30 && findMT ev > 40 && dphiJMETCut ev) >>> 
               iput (getDiscrim4EvJJ ev)

classifyM :: MonadPlus m => JESParam -> IxStateT m RawEv Discriminant ()
classifyM jes = 
    imodify taubjetMergeIx >>> 
    mJesCorrection jes >>>= \e -> 
    mMETRecalculate e >>>  
    objrecon >>> 
    mMoreThan2J >>> 
    muonVeto >>>
    classifyElectron checkEvJJ checkEEJJ 


classify :: (Functor m, MonadPlus m) => JESParam -> PhyEventClassified -> m Discriminant
classify jes ev = snd <$> runIxStateT (classifyM jes) (Raw ev)



atlas_7TeV_leptoquark :: ([Double],[Double]) 
                      -> WebDAVConfig 
                      -> WebDAVRemoteDir 
                      -> String 
                      -> IO (Maybe ())
atlas_7TeV_leptoquark (as,bs) wdavcfg wdavrdir bname = do 
    print bname 
    let fp = bname ++ "_pgs_events.lhco.gz"
    boolToMaybeM (doesFileExistInDAV wdavcfg wdavrdir fp) $ do 
      downloadFile False wdavcfg wdavrdir fp 
      bstr <- LB.readFile fp 
      let (a,b) = (head as, head bs)
          jesparam = JESParam a b 
          unzipped =decompress bstr 
          evts = parsestr unzipped 
          passed jes = (catMaybes . map (classify jes)) evts  
          --          testlst = [ (jes, asclst jes) | a <- as, b <- bs, let jes = JESParam a b ]
          evjjevts = filter isEvJJ (passed jesparam)
          eejjevts = filter isEEJJ (passed jesparam)
      -- print (length (passed jesparam))
      print (length evjjevts, length eejjevts)
      print (take 4 evjjevts) 
      print (take 4 eejjevts) 
      tcanvas <- newTCanvas "Test" "TEst" 640 480 
      setLogy tcanvas 1
      h1 <- newTH1D "t1" "T1" 50 0 1000
      mapM_ (fill1 h1) . map discEvJJ_mT $ evjjevts 
      draw h1 "" 
      saveAs tcanvas "test4.pdf" "" 
      delete h1
      delete tcanvas 
      return () 
