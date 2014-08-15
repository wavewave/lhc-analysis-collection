{-# LANGUAGE RecordWildCards #-}  
{-# LANGUAGE GADTs #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiWayIf #-}

-----------------------------------------------------------------------------
-- |
-- Module      : HEP.Physics.Analysis.ATLAS.SUSY.SUSY_1to2L2to6JMET_8TeV
-- Copyright   : (c) 2013 Ian-Woo Kim
--
-- License     : GPL-3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-- LHC ATLAS SUSY search analysis code 
-- 
-- based on ATLAS-CONF-2013-062
-- 
-----------------------------------------------------------------------------

module HEP.Physics.Analysis.ATLAS.SUSY.SUSY_1to2L2to6JMET_8TeV where

import Codec.Compression.GZip
import Control.Applicative
-- import Control.Arrow ((&&&),(>>>))
-- import Control.Comonad
-- import Control.Comonad.Trans.Store
import Control.Lens hiding ((#))
import Control.Monad
import Control.Monad.State
import Control.Monad.Trans 
import Control.Monad.Trans.Either
import           Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.Aeson.Generic as G
import           Data.Aeson.Types
import qualified Data.ByteString.Lazy.Char8 as LB
import Data.Data
import Data.Default
import Data.Either
import Data.List
import Data.Maybe
import qualified Data.Map.Strict as M
import System.Directory
-- 
import HEP.Parser.LHCOAnalysis.PhysObj 
import HEP.Parser.LHCOAnalysis.Parse
import HEP.Storage.WebDAV.CURL
import HEP.Storage.WebDAV.Type
import HEP.Util.Either 
import HEP.Util.Functions (invmass)
-- 
import HEP.Physics.Analysis.Common.PhyEventNoTau
import HEP.Physics.Analysis.ATLAS.Common ((#),tau2Jet, bJet2Jet, deltaRdist,mt,normalizeDphi)
-- 
import Prelude hiding (subtract)

-------------------------
-- histogram data type --
-------------------------

data EType = S1L1BLM | S1L1BHM | S1L2BLM | S1L2BHM | S1L3J | S1L5J | S2Mu2J 
           | BH1E3J | BH1M3J | BH1E5J | BH1M5J | BH1E6J | BH1M6J 
           | IH1E3J | IH1M3J | IH1E5J | IH1M5J | IH1E6J | IH1M6J
           deriving (Show, Eq, Ord, Data, Typeable)

instance ToJSON EType where toJSON = G.toJSON

type HistEType = [ (EType,Int) ]

data TotalSR a = TotalSR { numS1L1BLM :: a
                         , numS1L1BHM :: a
                         , numS1L2BLM :: a
                         , numS1L2BHM :: a 
                         , numS1L3J :: a
                         , numS1L5J :: a 
                         , numS2Mu2J :: a 
                         , numBH1E3J :: a
                         , numBH1M3J :: a 
                         , numBH1E5J :: a 
                         , numBH1M5J :: a 
                         , numBH1E6J :: a 
                         , numBH1M6J :: a 
                         , numIH1E3J :: a
                         , numIH1M3J :: a 
                         , numIH1E5J :: a 
                         , numIH1M5J :: a 
                         , numIH1E6J :: a 
                         , numIH1M6J :: a 
                         }


deriving instance (Show a)     => Show (TotalSR a) 
deriving instance (Eq a)       => Eq (TotalSR a)
deriving instance (Data a)     => Data (TotalSR a)
deriving instance Typeable1 TotalSR 


instance (Data a) => ToJSON (TotalSR a) where toJSON = G.toJSON

instance (Num a)  => Num (TotalSR a) where
  a + b = TotalSR { numS1L1BLM = numS1L1BLM a + numS1L1BLM b   
                  , numS1L1BHM = numS1L1BHM a + numS1L1BHM b 
                  , numS1L2BLM = numS1L2BLM a + numS1L2BLM b 
                  , numS1L2BHM = numS1L2BHM a + numS1L2BHM b 
                  , numS1L3J = numS1L3J a + numS1L3J b 
                  , numS1L5J = numS1L5J a + numS1L5J b 
                  , numS2Mu2J = numS2Mu2J a + numS2Mu2J b 
                  , numBH1E3J = numBH1E3J a + numBH1E3J b 
                  , numBH1M3J = numBH1M3J a + numBH1M3J b 
                  , numBH1E5J = numBH1E5J a + numBH1E5J b 
                  , numBH1M5J = numBH1M5J a + numBH1M5J b 
                  , numBH1E6J = numBH1E6J a + numBH1E6J b 
                  , numBH1M6J = numBH1M6J a + numBH1M6J b 
                  , numIH1E3J = numIH1E3J a + numIH1E3J b 
                  , numIH1M3J = numIH1M3J a + numIH1M3J b 
                  , numIH1E5J = numIH1E5J a + numIH1E5J b 
                  , numIH1M5J = numIH1M5J a + numIH1M5J b 
                  , numIH1E6J = numIH1E6J a + numIH1E6J b 
                  , numIH1M6J = numIH1M6J a + numIH1M6J b 
                  } 
  a * b = TotalSR { numS1L1BLM = numS1L1BLM a * numS1L1BLM b   
                  , numS1L1BHM = numS1L1BHM a * numS1L1BHM b 
                  , numS1L2BLM = numS1L2BLM a * numS1L2BLM b 
                  , numS1L2BHM = numS1L2BHM a * numS1L2BHM b 
                  , numS1L3J = numS1L3J a * numS1L3J b 
                  , numS1L5J = numS1L5J a * numS1L5J b 
                  , numS2Mu2J = numS2Mu2J a * numS2Mu2J b 
                  , numBH1E3J = numBH1E3J a * numBH1E3J b 
                  , numBH1M3J = numBH1M3J a * numBH1M3J b 
                  , numBH1E5J = numBH1E5J a * numBH1E5J b 
                  , numBH1M5J = numBH1M5J a * numBH1M5J b 
                  , numBH1E6J = numBH1E6J a * numBH1E6J b 
                  , numBH1M6J = numBH1M6J a * numBH1M6J b 
                  , numIH1E3J = numIH1E3J a * numIH1E3J b 
                  , numIH1M3J = numIH1M3J a * numIH1M3J b 
                  , numIH1E5J = numIH1E5J a * numIH1E5J b 
                  , numIH1M5J = numIH1M5J a * numIH1M5J b 
                  , numIH1E6J = numIH1E6J a * numIH1E6J b 
                  , numIH1M6J = numIH1M6J a * numIH1M6J b 
                  }
  negate = id 
  abs = id
  fromInteger n = TotalSR { numS1L1BLM = fromInteger n 
                          , numS1L1BHM = fromInteger n
                          , numS1L2BLM = fromInteger n
                          , numS1L2BHM = fromInteger n
                          , numS1L3J = fromInteger n
                          , numS1L5J = fromInteger n
                          , numS2Mu2J = fromInteger n
                          , numBH1E3J = fromInteger n
                          , numBH1M3J = fromInteger n
                          , numBH1E5J = fromInteger n
                          , numBH1M5J = fromInteger n
                          , numBH1E6J = fromInteger n
                          , numBH1M6J = fromInteger n
                          , numIH1E3J = fromInteger n
                          , numIH1M3J = fromInteger n
                          , numIH1E5J = fromInteger n
                          , numIH1M5J = fromInteger n
                          , numIH1E6J = fromInteger n
                          , numIH1M6J = fromInteger n

                          }
  signum _ = fromInteger 1


mkHistogram :: [EType] -> HistEType  
mkHistogram etyps = 
  let lst = map (\x->(x,1)) etyps
      ascmap = foldr (\(k,v) m->M.insertWith (+) k v m) M.empty lst 
  in M.toAscList ascmap



multiplyScalar c a =  
  TotalSR { numS1L1BLM = c * numS1L1BLM a 
          , numS1L1BHM = c * numS1L1BHM a 
          , numS1L2BLM = c * numS1L2BLM a 
          , numS1L2BHM = c * numS1L2BHM a 
          , numS1L3J = c * numS1L3J a 
          , numS1L5J = c * numS1L5J a 
          , numS2Mu2J = c * numS2Mu2J a 
          , numBH1E3J = c * numBH1E3J a 
          , numBH1M3J = c * numBH1M3J a 
          , numBH1E5J = c * numBH1E5J a 
          , numBH1M5J = c * numBH1M5J a 
          , numBH1E6J = c * numBH1E6J a 
          , numBH1M6J = c * numBH1M5J a 
          , numIH1E3J = c * numIH1E3J a 
          , numIH1M3J = c * numIH1M3J a 
          , numIH1E5J = c * numIH1E5J a 
          , numIH1M5J = c * numIH1M5J a 
          , numIH1E6J = c * numIH1E6J a 
          , numIH1M6J = c * numIH1M6J a 
          }

-----------------------
-- utility functions --
-----------------------
 
mkTotalSR :: (Num a) => [[ (EType, a) ]] -> TotalSR a
mkTotalSR hists = TotalSR { numS1L1BLM = sumup S1L1BLM
                          , numS1L1BHM = sumup S1L1BHM
                          , numS1L2BLM = sumup S1L2BLM
                          , numS1L2BHM = sumup S1L2BHM
                          , numS1L3J = sumup S1L3J
                          , numS1L5J = sumup S1L5J
                          , numS2Mu2J = sumup S2Mu2J
                          , numBH1E3J = sumup BH1E3J
                          , numBH1M3J = sumup BH1M3J
                          , numBH1E5J = sumup BH1E5J
                          , numBH1M5J = sumup BH1M5J
                          , numBH1E6J = sumup BH1E6J
                          , numBH1M6J = sumup BH1M6J
                          , numIH1E3J = sumup IH1E3J
                          , numIH1M3J = sumup IH1M3J
                          , numIH1E5J = sumup IH1E5J
                          , numIH1M5J = sumup IH1M5J
                          , numIH1E6J = sumup IH1E6J
                          , numIH1M6J = sumup IH1M6J
                          }
  where sumup k = (sum . mapMaybe (lookup k)) hists



getRFromSR sr = 
    let r = TotalSR { numS1L1BLM = g numS1L1BLM
                    , numS1L1BHM = g numS1L1BHM
                    , numS1L2BLM = g numS1L2BLM
                    , numS1L2BHM = g numS1L2BHM
                    , numS1L3J = g numS1L3J
                    , numS1L5J = g numS1L5J
                    , numS2Mu2J = g numS2Mu2J
                    , numBH1E3J = g numBH1E3J
                    , numBH1M3J = g numBH1M3J
                    , numBH1E5J = g numBH1E5J
                    , numBH1M5J = g numBH1M5J
                    , numBH1E6J = g numBH1E6J
                    , numBH1M6J = g numBH1M6J
                    , numIH1E3J = g numIH1E3J
                    , numIH1M3J = g numIH1M3J
                    , numIH1E5J = g numIH1E5J
                    , numIH1M5J = g numIH1M5J
                    , numIH1E6J = g numIH1E6J
                    , numIH1M6J = g numIH1M6J
                    } 
    in maximumInSR r 
  where getratio f x y = f x / f y 
        g f = getratio f sr limitOfNBSM_SR

-- | this is from S_exp^95
limitOfNBSM :: [ (EType,Double) ]
limitOfNBSM = [ ( S1L1BLM, 6.9 )
              , ( S1L1BHM, 6.3 )
              , ( S1L2BLM, 13.2) 
              , ( S1L2BHM, 5.3 )
              , ( S1L3J  , 7.3 )
              , ( S1L5J  , 10.0) 
              , ( S2Mu2J , 5.9 )
              , ( BH1E3J , 20.2)
              , ( BH1M3J , 15.6)
              , ( BH1E5J , 12.6)
              , ( BH1M5J , 7.6 )
              , ( BH1E6J , 7.8 )
              , ( BH1M6J , 7.1 )
              , ( IH1E3J , 5.7 )
              , ( IH1M3J , 5.1 )
              , ( IH1E5J , 5.4 )
              , ( IH1M5J , 4.7 )
              , ( IH1E6J , 4.4 )
              , ( IH1M6J , 4.1 )
              ] 

limitOfNBSM_SR :: TotalSR Double
limitOfNBSM_SR = mkTotalSR [limitOfNBSM]



maximumInSR TotalSR{..} = 
    maximum [ numS1L1BLM, numS1L1BHM, numS1L2BLM, numS1L2BHM, numS1L3J, numS1L5J, numS2Mu2J
            , numBH1E3J, numBH1M3J, numBH1E5J, numBH1M5J, numBH1E6J, numBH1M6J 
            , numIH1E3J, numIH1M3J, numIH1E5J, numIH1M5J, numIH1E6J, numIH1M6J  
            ]


------------------------------------------
-- intermediate data types for analysis --
------------------------------------------

data PrunedEvent = Unpruned PhyEventClassified
                 | TauMerged PhyEventNoTau


data MultiLep = MultiLep1 Lepton12Obj | MultiLep2 Lepton12Obj Lepton12Obj | MultiLepOther


-- data Soft1L3J deriving Show 
-- data Soft1L5J deriving Show 
data Soft2Muon = Soft2Muon deriving Show

data Soft1L1BLow = Soft1L1BLow deriving Show 
data Soft1L1BHigh = Soft1L1BHigh deriving Show

data Soft1L2BLow = Soft1L2BLow deriving Show
data Soft1L2BHigh = Soft1L2BHigh deriving Show
 
data SoftEvent = SoftSingleLep (Either String Soft1L1B, Either String Soft1L2B, Either String Soft1L)
                 | SoftDiMuon (Either String Soft2Muon)
                 deriving Show

data Soft1L = Soft1L3J | Soft1L5J  
            deriving Show 
data Soft1L1B = Soft1L1B (Either String Soft1L1BLow, Either String Soft1L1BHigh)
              deriving Show 
data Soft1L2B = Soft1L2B (Either String Soft1L2BLow, Either String Soft1L2BHigh)
              deriving Show 

data MassRegion = LowMass | HighMass deriving (Show,Eq,Ord) 


data HardEvent = HardEvent (HardInc, Either String HardBin)
               deriving Show

data HardInc = HardInc (Either String HardInc3J, Either String HardInc5J, Either String HardInc6J)
             deriving Show 


data HardBin = HardBin3J { hardBinIsMuon :: Bool } 
             | HardBin5J { hardBinIsMuon :: Bool } 
             | HardBin6J { hardBinIsMuon :: Bool } 
             deriving Show


data HardInc3J = HardInc3J { hardInc3JIsMuon :: Bool } deriving Show 
data HardInc5J = HardInc5J { hardInc5JIsMuon :: Bool } deriving Show 
data HardInc6J = HardInc6J { hardInc6JIsMuon :: Bool } deriving Show 


data IsInclusive = Inclusive | Binned deriving (Show,Eq,Ord)

data MultiJet = MultiJet2 (JetBJetObj,JetBJetObj) [JetBJetObj]
              | MultiJet3 (JetBJetObj,JetBJetObj,JetBJetObj) [JetBJetObj]
              | MultiJet5 (JetBJetObj,JetBJetObj,JetBJetObj,JetBJetObj,JetBJetObj) [JetBJetObj]
              | MultiJet6 (JetBJetObj,JetBJetObj,JetBJetObj,JetBJetObj,JetBJetObj,JetBJetObj) [JetBJetObj]


getETypes :: Either String (Either String SoftEvent, Either String HardEvent) -> [EType]
getETypes (Left _) = []
getETypes (Right (es,eh)) =
  let result_soft = case es of 
        Left _ -> [] 
        Right (SoftSingleLep (e1b,e2b,einc)) -> 
          let r_1b = case e1b of 
                Left _ -> []
                Right (Soft1L1B (e1bl,e1bh)) -> 
                  let r_1bl = either (const []) (const [S1L1BLM]) e1bl  
                      r_1bh = either (const []) (const [S1L1BHM]) e1bh
                  in r_1bl ++ r_1bh
              r_2b = case e2b of 
                Left _ -> []
                Right (Soft1L2B (e2bl,e2bh)) ->
                  let r_2bl = either (const []) (const [S1L2BLM]) e2bl
                      r_2bh = either (const []) (const [S1L2BHM]) e2bh
                  in r_2bl ++ r_2bh   
              r_inc = case einc of 
                Left _ -> []
                Right Soft1L3J -> [S1L3J]
                Right Soft1L5J -> [S1L5J]
          in r_1b ++ r_2b ++ r_inc
        Right (SoftDiMuon e2mu) -> either (const []) (const [S2Mu2J]) e2mu
      result_hard = case eh of 
        Left _ -> []
        Right (HardEvent (HardInc (eih3, eih5, eih6), ebh)) ->
          let r_bh = either (const [])  
                       (\case HardBin3J b -> if b then [BH1M3J] else [BH1E3J]   
                              HardBin5J b -> if b then [BH1M5J] else [BH1E5J] 
                              HardBin6J b -> if b then [BH1M6J] else [BH1E6J] 
                       ) ebh 
              r_ih3 = either (const []) (\case HardInc3J b -> if b then [IH1M3J] else [IH1E3J]) eih3
              r_ih5 = either (const []) (\case HardInc5J b -> if b then [IH1M5J] else [IH1E5J]) eih5
              r_ih6 = either (const []) (\case HardInc6J b -> if b then [IH1M6J] else [IH1E6J]) eih6
          in r_ih3 ++ r_ih5 ++ r_ih6 ++ r_bh 
  in result_soft ++ result_hard



-------------------
-- monad utility --
-------------------

guardE :: String -> Bool -> Either String ()
guardE msg b = if b then return () else Left msg 


-- a little utility

-- | Z-boson mass
mZ :: Double 
mZ = 91.1876
 
missingETpT :: LensMissingET a => a -> Double
missingETpT = snd . phiptmet . view missingET  

missingETphi :: LensMissingET a => a -> Double
missingETphi = fst . phiptmet . view missingET

transverseXY :: FourMomentum -> (Double,Double)
transverseXY (p0,p1,p2,p3) = (p1,p2)

transverseXYFromPhiPT :: (Double,Double) -> (Double,Double)
transverseXYFromPhiPT (phi,pt) = (pt * cos phi, pt * sin phi)

mTlep :: (LensMissingET a) => a -> Lepton12Obj -> Double
mTlep x l = let phipt = (phiptmet . view missingET) x
                vecptmet = transverseXYFromPhiPT phipt 
                vecptlep = (transverseXY . fourmom) l  
            in mt vecptmet vecptlep 

deltaPhiMin :: (LensMissingET a, MomObj b, MomObj c) => a -> (b,c) -> Double
deltaPhiMin x (y,z) = let phimet = missingETphi x
                          dphi1 = normalizeDphi phimet (phi y)
                          dphi2 = normalizeDphi phimet (phi z)
                      in minimum [dphi1,dphi2]

deltaRMin :: (LensMissingET a, LensJetBJets a) => a -> Lepton12Obj -> Double
deltaRMin x l = minimum (map (deltaRdist l) (view jetBJets x))


-- | exclusive m_eff defined for the leading three jets in the event
meffExcl :: (PhyEventNoTauNoBJet,Lepton12Obj) -> Double
meffExcl (x,l) = let ptjsum = (sum . map pt . take 3 . view jets) x 
                 in pt l + ptjsum + missingETpT x

-- | inclusivem_eff defined for all the signal jets in the event 
meffIncl :: (LensJetBJets a, LensMissingET a) => (a,Lepton12Obj) -> Double
meffIncl (x,l) = let ptjsum = (sum . map pt . view jetBJets) x 
                 in pt l + ptjsum + missingETpT x

mCT :: (MomObj a, MomObj b) => a -> b -> Double 
mCT x y = sqrt ((x0+y0)^(2::Int) - (x1-y1)^(2::Int) - (x2-y2)^(2::Int) - (x3-y3)^(2::Int))
  where (x0,x1,x2,x3) = fourmom x
        (y0,y1,y2,y3) = fourmom y


hT2 :: (LensJetBJets a) => a -> Double 
hT2 = sum . map pt . drop 2 . view jetBJets 

isJet :: JetBJetObj -> Bool 
isJet (JO_Jet _) = True
isJet (JO_BJet _) = False

isBJet :: JetBJetObj -> Bool 
isBJet (JO_Jet _) = False
isBJet (JO_BJet _) = True


-- data SoftEventType = DiMuonEvent | SingleLepEvent







getNJets :: (LensJetBJets a) => Int -> a -> Either String MultiJet
getNJets 2 x = case view jetBJets x of 
                 j1:j2:js -> return (MultiJet2 (j1,j2) js)
                 _ -> Left "getNJets: njet < 2"
getNJets 3 x = case view jetBJets x of 
                 j1:j2:j3:js -> return (MultiJet3 (j1,j2,j3) js)
                 _ -> Left "getNJets: njet < 3"
getNJets 5 x = case view jetBJets x of 
                 j1:j2:j3:j4:j5:js -> return (MultiJet5 (j1,j2,j3,j4,j5) js)
                 _ -> Left "getNJets: njet < 5"
getNJets 6 x = case view jetBJets x of 
                 j1:j2:j3:j4:j5:j6:js -> return (MultiJet6 (j1,j2,j3,j4,j5,j6) js)
                 _ -> Left "getNJets: njet < 6"
getNJets _ x = Left "getNJets: asking for n /= 2,3,5,6"



phyEventNoTau :: (PhyEventNoTau, [PhyObj Jet]) -> PhyEventNoTau
phyEventNoTau = fst

mkPhyEventNoTau :: PhyEventClassified -> PhyEventNoTau
mkPhyEventNoTau PhyEventClassified {..} =
    PhyEventNoTau { nt_eventId = eventid 
                  , nt_photons = map snd photonlst
                  , nt_electrons = map snd electronlst
                  , nt_muons = map snd muonlst
                  , nt_jets = (map snd . ptordering . (jetlst ++) . map ((,) <$> fst <*> tau2Jet.snd)) taulst
                  , nt_bjets = map snd bjetlst
                  , nt_missingET = met
                  }


mergeTau :: PrunedEvent -> Either String PrunedEvent
mergeTau (Unpruned ev) = Right (TauMerged (mkPhyEventNoTau ev))
mergeTau _ = Left ("mergeTau: not Unpruned")



-- | main entry for one event
classify :: PhyEventClassified -> Either String (Either String SoftEvent, Either String HardEvent)
classify ev = (mergeTau 
              >=> (\case TauMerged e -> Right (mkChannel e)
                         _ -> Left "proc1ev: not TauMerged") )

             (Unpruned ev) 
 


mkChannel :: PhyEventNoTau -> (Either String SoftEvent, Either String HardEvent) 
mkChannel e = (chanSoft e, chanHard e)


-- | soft lepton channel  
chanSoft :: PhyEventNoTau -> Either String SoftEvent
chanSoft = branchSoft. isolateLepton . preselectionSoft 
  
          
branchSoft :: PhyEventNoTau -> Either String SoftEvent 
branchSoft e = let (ls, m) = countLeptonNumber e 
               in case ls of 
                    MultiLep1 l -> SoftSingleLep <$> chanSoft1Lep (e,l) 
                    MultiLep2 l1 l2 -> if m == 2 then Right $ SoftDiMuon (chanSoft2Muon (e,l1,l2)) 
                                                 else Left "branchSoft : not dimuon " 
                    _ -> Left "branchSoft : not single lep or dimuon"
  
              
  
chanSoft1Lep :: (PhyEventNoTau,Lepton12Obj)
             -> Either String ( Either String Soft1L1B 
                              , Either String Soft1L2B
                              , Either String Soft1L)
chanSoft1Lep (e,l) = let (_, m) = countLeptonNumber e 
                     in do soft1LepCheckPT e m
                           return (chanSoft1L1B (e,l), chanSoft1L2B e, chanSoft1L (e,l))


chanSoft1L1B :: (PhyEventNoTau,Lepton12Obj) -> Either String Soft1L1B 
chanSoft1L1B (ev,l) = do 
    let (nj,nb) = countJetNumber ev
    guardE "chanSoft1L1B: nj < 3" (nj >= 3)
    guardE "chanSoft1L1B: nb < 1" (nb >= 1)
    case head (view jetBJets ev) of 
      JO_BJet _ -> Left "chanSoft1L1B: leading jet is a b-jet"
      _ -> return ()

    guardE "chanSoft1L1B: mT <= 100" (mTlep ev l > 100)
    let r = missingETpT ev / meffIncl (ev,l)
    guardE "chanSoft1L1B: ETmiss/meff^incl <= 0.35" (r > 0.35)
    guardE "chanSoft1L1B: deltaR_min <= 1.0" (deltaRMin ev l > 1.0)
    (return . Soft1L1B) (chanSoft1L1BLow ev, chanSoft1L1BHigh ev)
 
chanSoft1L1BLow :: PhyEventNoTau -> Either String Soft1L1BLow
chanSoft1L1BLow ev = do 
    getNJets 3 ev >>= soft1L1BCheckPTJet LowMass
    guardE "chanSoft1L1BLow: ETmiss <= 250" (missingETpT ev > 250)
    return Soft1L1BLow

chanSoft1L1BHigh :: PhyEventNoTau -> Either String Soft1L1BHigh
chanSoft1L1BHigh ev = do 
    getNJets 3 ev >>= soft1L1BCheckPTJet HighMass
    guardE "chanSoft1L1BLow: ETmiss <= 300" (missingETpT ev > 300)
    return Soft1L1BHigh


chanSoft1L2B :: PhyEventNoTau -> Either String Soft1L2B
chanSoft1L2B ev = do 
    let (nj,nb) = countJetNumber ev
    guardE "chanSoft1L2B: nj < 2" (nj >= 2)
    guardE "chanSoft1L2B: nb /=2" (nb == 2)
    getNJets 2 ev 
      >>= soft1L2BCheckPTJet 
      >>= guardE "chanSoft1L2B: deltaphimin <= 0.4" . (>0.4) . deltaPhiMin ev 
    (return . Soft1L2B) (chanSoft1L2BLow ev, chanSoft1L2BHigh ev)

chanSoft1L2BLow :: PhyEventNoTau -> Either String Soft1L2BLow
chanSoft1L2BLow ev = do 
    guardE "chanSoft1L2BLow: ETmiss <= 200" (missingETpT ev > 200)
    case view bjets ev of
      b1:b2:[] -> guardE "chanSoft1L2BLow: mCT <= 150" (mCT b1 b2 > 150)
      _ -> Left "chanSoft1L2BLow: not exactly 2 b-jets" 
    guardE "chanSoft1L2BLow: HT_2 >= 50" (hT2 ev < 50)
    return Soft1L2BLow

chanSoft1L2BHigh :: PhyEventNoTau -> Either String Soft1L2BHigh
chanSoft1L2BHigh ev = do 
    guardE "chanSoft1L2BHigh: ETmiss <= 300" (missingETpT ev > 300)
    case view bjets ev of
      b1:b2:[] -> guardE "chanSoft1L2BHigh: mCT <= 200" (mCT b1 b2 > 200)
      _ -> Left "chanSoft1L2BHigh: not exactly 2 b-jets"
    return Soft1L2BHigh
                      



chanSoft1L :: (PhyEventNoTau,Lepton12Obj) -> Either String Soft1L
chanSoft1L (ev,l) = do 
    guardE "chanSoft1L: mT <= 100" (mTlep ev l > 100) 
    let r = missingETpT ev / meffIncl (ev,l)
    -- soft1LCheckRatioMETMeff e
    guardE "chanSoft1L: ETmiss / meff^incl <= 0.3" (r > 0.3)
    let (nj,_) = countJetNumber ev
    if | nj == 3 || nj == 4 -> chanSoft1L3J (ev,l)
       | nj >= 5 -> chanSoft1L5J ev 
       | otherwise -> Left "chanSoft1L: nj < 3"

chanSoft1L3J :: (PhyEventNoTau,Lepton12Obj) -> Either String Soft1L
chanSoft1L3J (ev,l) = do 
    getNJets 3 ev >>= soft1L3JCheckPTJet
    guardE "chanSoft1L3J: ETmiss <= 400" (missingETpT ev > 400)
    guardE "chanSoft1L3J: deltaR_min <= 1.0" (deltaRMin ev l > 1.0) 
    return Soft1L3J

chanSoft1L5J :: PhyEventNoTau -> Either String Soft1L 
chanSoft1L5J ev = do 
    getNJets 5 ev >>= soft1L5JCheckPTJet 
    guardE "chanSoft1L5J: ETmiss <= 300" (missingETpT ev > 300)
    return Soft1L5J
  

chanSoft2Muon :: (PhyEventNoTau, Lepton12Obj, Lepton12Obj) -> Either String Soft2Muon
chanSoft2Muon (ev,l1,l2) = do 
    guardE "chanSoft2Muon: pTlep <= 6 || pTLep >= 25" (pt l1 > 6 && pt l1 < 25 && pt l2 > 6 && pt l2 > 25)
    let mmumu = invmass (fourmom l1) (fourmom l2)
    guardE "chanSoft2Muon: M_mumu <= 15" (mmumu > 15)
    guardE "chanSoft2Muon: M_mumu near Z-pole" (abs (mmumu - mZ) > 10)
    getNJets 2 ev >>= soft2MuonCheckPTJet 
    guardE "chanSoft2Muon: ETmiss <= 170" (missingETpT ev > 170)
    guardE "chanSoft2Muon: mT(l2) <= 80" (mTlep ev l2 > 80)
    guardE "chanSoft2Muon: deltaR_min (l2) <= 1.0" (deltaRMin ev l2 > 1.0)
    return Soft2Muon 


-- | hard lepton channel 
chanHard :: PhyEventNoTau -> Either String HardEvent
chanHard e = do let e' = (isolateLepton . preselectionHard) e
                l <- hardCheckNLep e'
                guardE "chanHard: pT_l <= 25" (pt l > 25) 
                (return . HardEvent) (chanHardInc (e',l), chanHardBin (e',l))
             
chanHardInc :: (PhyEventNoTauNoBJet,Lepton12Obj) -> HardInc
chanHardInc (e,l) = HardInc (chanHardInc3J (e,l), chanHardInc5J (e,l), chanHardInc6J (e,l)) 

chanHardInc3J :: (PhyEventNoTauNoBJet,Lepton12Obj) -> Either String HardInc3J
chanHardInc3J (ev,l) = do 
    getNJets 3 ev >>= hardCheckPTJet Inclusive  
    let etmiss = missingETpT ev 
    guardE "chanHardInc3J: ETmiss <= 500" (etmiss > 500)
    (guardE "chanHardInc3J: mT <= 150" . (> 150)) (mTlep ev l)
    let r = etmiss / meffExcl (ev,l)
    guardE "chanHardInc3J: ETmiss / meff^excl <= 0.3" (r > 0.3) 
    guardE "chanHardInc3J: meff^incl <= 1400" (meffIncl (ev,l) > 1400)
    return (HardInc3J (isMuon l))

chanHardInc5J :: (PhyEventNoTauNoBJet,Lepton12Obj) -> Either String HardInc5J
chanHardInc5J (ev,l) = do 
    getNJets 5 ev >>= hardCheckPTJet Inclusive 
    (guardE "chanHardInc5J: ETmiss <= 300" . (> 300). missingETpT) ev
    (guardE "chanHardInc5J: mT <= 200" . (> 200)) (mTlep ev l)
    guardE "chanHardInc5J: meff^incl <= 1400" (meffIncl (ev,l) > 1400)
    return (HardInc5J (isMuon l))

chanHardInc6J :: (PhyEventNoTauNoBJet,Lepton12Obj) -> Either String HardInc6J 
chanHardInc6J (ev,l) = do 
    getNJets 6 ev >>= hardCheckPTJet Inclusive 
    (guardE "chanHardInc6J: ETmiss <= 350" . (> 350). missingETpT) ev
    (guardE "chanHardInc6J: mT <= 150" . (> 150)) (mTlep ev l)
    guardE "chanHardInc6J: meff^incl <= 600" (meffIncl (ev,l) > 600)
    return (HardInc6J (isMuon l))

chanHardBin :: (PhyEventNoTauNoBJet,Lepton12Obj) -> Either String HardBin
chanHardBin (e,l) = 
  case chanHardBin3J (e,l) of 
    Right x -> Right x
    Left str3 -> case chanHardBin5J (e,l) of 
      Right y -> Right y     
      Left str5 -> case chanHardBin6J (e,l) of 
        Right z -> Right z
        Left str6 -> Left (str3 ++ "|" ++ str5 ++ "|" ++ str6)



chanHardBin3J :: (PhyEventNoTauNoBJet,Lepton12Obj) -> Either String HardBin
chanHardBin3J (ev,l) = do 
    getNJets 3 ev >>= hardCheckPTJet Binned  
    let etmiss = missingETpT ev 
    guardE "chanHardBin3J: ETmiss <= 300" (etmiss > 300)
    guardE "chanHardBin3J: mT <= 150" (mTlep ev l > 150)
    let r = etmiss / meffExcl (ev,l)
    guardE "chanHardBin3J: ETmiss / meff^excl <= 0.3" (r > 0.3) 
    guardE "chanHardBin3J: meff^incl <= 800" (meffIncl (ev,l) > 800)
    return (HardBin3J (isMuon l))

chanHardBin5J :: (PhyEventNoTauNoBJet,Lepton12Obj) -> Either String HardBin
chanHardBin5J (ev,l) = do  
    getNJets 5 ev >>= hardCheckPTJet Binned 
    (guardE "chanHardBin5J: ETmiss <= 300" . (> 300). missingETpT) ev
    (guardE "chanHardBin5J: mT <= 150" . (> 150)) (mTlep ev l)
    guardE "chanHardBin5J: meff^incl <= 800" (meffIncl (ev,l) > 800)
    return (HardBin5J (isMuon l))


chanHardBin6J :: (PhyEventNoTauNoBJet,Lepton12Obj) -> Either String HardBin
chanHardBin6J (ev,l) = do 
    getNJets 6 ev >>= hardCheckPTJet Binned 
    (guardE "chanHardBin6J: ETmiss <= 250" . (> 250). missingETpT) ev
    (guardE "chanHardBin6J: mT <= 150" . (> 150)) (mTlep ev l)
    guardE "chanHardBin3J: meff^incl <= 600" (meffIncl (ev,l) > 600)
    return (HardBin6J (isMuon l))


-----------------------
-- counting function -- 
-----------------------

countLeptonNumber :: (LensElectrons a, LensMuons a, LensLeptons a) => a -> (MultiLep,Int)
countLeptonNumber e = let ne = length (view electrons e)
                          nm = length (view muons e)
                          ls = case view leptons e of 
                                 x1:[] -> MultiLep1 x1
                                 x1:x2:[] -> MultiLep2 x1 x2
                                 _ -> MultiLepOther 
                      in (ls,nm)


countJetNumber :: (LensJetBJets a) => a -> (Int,Int)
countJetNumber e = let alljets = view jetBJets e
                       nj = (length . filter isJet) alljets
                       nb = (length . filter isBJet) alljets
                   in (nj+nb, nb)

--------------------
-- soft1Lep check -- 
--------------------

-- | check PT of leptons for soft1Lep channel
soft1LepCheckPT :: PhyEventNoTau -> Int -- ^ number of muons 
                -> Either String ()
soft1LepCheckPT e nmuon
  | nmuon == 0 = case view electrons e of
                   x:[] -> if (pt x > 10 && pt x < 25) then Right () else Left "soft1LepPTCheck: elec energy out of range"
                   _ -> Left "soft1LepPTCheck: no match in elec number" 
  | nmuon == 1 = case view muons e of 
                   x:[] -> if (pt x > 6 && pt x < 25) then Right () else Left "soft1LepPTCheck: muon energy out of range"
                   _ -> Left "soft1LepPTCheck: no match in muon number" 
  | otherwise = Left ("soft1LepPTCheck: nmuon = " ++ show nmuon)

--------------------
-- Soft1L1B check -- 
--------------------


soft1L1BCheckPTJet :: MassRegion -> MultiJet -> Either String ()
soft1L1BCheckPTJet typ (MultiJet3 (j1,j2,j3) js) = 
    let cond = case typ of 
          LowMass -> pt j1 > 180 && pt j2 > 40 && pt j3 > 40 
          HighMass -> pt j1 > 180 && pt j2 > 25 && pt j3 > 25
    in guardE ("soft1L1BCheckPTJet : jet pT condition for " ++ show typ ++ " not satifisfied") cond
soft1L1BCheckPTJet _ _ = Left "soft1L1BCheckPTJet : other than 3 jet analysis"



---------------------
-- Soft1L2B check  -- 
---------------------

-- | > 60, > 60, < 50
soft1L2BCheckPTJet :: MultiJet -> Either String (JetBJetObj,JetBJetObj)
soft1L2BCheckPTJet (MultiJet2 (j1,j2) js) = do
    guardE ("soft1L1BCheckPTJet : jet pT condition for not satifisfied") 
      (pt j1 > 60 && pt j2 > 60 && case js of {[] -> True ; j3:js' -> pt j3 < 50 }) 
    return (j1,j2)
soft1L2BCheckPTJet _ = Left "soft1L2BCheckPTJet : other than 2 jet analysis"






--------------------------
-- Soft1L channel check -- 
--------------------------

-- | > 180, > 25, > 25  
soft1L3JCheckPTJet :: MultiJet -> Either String ()
soft1L3JCheckPTJet (MultiJet3 (j1,j2,j3) js) =
    guardE "soft1L3JCheckPTJet: jet pT condition not satisfied" (pt j1 > 180 && pt j2 > 25 && pt j3 > 25)
soft1L3JCheckPTJet _ = Left "soft1L3JCheckPTJet: other than 3 jet analysis"
 

-- | > 180, > 25, > 25, > 25, > 25
soft1L5JCheckPTJet :: MultiJet -> Either String ()
soft1L5JCheckPTJet (MultiJet5 (j1,j2,j3,j4,j5) js) = 
    guardE "soft1L5JCheckPTJet: jet pT condition not satisfied" (pt j1 > 180 && pt j2 > 25 && pt j3 > 25 && pt j4 > 25 && pt j5 > 25)
soft1L5JCheckPTJet _ = Left "soft1L5JCheckPTJet: other than 5 jet analysis" 



-----------------------------
-- Soft2Muon channel check -- 
-----------------------------


-- | > 70, > 25
soft2MuonCheckPTJet :: MultiJet -> Either String ()
soft2MuonCheckPTJet (MultiJet2 (j1,j2) js) = 
  guardE "soft2MuonCheckPTJet: jet pT condition not satisfied" (pt j1 > 70 && pt j2 > 25)

----------------------------
----------------------------
---- hard channel check ----
----------------------------
----------------------------

-- | == 1
hardCheckNLep :: PhyEventNoTauNoBJet -> Either String Lepton12Obj
hardCheckNLep e = case view leptons e of 
                    x:[] -> Right x
                    _ -> Left "hardCheckNLep: nlep /= 1" 


hardCheckPTJet :: IsInclusive -> MultiJet -> Either String ()
hardCheckPTJet typ (MultiJet3 (j1,j2,j3) js) = do 
  let cond1 = pt j1 > 80 && pt j2 > 80 && pt j3 > 30 
      cond2 = case typ of
                Inclusive -> True 
                Binned -> case js of 
                            [] -> True
                            j:js' -> pt j < 40
  guardE ("hardCheckPTJet: 3jet condition for " ++ show typ ++ " not satisfied ") (cond1 && cond2)
hardCheckPTJet typ (MultiJet5 (j1,j2,j3,j4,j5) js) = do 
  let cond1 = pt j1 > 80 && pt j2 > 50 && pt j3 > 40 && pt j4 > 40 && pt j5 > 40 
      cond2 = case typ of
                Inclusive -> True 
                Binned -> case js of 
                            [] -> True
                            j:js' -> pt j < 40
  guardE ("hardCheckPTJet: 5jet condition for " ++ show typ ++ " not satisfied ") (cond1 && cond2)
hardCheckPTJet typ (MultiJet6 (j1,j2,j3,j4,j5,j6) js) = do 
  let cond1 = pt j1 > 80 && pt j2 > 50 && pt j3 > 40 && pt j4 > 40 && pt j5 > 40 && pt j6 > 40
  guardE ("hardCheckPTJet: 5jet condition for " ++ show typ ++ " not satisfied ") cond1
hardCheckPTJet _ _ = Left "hardCheckPTJet: other than 3,5,6 jet analysis"





-- | preselection object for soft lepton analysis
preselectionSoft :: PhyEventNoTau -> PhyEventNoTau
preselectionSoft ev = 
  let es = filter (\e -> pt e > 7  && abs (eta e) < 2.47) . view electrons $ ev  
      ms = filter (\m -> pt m > 6  && abs (eta m) < 2.4)  . view muons     $ ev
      js = filter (\j -> pt j > 20 && abs (eta j) < 2.8)  . view jets      $ ev
      bs = filter (\b -> pt b > 20 && abs (eta b) < 2.8)  . view bjets     $ ev   
  in (set electrons es . set muons ms . set jets js . set bjets bs) ev




-- | preselection object for hard lepton analysis
preselectionHard :: PhyEventNoTau -> PhyEventNoTauNoBJet
preselectionHard ev =
  let alljs = (map (\case JO_Jet j ->j ; JO_BJet b -> bJet2Jet b) . view jetBJets) ev
      es = filter (\e -> pt e > 10  && abs (eta e) < 2.47) . view electrons $ ev  
      ms = filter (\m -> pt m > 10  && abs (eta m) < 2.4)  . view muons     $ ev
      js = filter (\j -> pt j > 20 && abs (eta j) < 2.8) alljs
  in ( set electrons es 
     . set muons ms  
     . set jets js 
     . set eventId (view eventId ev)
     . set photons (view photons ev)
     . set missingET (view missingET ev) ) def


-- | lepton isolation according to ATLAS 
isolateLepton :: (LensElectrons event, LensMuons event, LensJetBJets event) => event -> event
isolateLepton ev = ( set electrons (sortBy (flip ptcompare) es') 
                   . set muons (sortBy (flip ptcompare) ms') 
                   . set jetBJets js' ) ev 
  where es = (map LO_Elec . view electrons) ev 
        ms = (map LO_Muon . view muons) ev 
        js = view jetBJets ev 
        ls = es ++ ms
        lalst = zip [1..] ls 
        jalst = zip [1..] js 
        --
        f = (,,)<$>fst<*>snd<*>flip classifyJetsInLepCone jalst . snd 
        clst = map (mkElimCmd . f) lalst  
        -- 
        g NoElim (accl,accj) = (accl,accj)
        g (ElimJet jis) (accl,accj) = (accl, jis++accj)
        g (ElimLep li) (accl,accj) = ((li:accl), accj)
        --
        (elst_l,elst_j) = (((,)<$>nub.fst<*>nub.snd) . foldr g ([],[])) clst 
        ls' = (map snd . filter (not . flip elem elst_l . fst)) lalst
        js' = (map snd . filter (not . flip elem elst_j . fst)) jalst
        -- 
        h (LO_Elec x) (acce,accm) = (x:acce,accm)
        h (LO_Muon x) (acce,accm) = (acce,x:accm)
        (es',ms') = foldr h ([],[]) ls'
        

data ConePos = InInner | BetweenInnerNOuter | OutOuter
             deriving (Show, Eq, Ord)

data ElimCmd = ElimLep Int | ElimJet [Int] | NoElim

conePos :: (MomObj a, MomObj b) => a -> b -> ConePos
conePos x y | deltaRdist x y < 0.2 = InInner
            | deltaRdist x y >=0.2 && deltaRdist x y < 0.4 = BetweenInnerNOuter 
            | otherwise = OutOuter

classifyJetsInLepCone :: Lepton12Obj -> [(Int,JetBJetObj)] -> ([Int],[Int],[Int])
classifyJetsInLepCone l jlst = foldr g ([],[],[]) jplst    
  where f = (,) <$> fst <*> conePos l . snd  
        -- jplst :: [(Int,ConePos)]
        jplst = map f jlst
        g (i,InInner) (acci,accio,acco) = (i:acci,accio,acco)
        g (i,BetweenInnerNOuter) (acci,accio,acco) = (acci,i:accio,acco)
        g (i,OutOuter) (acci,accio,acco) = (acci,accio,i:acco)

mkElimCmd :: (Int,Lepton12Obj,([Int],[Int],[Int])) -> ElimCmd 
mkElimCmd (i,l,(jeti,jetio,_)) = case l of
                                   LO_Elec _ -> if (not.null) jetio then ElimLep i else ElimJet jeti
                                   LO_Muon _ -> if null jeti && null jetio then NoElim else ElimLep i


------------------------
-- histogram analysis --                                    
------------------------

classifyAndGetValue :: (PhyEventNoTau->Double,PhyEventNoTauNoBJet->Double) 
                       -> PhyEventClassified 
                       -> Either String (Either String Double, Either String Double) -- ^ (soft, hard)
classifyAndGetValue (f1,f2) ev = (mergeTau
                                     >=> (\case TauMerged e -> Right (worker e) 
                                                _ -> Left "classifyAndGetMET: not TauMerged"))
                                   (Unpruned ev) 
  where worker e = (workerSoft e, workerHard e)
        workerSoft e = do let e' = (isolateLepton . preselectionSoft) e 
                              (ls,m) = countLeptonNumber e' 
                          case ls of 
                            MultiLep1 l -> do 
                              let (nj,_) = countJetNumber e'
                              guardE "workerSoft: nj < 3" (nj >= 3)
                              -- getNJets 3 e' >>= soft1L3JCheckPTJet 
                              (return . f1) e' 
                            _ -> Left "workerSoft : not single lep" 

        workerHard e = do let e' = (isolateLepton . preselectionHard) e 
                          l <- hardCheckNLep e' 
                          guardE "workerHard:pT_l <= 25" (pt l > 25)
                          -- getNJets 3 e' >>= hardCheckPTJet Inclusive
                          (return . f2) e'






      
------------------- 
-- main analysis --
-------------------

-- | 
atlas_SUSY_1to2L2to6JMET_8TeV :: WebDAVConfig 
                              -> WebDAVRemoteDir 
                              -> String 
                              -> IO (Maybe ()) 
atlas_SUSY_1to2L2to6JMET_8TeV wdavcfg wdavrdir bname = do 
    print bname 
    let fp = bname ++ "_pgs_events.lhco.gz"
    boolToMaybeM (doesFileExistInDAV wdavcfg wdavrdir fp) $ do 
      downloadFile False wdavcfg wdavrdir fp 
      bstr <- LB.readFile fp 
      let unzipped =decompress bstr 
          evts = parsestr unzipped 
          passed = map classify evts  
          -- asclst jes = mkHistogram (passed jes)
          -- testlst = [ (jes, asclst jes) | a <- as, b <- bs, let jes = JESParam a b ]
          hist =  mkHistogram. concat . filter (not.null) . map getETypes $ passed 

      let jsonfn = bname ++ "_ATLAS_1to2L2to6JMET_8TeV.json"
      let bstr = encodePretty hist 
      LB.writeFile jsonfn bstr 
      uploadFile wdavcfg wdavrdir jsonfn 
      removeFile jsonfn
      removeFile fp 
      return ()

-- | for making histogram 
atlas_hist :: (PhyEventClassified -> Either String (Either String a, Either String b))
           -> WebDAVConfig 
           -> WebDAVRemoteDir 
           -> String 
           -> EitherT String IO ([a],[b])
atlas_hist anal wdavcfg wdavrdir bname = do 
    let fp = bname ++ "_pgs_events.lhco.gz"
    guardEitherM (fp ++ " does not exist!") (doesFileExistInDAV wdavcfg wdavrdir fp)
    liftIO $ downloadFile False wdavcfg wdavrdir fp 
    bstr <- liftIO $ LB.readFile fp 
    let unzipped =decompress bstr 
        evts = parsestr unzipped 
        analyzed = (rights . map anal) evts  
        as = (rights . map fst) analyzed
        bs = (rights . map snd) analyzed
    return (as,bs)

atlas_getMET = atlas_hist (classifyAndGetValue (missingETpT,missingETpT))

atlas_get1stJetPT = atlas_hist (classifyAndGetValue (fstjetpt . view jets , fstjetpt . view jets ) )
  where fstjetpt [] = -100
        fstjetpt (x:xs) = pt x