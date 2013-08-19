{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}

import Control.Applicative
import Control.Lens
import Control.Monad 
import           Control.Monad.Trans (liftIO)
import           Control.Monad.Trans.Either (EitherT(..))
import           Control.Monad.Trans.Maybe 
-- import Data.Attoparsec.Lazy
import qualified Data.Aeson.Generic as G
import qualified Data.ByteString.Lazy.Char8 as LB
import           Data.Foldable (foldrM)
import           Data.Maybe 
import System.Environment 
import System.IO
-- 

import HEP.Storage.WebDAV.CURL
import HEP.Storage.WebDAV.Type
-- import HEP.Storage.WebDAV.Util
import HEP.Util.Either 
-- 
import HEP.Physics.Analysis.ATLAS.Common
import HEP.Physics.Analysis.ATLAS.SUSY.SUSY_1to2L2to6JMET_8TeV
import HEP.Physics.Analysis.Common.XSecNTotNum
import HEP.Util.Work 
--
import Util
import Debug.Trace

import HROOT


data AnalysisType = MET deriving (Show)

-- | 
getAnalysis :: AnalysisType -> WebDAVConfig -> WebDAVRemoteDir -> String 
            -> EitherT String IO ([Double], [Double])
getAnalysis MET = atlas_getMET
{- 
getAnalysis MEFF = atlas_getMeff
getAnalysis RatioMET_MEFF = atlas_getRatioMET_Meff
getAnalysis FirstLepPT = atlas_get1stLepPT 
getAnalysis FirstJetPT = atlas_get1stJetPT
-}

-- |
getAnalysisMaxx :: AnalysisType -> Double 
getAnalysisMaxx MET = 1500
{- getAnalysisMaxx MEFF = 3000
getAnalysisMaxx RatioMET_MEFF = 0.4
getAnalysisMaxx FirstLepPT = 1500
getAnalysisMaxx FirstJetPT = 1000
-}

-- | 
luminosity :: Double 
luminosity = 20300 

createRdirBName_xqld procname (mg,mq,mn) = 
  let rdir = "montecarlo/admproject/XQLDdegen/8TeV/scan_" ++ procname 
      basename = "ADMXQLD111degenMG"++ show mg++ "MQ" ++ show mq ++ "ML50000.0MN" ++ show mn ++ "_" ++ procname ++ "_LHC8ATLAS_NoMatch_NoCut_AntiKT0.4_NoTau_Set"
  in (rdir,basename)  

createRdirBName_xqldnoneut procname (mg,mq,mn) = 
  let rdir = "montecarlo/admproject/XQLDdegen/8TeV/scan_" ++ procname 
      basename = "ADMXQLD111degenMG"++ show mg++ "MQ" ++ show mq ++ "ML50000.0MN50000.0_" ++ procname ++ "_LHC8ATLAS_NoMatch_NoCut_AntiKT0.4_NoTau_Set"
  in (rdir,basename)  

{-
createRdirBName_simplifiedsusy procname (mg,mq,mn) = 
  let rdir = "montecarlo/admproject/SimplifiedSUSY/8TeV/scan_" ++ procname 
      basename = "SimplifiedSUSYMN"++ show mn++ "MG" ++ show mg ++ "MSQ" ++ show mq ++ "_" ++ procname ++ "_LHC8ATLAS_NoMatch_NoCut_AntiKT0.4_NoTau_Set"
  in (rdir,basename)  
-}

dirset_xqld = [ "2sg_2l8j2x"
              , "sqsg_2l7j2x"
              , "2sq_2l6j2x" ]

dirset_xqldnoneut = [ "2sg_2l4j2x"
                    , "2sq_oo_2l2j2x"
                    , "2sq_no_2l2j2x"
                    , "2sq_nn_2l2j2x"
                    , "sqsg_o_2l3j2x"
                    , "sqsg_n_2l3j2x"
                    ]


{-
dirset_simplifiedsusy = [ "2sg_4j2n" 
                        , "2sq_2j2n"
                        , "sqsg_3j2n" ] 

-}


getResult f (rdir,basename) = do 
  let nlst = [1]
  fileWork f "config1.txt" rdir basename nlst 

main = do 
  let set = [ (1500.0,1000.0, mn) | mn <- [100.0,300.0,500.0] ]
      set' = [ (1500.0,1000.0,100.0) ] 
  mapM_ (mainAnalysis MET createRdirBName_xqldnoneut dirset_xqldnoneut) set' 

  -- mainAnalysis FirstLepPT createRdirBName_xqldnoneut dirset_xqldnoneut (mg,mq,mn)

  -- mapM_ (mainAnalysis MET createRdirBName_simplifiedsusy dirset_simplifiedsusy) set -- (mg,mq,mn)
  -- mapM_ (mainAnalysis MET createRdirBName_xuddnoneut dirset_xuddnoneut) set' 
  -- mainAnalysis MET createRdirBName_xqld dirset_xqld (mg,mq,mn) 

  -- mainAnalysis FirstLepPT createRdirBName_xqld dirset_xqld (mg,mq,mn)
  -- mainAnalysis FirstJetPT createRdirBName_xudd dirset_xudd (mg,mq,mn)
  -- mainAnalysis FirstJetPT createRdirBName_simplifiedsusy dirset_simplifiedsusy (mg,mq,mn)

  -- mainAnalysisNJet createRdirBName_xudd dirset_xudd (mg,mq,mn)
  -- mapM_ (mainAnalysisNJet createRdirBName_simplifiedsusy dirset_simplifiedsusy) set
  -- mapM_ (mainAnalysisNJet createRdirBName_xuddnoneut dirset_xuddnoneut) set' 

-- | 
mainAnalysis :: AnalysisType
             -> (String -> (Double,Double,Double) -> (String,String)) 
             -> [String] 
             -> (Double,Double,Double) 
             -> IO ()
mainAnalysis analtype rdirbnamefunc dirset (mg,mq,mn) = do 
  let minx = 0; maxx = getAnalysisMaxx analtype; nchan = 50
      (_,bname') = rdirbnamefunc "total" (mg,mq,mn)
  tfile <- newTFile (bname' ++ "_1to2Lto6JMET_Hist" ++ show analtype ++ ".root") "NEW" "" 1   
  hsoft <- newTH1F "soft" "soft" nchan minx maxx
  hhard <- newTH1F "hard" "hard" nchan minx maxx
  r <- runEitherT $ mapM_ (\x -> countEvent analtype rdirbnamefunc (mg,mq,mn) x (hsoft,hhard)) dirset 
  mapM_ (\x->write x "" 0 0) [hsoft,hhard]
  close tfile ""
  case r of 
    Left err -> putStrLn err
    Right _ -> return ()

{-
mainAnalysisNJet :: (String -> (Double,Double,Double) -> (String,String)) 
                 -> [String] 
                 -> (Double,Double,Double) 
                 -> IO ()
mainAnalysisNJet rdirbnamefunc dirset (mg,mq,mn) = do 
  let minx = -0.5; maxx = 9.5; nchan = 10
      (_,bname') = rdirbnamefunc "total" (mg,mq,mn)
  tfile <- newTFile (bname' ++ "_NJet.root") "NEW" "" 1   
  hist <- newTH1F "NJet" "NJet" nchan minx maxx
  r <- runEitherT $ mapM_ (\x -> countNJetEvent rdirbnamefunc (mg,mq,mn) x hist) dirset 
  write hist "" 0 0
  close tfile ""
  case r of 
    Left err -> putStrLn err
    Right _ -> return ()
-}

getXsec :: WebDAVConfig -> WebDAVRemoteDir -> String -> EitherT String IO CrossSectionAndCount 
getXsec wdavcfg wdavrdir bname = do 
  let fp2 = bname ++ "_total_count.json" 
  guardEitherM (show wdavrdir ++ "/" ++ fp2 ++ " not exist!") (doesFileExistInDAV wdavcfg wdavrdir fp2) 
  (_,mr2) <- liftIO (downloadFile True wdavcfg wdavrdir fp2)
  r2 <- (liftM LB.pack . EitherT . return . maybeToEither (fp2 ++ " is not downloaded ")) mr2 
  (xsec :: CrossSectionAndCount) <- (EitherT . return . maybeToEither (fp2 ++ " JSON cannot be decoded") . G.decode)  r2  
  return xsec 

countEvent :: AnalysisType 
           -> (String -> (Double,Double,Double) -> (String,String)) 
           -> (Double,Double,Double) 
           -> String 
           -> (TH1F, TH1F) 
           -> EitherT String IO ()
countEvent analtype rdirbnamefunc (mg,mq,mn) str (hsoft,hhard) = do 
  let (rdir,bname) = rdirbnamefunc str (mg,mq,mn)
  xsec <- singleWork getXsec "config1.txt" rdir bname 1 
  let weight = crossSectionInPb xsec * luminosity / fromIntegral (numberOfEvent xsec)
  (rs,rh) <- getCount analtype (rdir,bname) 
  liftIO (mapM_ (\x -> fill1w hsoft x weight) rs)
  liftIO (mapM_ (\x -> fill1w hhard x weight) rh)


{-
countNJetEvent :: (String -> (Double,Double,Double) -> (String,String)) 
               -> (Double,Double,Double) 
               -> String 
               -> TH1F
               -> EitherT String IO ()
countNJetEvent rdirbnamefunc (mg,mq,mn) str hist = do 
  let (rdir,bname) = rdirbnamefunc str (mg,mq,mn)
  xsec <- singleWork getXsec "config1.txt" rdir bname 1 
  let weight = crossSectionInPb xsec * luminosity / fromIntegral (numberOfEvent xsec)
  r <- getNJetCount (rdir,bname) 
  -- liftIO $ print r
  liftIO ( mapM_ (\x->fill1w hist (fromIntegral x) weight) r )
  -- liftIO $ (mapM_ print r )
-}
      
getCount :: AnalysisType -> (String,String) -> EitherT String IO ([Double],[Double])
getCount analtype (rdir,basename) = do 
  let nlst = [1]
  rs <- fileWork (getAnalysis analtype) "config1.txt" rdir basename nlst
  --  liftIO $ print (length (head rs))
  (return . ((,) <$> concat . map (view _1) <*> concat . map (view _2))) rs 
       
{-
getNJetCount :: (String,String) -> EitherT String IO [Int]
getNJetCount (rdir,basename) = do 
  let nlst = [1]
  rs <- fileWork atlas_getNJet "config1.txt" rdir basename nlst
  liftIO $ print (length (head rs))
  return (concat rs)
-}