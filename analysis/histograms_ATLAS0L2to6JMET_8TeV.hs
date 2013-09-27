{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}

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
import HEP.Physics.Analysis.ATLAS.SUSY.SUSY_0L2to6JMET_8TeV
import HEP.Physics.Analysis.Common.XSecNTotNum
import HEP.Util.Work 
--
import Util
import Debug.Trace

import HROOT


data AnalysisType = MET | MEFF | RatioMET_MEFF | FirstLepPT | FirstJetPT deriving (Show)

-- | 
getAnalysis :: AnalysisType -> WebDAVConfig -> WebDAVRemoteDir -> String 
            -> EitherT String IO [(Double, (Bool,Bool,Bool,Bool,Bool))]
getAnalysis MET = atlas_getMissingET
getAnalysis MEFF = atlas_getMeff
getAnalysis RatioMET_MEFF = atlas_getRatioMET_Meff
getAnalysis FirstLepPT = atlas_get1stLepPT 
getAnalysis FirstJetPT = atlas_get1stJetPT

-- |
getAnalysisMinMaxX :: AnalysisType -> (Double,Double) 
getAnalysisMinMaxX MET = (0,1000)
getAnalysisMinMaxX MEFF = (0,3000)
getAnalysisMinMaxX RatioMET_MEFF = (0,0.7)
getAnalysisMinMaxX FirstLepPT = (-10,500)
getAnalysisMinMaxX FirstJetPT = (0,1000)


-- | 
luminosity :: Double 
luminosity = 20300 

-- data DataSet a where  
--  Triple (Double,Double,Double) | Doublet (Double,Double) 

createRdirBName_xqld procname (mg,mq,mn) = 
  let rdir = "montecarlo/admproject/XQLDdegen/8TeV/neutLOSP/scan_" ++ procname 
      basename = "ADMXQLD111degenMG"++ show mg++ "MQ" ++ show mq ++ "ML50000.0MN" ++ show mn ++ "_" ++ procname ++ "_LHC8ATLAS_NoMatch_NoCut_AntiKT0.4_NoTau_Set"
  in (rdir,basename)  

createRdirBName_xqldnoneut procname (mg,mq,mn) = 
  let rdir = "montecarlo/admproject/XQLDdegen/8TeV/scan_" ++ procname 
      basename = "ADMXQLD111degenMG"++ show mg++ "MQ" ++ show mq ++ "ML50000.0MN50000.0_" ++ procname ++ "_LHC8ATLAS_NoMatch_NoCut_AntiKT0.4_NoTau_Set"
  in (rdir,basename)  



createRdirBName_xudd procname (mg,mq,mn) = 
  let rdir = "montecarlo/admproject/XUDDdegen/8TeV/neutLOSP/scan_" ++ procname 
      basename = "ADMXUDD112degenMG"++ show mg++ "MQ" ++ show mq ++ "ML50000.0MN" ++ show mn ++ "_" ++ procname ++ "_LHC8ATLAS_NoMatch_NoCut_AntiKT0.4_NoTau_Set"
  in (rdir,basename)  

createRdirBName_xuddnoneut procname (mg,mq,mn) = 
  let rdir = "montecarlo/admproject/XUDDdegen/8TeV/scan_" ++ procname 
      basename = "ADMXUDD112degenMG"++ show mg++ "MQ" ++ show mq ++ "ML50000.0MN50000.0_" ++ procname ++ "_LHC8ATLAS_NoMatch_NoCut_AntiKT0.4_NoTau_Set"
  in (rdir,basename)  


createRdirBName_simplifiedsusy procname (mg,mq,mn) = 
  let rdir = "montecarlo/admproject/SimplifiedSUSY/8TeV/scan_" ++ procname 
      basename = "SimplifiedSUSYMN"++ show mn++ "MG" ++ show mg ++ "MSQ" ++ show mq ++ "_" ++ procname ++ "_LHC8ATLAS_NoMatch_NoCut_AntiKT0.4_NoTau_Set"
  in (rdir,basename)  

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


dirset_xudd = [ "2sg_10j2x"
              , "sqsg_9j2x" 
              , "2sq_8j2x"
              ]

dirset_xuddnoneut = [ "2sg_6j2x" 
                    , "2sq_nn_4j2x"
                    , "2sq_no_4j2x" 
                    , "2sq_oo_4j2x" 
                    , "sqsg_n_5j2x"
                    , "sqsg_o_5j2x" ] 


dirset_simplifiedsusy = [ "2sg_4j2n" 
                        , "2sq_2j2n"
                        , "sqsg_3j2n" ] 


getResult f (rdir,basename) = do 
  let nlst = [1]
  fileWork f "config1.txt" rdir basename nlst 

main = do 
  let -- set = [ (1000.0,1000.0, mn) | mn <- [100.0,300.0,500.0] ]
      -- set' = [ (1500.0,1000.0,100.0) ] 
      -- set'' = [ (1500.0,1000.0,100.0), (1500.0,1000.0,300.0), (1500.0,1000.0,500.0) ] 
      -- set_xudd_sq = [ (2500.0,1500.0,50000.0) ] 
      -- set_smpl_sq = [ (2500.0,1500.0, mn) | mn <- [100.0,300.0,500.0] ]  
      set_xudd_neut = [ (1000.0,1000.0,mn) | mn <- [100.0, 300.0, 500.0] ]
      set_smpl_neut = [ (1000.0,1000.0, mn) | mn <- [100.0,300.0,500.0] ]

  -- mapM_ (mainAnalysis FirstLepPT createRdirBName_xqld dirset_xqld) set -- set'
  -- mapM_ (mainAnalysis FirstLepPT createRdirBName_simplifiedsusy dirset_simplifiedsusy) set -- set''

  -- mapM_ (mainAnalysis MET createRdirBName_xuddnoneut dirset_xuddnoneut) set_xudd_sq
  -- mapM_ (mainAnalysis MET createRdirBName_simplifiedsusy dirset_simplifiedsusy) set_smpl_sq
  -- mapM_ (mainAnalysis MET createRdirBName_xudd dirset_xudd) set_xudd_neut
  -- mapM_ (mainAnalysis MET createRdirBName_simplifiedsusy dirset_simplifiedsusy) set_smpl_neut


  -- mapM_ (mainAnalysis MEFF createRdirBName_xuddnoneut dirset_xuddnoneut) set_xudd_sq
  -- mapM_ (mainAnalysis MEFF createRdirBName_simplifiedsusy dirset_simplifiedsusy) set_smpl_sq
  -- mapM_ (mainAnalysis MEFF createRdirBName_xudd dirset_xudd) set_xudd_neut
  -- mapM_ (mainAnalysis MEFF createRdirBName_simplifiedsusy dirset_simplifiedsusy) set_smpl_neut



  -- mapM_ (mainAnalysis RatioMET_MEFF createRdirBName_xuddnoneut dirset_xuddnoneut) set_xudd_sq
  -- mapM_ (mainAnalysis RatioMET_MEFF createRdirBName_simplifiedsusy dirset_simplifiedsusy) set_smpl_sq
  mapM_ (mainAnalysis RatioMET_MEFF createRdirBName_xudd dirset_xudd) set_xudd_neut
  mapM_ (mainAnalysis RatioMET_MEFF createRdirBName_simplifiedsusy dirset_simplifiedsusy) set_smpl_neut



  -- mainAnalysis MET createRdirBName_xqldnoneut dirset_xqldnoneut (mg,mq,mn)
  -- mapM_ (mainAnalysis MET createRdirBName_simplifiedsusy dirset_simplifiedsusy) set -- (mg,mq,mn)
  -- mapM_ (mainAnalysis MET createRdirBName_xuddnoneut dirset_xuddnoneut) set' 
  -- mapM_ (mainAnalysis MET createRdirBName_xqld dirset_xqld) set

  -- mainAnalysis FirstLepPT createRdirBName_xqld dirset_xqld (mg,mq,mn)
  -- mainAnalysis FirstJetPT createRdirBName_xudd dirset_xudd (mg,mq,mn)
  -- mainAnalysis FirstJetPT createRdirBName_simplifiedsusy dirset_simplifiedsusy (mg,mq,mn)

  -- mapM_ (mainAnalysisNJet createRdirBName_xuddnoneut dirset_xuddnoneut) set_xudd_sq 
  -- mapM_ (mainAnalysisNJet createRdirBName_simplifiedsusy dirset_simplifiedsusy) set_smpl_sq
  -- mapM_ (mainAnalysisNJet createRdirBName_xudd dirset_xudd) set_xudd_neut 
  -- mapM_ (mainAnalysisNJet createRdirBName_simplifiedsusy dirset_simplifiedsusy) set_smpl_neut


  -- mapM_ (mainAnalysisNJet createRdirBName_simplifiedsusy dirset_simplifiedsusy) set
  -- mapM_ (mainAnalysisNJet createRdirBName_xuddnoneut dirset_xuddnoneut) set' 

-- | 
mainAnalysis :: AnalysisType
             -> (String -> (Double,Double,Double) -> (String,String)) 
             -> [String] 
             -> (Double,Double,Double) 
             -> IO ()
mainAnalysis analtype rdirbnamefunc dirset (mg,mq,mn) = do 
  let (minx,maxx) = getAnalysisMinMaxX analtype; nchan = 51
 
      (_,bname') = rdirbnamefunc "total" (mg,mq,mn)

  tfile <- newTFile (bname' ++ "_" ++ show analtype ++ ".root") "NEW" "" 1   
  ha <- newTH1F "2jet" "2jet" nchan minx maxx
  hb <- newTH1F "3jet" "3jet" nchan minx maxx
  hc <- newTH1F "4jet" "4jet" nchan minx maxx
  hd <- newTH1F "5jet" "5jet" nchan minx maxx
  he <- newTH1F "6jet" "6jet" nchan minx maxx

  r <- runEitherT $ mapM_ (\x -> countEvent analtype rdirbnamefunc (mg,mq,mn) x (ha,hb,hc,hd,he)) dirset 

  mapM_ (\x->write x "" 0 0) [ha,hb,hc,hd,he]
  close tfile ""

  case r of 
    Left err -> putStrLn err
    Right _ -> return ()


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
           -> (TH1F, TH1F, TH1F, TH1F, TH1F) 
           -> EitherT String IO ()
countEvent analtype rdirbnamefunc (mg,mq,mn) str (ha,hb,hc,hd,he) = do 
  let (rdir,bname) = rdirbnamefunc str (mg,mq,mn)
  xsec <- singleWork getXsec "config1.txt" rdir bname 1 
  let weight = crossSectionInPb xsec * luminosity / fromIntegral (numberOfEvent xsec)
  -- 
  let fillfunc :: Double -> (Double,(Bool,Bool,Bool,Bool,Bool)) -> IO ()
      fillfunc w (x,sr) = do
        when (view _1 sr) (fill1w ha x w >> return ())
        when (view _2 sr) (fill1w hb x w >> return ())
        when (view _3 sr) (fill1w hc x w >> return ())
        when (view _4 sr) (fill1w hd x w >> return ())
        when (view _5 sr) (fill1w he x w >> return ())
       
  r <- getCount analtype (rdir,bname) 
  -- liftIO $ print r
  liftIO ( mapM_ (fillfunc weight) r )
  -- liftIO $ (mapM_ print r )

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

      
getCount :: AnalysisType -> (String,String) -> EitherT String IO [(Double,(Bool,Bool,Bool,Bool,Bool))]
getCount analtype (rdir,basename) = do 
  let nlst = [1]
  rs <- fileWork (getAnalysis analtype) "config1.txt" rdir basename nlst
  liftIO $ print (length (head rs))
  return (concat rs)
       

getNJetCount :: (String,String) -> EitherT String IO [Int]
getNJetCount (rdir,basename) = do 
  let nlst = [1]
  rs <- fileWork atlas_getNJet "config1.txt" rdir basename nlst
  liftIO $ print (length (head rs))
  return (concat rs)
       
