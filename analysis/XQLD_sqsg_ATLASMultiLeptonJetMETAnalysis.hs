{-# LANGUAGE RecordWildCards, GADTs, ScopedTypeVariables #-}

import Control.Monad
import Control.Monad.Trans.Maybe
import qualified Data.Aeson.Generic as G
import qualified Data.ByteString.Lazy.Char8 as LB
import           Data.Foldable (foldrM)
import           Data.Maybe
import qualified Data.Text.Lazy as T
import qualified Data.Text.Format as TF
import qualified Data.Text.Lazy.Builder as TB
import           System.IO 
-- 
import HEP.Storage.WebDAV.CURL
-- import HEP.Storage.WebDAV.Util
import HEP.Util.Either 
-- 
import           HEP.Physics.Analysis.Common.XSecNTotNum
import           HEP.Physics.Analysis.ATLAS.Common
import           HEP.Physics.Analysis.ATLAS.SUSY.SUSY_MultiLepton
-- import           HEP.Physics.Analysis.ATLAS.SUSY_MultiLepton.PrettyPrint
import           HEP.Util.Work 
-- 

masslst = [ "100.0"
          , "200.0"
          , "300.0"
          , "400.0"
          , "500.0" 
          , "600.0"
          , "700.0"
          , "800.0"
          , "900.0"
          , "1000.0"
          , "1100.0"
          , "1200.0"
          , "1300.0"
          , "1400.0"
          , "1500.0"
          , "1600.0" 
          , "1700.0"
          , "1800.0"
          , "1900.0"
          , "2000.0" ]

datalst = [ (x,y) | x <- masslst, y <- masslst ] 

takeR [Just (_,_,_,r)] = r 

takeHist [Just (_,_,h,_)] = h

takeResult [Just (_,r,_,_)] = r


check_file_exist wdavcfg wdavrdir bname = do 
  let fp1 = bname ++ "_ATLAS7TeVMultiL2to4J.json"
      fp2 = bname ++ "_total_count.json" 
  b <- doesFileExistInDAV wdavcfg wdavrdir fp1 
  if b then return (Just (Just ()))  else return Nothing 


-- data TotalSRMLep = TotalSRMLep { 
--                               }
  {- 
  h <- openFile "xqldsquark_multilep.dat" WriteMode
  -- let h = stdout 
  forM_ datalst $ \(x,y) -> do  
    -- getCount x y 
    [(Just (_,_,_,r))] <- getLHCresult x y
    hPutStrLn h $ y ++ ", " ++ (T.unpack . TB.toLazyText . TF.fixed 2) r

  hClose h
  -}


main = do 
  {-
  rs <- forM datalst (\s -> (doJob check_file_exist . createRdirBName "2sq_2l2j2x") s -- "sqsg_2l3j2x") s -- "2sg_2l4j2x") s 
                                 >>= return . maybe (show s) (const []) . head)
  mapM_ print $ filter (not.null) rs  
  -}
  {-
  mapM_ (\proc->forM_ datalst (doJob (atlas_7TeV_MultiL2to4J (JESParam 5 2)) . createRdirBName proc))
        [ "sqsg_2l3j2x", "2sq_2l2j2x", "2sg_2l4j2x" ]
  -}
  
  h <- openFile "xqld_sqsg_multilep.dat" WriteMode 

  mapM_ (\(mg,msq,r)-> hPutStrLn h (mg ++ ", " ++ msq ++ ", " ++ r))
    =<< forM datalst
              (\(x,y) -> do 
                 t_2sq <- (getLHCresult.createRdirBName "2sq_2l2j2x") (x,y) 
                 t_sqsg <- (getLHCresult.createRdirBName "sqsg_2l3j2x") (x,y)
                 t_2sg <- (getLHCresult.createRdirBName "2sg_2l4j2x") (x,y)
                 let h_2sq = takeHist t_2sq 
                     h_sqsg = takeHist t_sqsg 
                     h_2sg = takeHist t_2sg
                     totsr =  mkTotalSR [h_2sq,h_sqsg,h_2sg] 
                     getratio (x,y) = let Just y' = lookup x nbsmlimit 
                                      in (y/ y') 
                     r_ratio = (maximum . map getratio) totsr 
                 return (x,y, show r_ratio))
  hClose h 
  

mkTotalSR hists = 
  let sumup k = (sum . mapMaybe (lookup k)) hists
      totsr = map (\k -> (k,sumup k)) [ SingleHardElec3J
                                      , SingleHardMuon3J
                                      , SingleHardElec4J
                                      , SingleHardMuon4J
                                      , SingleSoftElec  
                                      , SingleSoftMuon
                                      , MultiElecElec2J
                                      , MultiMuonMuon2J 
                                      , MultiElecMuon2J 
                                      , MultiElecElec4J 
                                      , MultiMuonMuon4J 
                                      , MultiElecMuon4J
                                      ]
  in totsr 



doJob wk (rdir,basename) = do
  let nlst = [1]
  Right r1 <- work wk "config1.txt" rdir basename nlst 
  return r1 

createRdirBName procname (mg,mq) = 
  let rdir = "montecarlo/admproject/XQLD/scan_" ++ procname 
      basename = "ADMXQLD111MG"++mg++ "MQ" ++ mq ++ "ML50000.0MN50000.0_" ++ procname ++ "_LHC7ATLAS_NoMatch_NoCut_AntiKT0.4_NoTau_Set"
  in (rdir,basename)  


getLHCresult = doJob atlasresult_4_7fb


atlasresult_4_7fb wdavcfg wdavrdir bname = do
  let fp1 = bname ++ "_ATLAS7TeVMultiL2to4J.json"
      fp2 = bname ++ "_total_count.json" 
  runMaybeT $ do  
    (_,mr1) <- MaybeT . boolToMaybeM (doesFileExistInDAV wdavcfg wdavrdir fp1) 
                      . downloadFile True wdavcfg wdavrdir $ fp1 
    r1 <- liftM LB.pack (MaybeT . return $ mr1) 
    (result :: [(JESParam,[(EventTypeCode,Int)])]) <- MaybeT . return $ G.decode r1 
   
    (_,mr2) <- MaybeT . boolToMaybeM (doesFileExistInDAV wdavcfg wdavrdir fp2) 
                      . downloadFile True wdavcfg wdavrdir $ fp2
    r2 <- liftM LB.pack (MaybeT . return $ mr2) 
    (xsec :: CrossSectionAndCount) <- MaybeT . return $ G.decode  r2  

    let weight = crossSectionInPb xsec * 4700 / fromIntegral (numberOfEvent xsec)
        hist = map (\(x,y) -> (x,fromIntegral y * weight)) ((snd.head) result)

    let getratio (x,y) = do y' <- lookup x nbsmlimit 
                            return (y/ y') 
        maxf (x,y) acc = do r <- getratio (x,y)
                            return (max acc r)
    maxratio <- MaybeT . return $ foldrM maxf 0 hist 

    return (xsec,result,hist,maxratio) 




{-
getCount n1 n2 = do 
  let nlst = [1]
      rdir = "montecarlo/admproject/XQLD/scan" 
      basename = "ADMXQLD111MG"++n1++ "MQ" ++ n2 ++ "ML50000.0MN50000.0_2sd_2l2j2x_LHC7ATLAS_NoMatch_NoCut_AntiKT0.4_NoTau_Set"
  r1 <- work (\wdavcfg wdavrdir nm -> getXSecNCount XSecLHE wdavcfg wdavrdir nm >>= getJSONFileAndUpload wdavcfg wdavrdir nm)
         "config1.txt" 
         rdir 
         basename 
         nlst 
  print r1 
  r2 <- work 
         (atlas_7TeV_MultiL2to4J (JESParam 5 2))
         "config1.txt"
         rdir
         basename
         nlst
  print r2 


-}


nbsmlimit = [ (SingleHardElec3J, 4.4) 
            , (SingleHardMuon3J, 3.6)
            , (SingleHardElec4J, 5.8)
            , (SingleHardMuon4J, 4.5)
            , (SingleSoftElec  , 8.6)
            , (SingleSoftMuon  , 9.0)
            , (MultiElecElec2J , 3.3)
            , (MultiMuonMuon2J , 3.6) 
            , (MultiElecMuon2J , 3.9)
            , (MultiElecElec4J , 7.2)
            , (MultiMuonMuon4J , 9.1)
            , (MultiElecMuon4J , 10.1)
            ]



