{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}

import Control.Monad 
import           Control.Monad.Trans.Maybe 
-- import Data.Attoparsec.Lazy
import qualified Data.Aeson.Generic as G
import qualified Data.ByteString.Lazy.Char8 as LB
import           Data.Foldable (foldrM)
import           Data.Maybe 
import System.IO
-- 
import HEP.Storage.WebDAV.CURL
-- import HEP.Storage.WebDAV.Util
import HEP.Util.Either 
-- 
import HEP.Physics.Analysis.ATLAS.Common
import HEP.Physics.Analysis.ATLAS.SUSY.SUSY_0L2to6J
import HEP.Physics.Analysis.Common.XSecNTotNum
import HEP.Util.Work 

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

main = do 
  {- 
  rs <- forM datalst (\s -> (doJob check_file_exist . createRdirBName "2sq_2l2j2x") s 
                                >>= return . maybe (show s) (const []) . head)
  mapM_ print $ filter (not.null) rs  -}
  -- forM_ datalst (getCount.createRdirBName "2sq_2l2j2x")
  h <- openFile "xqld_sqsg_data.dat" WriteMode 
   
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
                     r_ratio = getRFromSR totsr
                 -- print h_2sq
                 -- print h_sqsg
                 -- print h_2sg
                 return (x,y, show r_ratio))
  hClose h 


mkTotalSR hists = 
  let sumup k = (sum . mapMaybe (lookup k)) hists
      totsr = TotalSR { numCL = sumup CL 
                      , numEL = sumup EL 
                      , numAM = sumup AM
                      , numA'M = sumup A'M 
                      , numCM = sumup CM 
                      , numEM = sumup EM
                      , numAT = sumup AT 
                      , numBT = sumup BT 
                      , numCT = sumup CT 
                      , numDT = sumup DT 
                      , numET = sumup ET } 
  in totsr 


getRFromSR sr = 
    let r = TotalSR { numCL = g numCL 
                    , numEL = g numEL 
                    , numAM = g numAM 
                    , numA'M = g numA'M
                    , numCM = g numCM 
                    , numEM = g numEM
                    , numAT = g numAT
                    , numBT = g numBT 
                    , numCT = g numCT 
                    , numDT = g numDT 
                    , numET = g numET } 
    in maximumInSR r 
  where getratio f x y = f x / f y 
        g f = getratio f sr nbsmlimit_SR

maximumInSR TotalSR{..} = maximum [numCL,numEL,numAM,numA'M,numCM,numEM,numAT,numBT,numCT,numDT,numET] 

doJob wk (rdir,basename) = do
  let nlst = [1]
  Right r1 <- work wk "config1.txt" rdir basename nlst 
  return r1 


getLHCresult (rdir,basename) = do 
  let nlst = [1]
  Right r1 <- work -- fetchXSecNHist 
                       atlasresult_4_7fb
                       "config1.txt" 
                       rdir 
                       basename 
                       nlst 
  return r1 
  

        
createRdirBName procname (mg,mq) = 
  let rdir = "montecarlo/admproject/XQLD/scan_" ++ procname 
      basename = "ADMXQLD111MG"++mg++ "MQ" ++ mq ++ "ML50000.0MN50000.0_" ++ procname ++ "_LHC7ATLAS_NoMatch_NoCut_AntiKT0.4_NoTau_Set"
  in (rdir,basename)  

getCount (rdir,basename) = do 
  {- args <- getArgs 
  let n1 :: String = (args !! 0) 
      n2 :: String = (args !! 1) 
  -}
  let nlst = [1]

  r1 <- work (\wdavcfg wdavrdir nm -> getXSecNCount XSecLHE wdavcfg wdavrdir nm >>= getJSONFileAndUpload wdavcfg wdavrdir nm)
         "config1.txt" 
         rdir 
         basename 
         nlst 
  print r1

  r2 <- work 
         (atlas_7TeV_0L2to6J_bkgtest ([5],[2]))
         "config1.txt"
         rdir
         basename
         nlst
  print r2 

check_file_exist wdavcfg wdavrdir bname = do 
  let fp1 = bname ++ "_ATLAS7TeV0L2to6JBkgTest.json"
      fp2 = bname ++ "_total_count.json" 
  b <- doesFileExistInDAV wdavcfg wdavrdir fp1 
  if b then return (Just (Just ()))  else return Nothing 





-- atlasresult_4_7fb :: WebDAVConfig -> WebDAVRemoteDir -> String ->IO (Maybe ([ (EType,Double) ], Double))
--  -> IO (Maybe (CrossSectionAndCount,[(JESParam,HistEType)]))
atlasresult_4_7fb wdavcfg wdavrdir bname = do 
  let fp1 = bname ++ "_ATLAS7TeV0L2to6JBkgTest.json"
      fp2 = bname ++ "_total_count.json" 
  runMaybeT $ do  
    (_,mr1) <- MaybeT . boolToMaybeM (doesFileExistInDAV wdavcfg wdavrdir fp1) 
                      . downloadFile True wdavcfg wdavrdir $ fp1 
    r1 <- liftM LB.pack (MaybeT . return $ mr1) 
    (result :: [(JESParam, HistEType)]) <- MaybeT . return $ G.decode r1 
   
    (_,mr2) <- MaybeT . boolToMaybeM (doesFileExistInDAV wdavcfg wdavrdir fp2) 
                      . downloadFile True wdavcfg wdavrdir $ fp2
    r2 <- liftM LB.pack (MaybeT . return $ mr2) 
    (xsec :: CrossSectionAndCount) <- MaybeT . return $ G.decode  r2  

    let weight = crossSectionInPb xsec * 4700 / fromIntegral (numberOfEvent xsec)
        hist = map (\(x,y) -> (x,fromIntegral y * weight)) ((snd . head) result )
    let getratio (x,y) = do y' <- lookup x nbsmlimit 
                            return (y/ y') 
        maxf (x,y) acc = do r <- getratio (x,y)
                            return (max acc r)
    maxratio <- MaybeT . return $ foldrM maxf 0 hist 

    return (xsec,result,hist,maxratio) -- (xsec,result)


nbsmlimit = [ (CL, 51) 
            , (EL, 77) 
            , (AM, 24) 
            , (A'M, 28) 
            , (CM, 17)
            , (EM, 11) 
            , (AT, 3.1) 
            , (BT, 3.0)
            , (CT, 16)
            , (DT, 9.6) 
            , (ET, 12) ] 


nbsmlimit_SR = mkTotalSR [nbsmlimit] 
