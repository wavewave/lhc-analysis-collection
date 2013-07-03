{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}


import Control.Applicative ((<$>),(<*>),pure)
import           Control.Lens 
import Control.Monad 
import           Control.Monad.Trans 
import           Control.Monad.Trans.Either 
import           Control.Monad.Trans.Maybe 
import Data.Attoparsec.Char8 hiding (take)
-- import Data.Attoparsec.Lazy
import           Data.Aeson 
import           Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.Aeson.Generic as G
import qualified Data.ByteString.Char8 as B 
import qualified Data.ByteString.Lazy.Char8 as LB
import           Data.Data
import Data.Function (on)
import Data.List (lookup, sortBy) 
import Data.Maybe 
import Data.String
import Data.Typeable
import System.Environment (getArgs)
-- 
import HEP.Automation.EventGeneration.Config
import HEP.Storage.WebDAV.CURL
import HEP.Storage.WebDAV.Type 
-- import HEP.Storage.WebDAV.Util
import HEP.Util.Functions
import HEP.Util.Either 
-- 
import HEP.Physics.Analysis.ATLAS.Common
import HEP.Physics.Analysis.ATLAS.SUSY.SUSY_0L2to6JMET_8TeV
import HEP.Physics.Analysis.Common.XSecNTotNum
import HEP.Util.Work 



chisquareTTBar  :: TotalSR Double -> Double 
chisquareTTBar TotalSR {..} = ((numAL - 870)^2) / (180^2)  
                            + ((numAM - 7.8)^2) / (2.8^2) 
                            + ((numBM - 2.2)^2) / (2.0^2) 
                            + ((numBT - 0.6)^2) / (0.7^2) 
                            + ((numCM - 50)^2) / (11^2) 
                            + ((numCT - 0.9)^2) / (0.9^2) 
                            + ((numDT - 5.8)^2) / (2.1^2) 
                            + ((numEL - 76)^2) / (19^2) 
                            + ((numEM - 20)^2) / (6^2) 
                            + ((numET - 1.7)^2) / (1.4^2)  




-- (\wdavcfg wdavrdir nm -> getXSecNCount wdavcfg wdavrdir nm >>= getJSONFileAndUpload wdavcfg wdavrdir nm)
-- atlas_8TeV_0L2to6J_bkgtest

            -- testprint 
            -- 

jestest = ([0,1..20], [0,1..10])


main = do 
  args <- getArgs 
  let n1 :: Int = read (args !! 0) 
      n2 :: Int = read (args !! 1) 
      nlst = (drop (n1-1) . take n2) [1..] 
      rdir = "montecarlo/admproject/sm8/tt012" 
      basename = "SM_tt012j_LHC8ATLAS_MLM_DefCut_AntiKT0.4_NoTau_Set"
      fn = basename ++ show n1 ++ "to" ++ show n2 ++ "_ATLAS8TeV0L2to6JBkgTest.json" 
   
  bstr <- LB.readFile fn 
  let Just (xsecn,lst)  = G.decode bstr :: Maybe (CrossSectionAndCount,[(JESParam, TotalSR Double)])
      totevts = numberOfEvent xsecn 
      xsec = crossSectionInPb xsecn 
      weight = 238.0 * 20300 / fromIntegral totevts

      lst' = flip map lst $ \(x,y)-> let z' = multiplyScalar weight y
                                         y' = chisquareTTBar z'
                                     in (x,y'/10.0,z')

  print xsecn
  mapM_ print (sortBy (compare `on` snd3) lst')
  


  
  {-
  r1 <- work (\wdavcfg wdavrdir nm -> getXSecNCount XSecPYTHIA wdavcfg wdavrdir nm >>= getJSONFileAndUpload wdavcfg wdavrdir nm)
         "config1.txt" 
         rdir 
         basename 
         nlst 
  return ()
  -}
  {- 
  r1 <- work (atlas_8TeV_0L2to6J_bkgtest jestest)
         "config1.txt" 
         rdir 
         basename 
         nlst 
  -- print r1
  return () 
  -}
  {-
  r <- work fetchXSecNHist 
         "config1.txt" 
         rdir 
         basename 
         nlst 
  case r of 
    Left err -> putStrLn err 
    Right vs -> do --  return ()

      let vs' = catMaybes vs 
      let totevts = (sum . map (numberOfEvent.fst)) vs'
          mul = (*) <$> crossSectionInPb <*> fromIntegral . numberOfEvent
          totcross = (/ (fromIntegral totevts)) . sum . map (mul . fst) $ vs' 
          -- -- if I use only LO (normally not do that)
          -- weight = {- 238.0 -} totcross * 20300 / fromIntegral totevts 
          -- -- for 8 TeV ttbar (cross section from HATHOR ) 
          -- weight = 238.0 * 20300 / fromIntegral totevts 
          test a b = let hists = mapMaybe (lookup (JESParam a b)) . map snd $ vs'
                         sumup k = (sum . mapMaybe (lookup k)) hists
                         totsr :: TotalSR Int 
                         totsr = TotalSR { numAL = sumup AL
                                         , numAM = sumup AM
                                         , numBM = sumup BM 
                                         , numBT = sumup BT 
                                         , numCM = sumup CM 
                                         , numCT = sumup CT 
                                         , numDT = sumup DT 
                                         , numEL = sumup EL
                                         , numEM = sumup EM
                                         , numET = sumup ET } 
                     in (JESParam a b, totsr) 
      let xsecn = CrossSectionAndCount totcross totevts
          combined = (xsecn,[ test a b | a <- [0,1..20], b <- [0,1..10] ] )
          fn = basename ++ show n1 ++ "to" ++ show n2 ++ "_ATLAS8TeV0L2to6JBkgTest.json" 
          bstr = encodePretty combined 
      LB.writeFile fn bstr 
  -}      




fetchXSecNHist :: WebDAVConfig -> WebDAVRemoteDir -> String -> IO (Maybe (CrossSectionAndCount,[(JESParam,HistEType)]))
fetchXSecNHist wdavcfg wdavrdir bname = do 
  let fp1 = bname ++ "_ATLAS8TeV0L2to6JBkgTest.json"
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
    return (xsec,result)
