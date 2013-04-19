module Main where 

import Codec.Compression.GZip (decompress)
import Control.Monad (forM_)
import qualified Data.ByteString.Lazy.Char8 as LB
import Data.Maybe 
import HROOT.Core
import HROOT.Hist
import HROOT.Graf
import HROOT.IO
-- 
import HEP.Storage.WebDAV.CURL
import HEP.Util.Either 
import HEP.Parser.LHCOAnalysis.Parse (parsestr)
--
import HEP.Physics.Analysis.ATLAS.Common
import HEP.Physics.Analysis.ATLAS.Exotic.Leptoquark
import HEP.Util.Work

{-
datalst_squark = [ "200.0", "300.0", "400.0", "500.0", "600.0"
                 , "700.0", "800.0", "900.0", "1000.0", "1100.0", "1200.0" ] 
datalst = map (\x->("100.0",x)) datalst_squark
          ++ map (\x->("200.0",x)) (drop 1 datalst_squark)
          ++ map (\x->("300.0",x)) (drop 2 datalst_squark)
          ++ map (\x->("400.0",x)) (drop 3 datalst_squark)
          ++ map (\x->("500.0",x)) (drop 4 datalst_squark)
          ++ map (\x->("600.0",x)) (drop 5 datalst_squark)
          ++ map (\x->("700.0",x)) (drop 6 datalst_squark)
          ++ map (\x->("800.0",x)) (drop 7 datalst_squark)
          ++ map (\x->("900.0",x)) (drop 8 datalst_squark)
          ++ map (\x->("1000.0",x)) (drop 9 datalst_squark)
          ++ map (\x->("1100.0",x)) (drop 10 datalst_squark)
-}

testdatalst = [ ("500.0", "1000.0") ] 

main = do 
  forM_ testdatalst $ \(x,y) -> do 
    let nlst = [1] 
        rdir = "montecarlo/admproject/SimplifiedSUSY/scan2w2j2x"
        basename = "SimplifiedSUSYMN"++x++ "MG50000.0MSQ" ++ y ++ "_2sq_2w2j2x_LHC7ATLAS_NoMatch_NoCut_AntiKT0.4_NoTau_Set" 
    r <- work testwork "config1.txt" rdir basename nlst 
    putStrLn $ x ++ ", " ++ y ++ ", " ++ (show r) 


testwork wdavcfg wdavrdir bname = do
    let fp = bname ++ "_pgs_events.lhco.gz"
    boolToMaybeM (doesFileExistInDAV wdavcfg wdavrdir fp) $ do 
      downloadFile False wdavcfg wdavrdir fp 
      bstr <- LB.readFile fp 
      let unzipped =decompress bstr 
          evts = parsestr unzipped 
          jes = JESParam 5 2 
          passed = (catMaybes . map (classify jes)) evts  

      tfile <- newTFile "test.root" "NEW" "" 1   
      h1 <- newTH1F "haskell" "test" 20 1 500
      mapM_ (fill1 h1) passed 
      write h1 "" 0 0 
      close tfile ""

      -- return (passed)

    