{-# LANGUAGE RecordWildCards, GADTs #-}

module HEP.Physics.Analysis.ATLAS.SUSY.Format where

import           Codec.Compression.GZip
import           Control.Monad
import qualified Data.ByteString.Lazy.Char8 as LB
import           Data.Maybe
-- import           System.Environment
import           System.FilePath
import           System.IO 
import           Text.Hastache
import           Text.Hastache.Context
-- 
import           HEP.Parser.LHCOAnalysis.Parse
-- 
import           HEP.Physics.Analysis.ATLAS.SUSY
import           HEP.Util.Format
-- 
import Debug.Trace


{-
filelist = 
  [ ( 1000, 400, 3.67e-3
    , "data20130222/ADMXQLD211MST50000.0MG1000.0MSQ400.0_gluinopair_stopdecayfull_LHC7ATLAS_NoMatch_NoCut_Cone0.4_Set1_pgs_events.lhco.gz" )
  , ( 1000, 500, 3.67e-3
    , "data20130222/ADMXQLD211MST50000.0MG1000.0MSQ500.0_gluinopair_stopdecayfull_LHC7ATLAS_NoMatch_NoCut_Cone0.4_Set1_pgs_events.lhco.gz" )
  , ( 1000, 600, 3.67e-3
    , "data20130222/ADMXQLD211MST50000.0MG1000.0MSQ600.0_gluinopair_stopdecayfull_LHC7ATLAS_NoMatch_NoCut_Cone0.4_Set1_pgs_events.lhco.gz" )
  , ( 1000, 700, 3.67e-3
    , "data20130222/ADMXQLD211MST50000.0MG1000.0MSQ700.0_gluinopair_stopdecayfull_LHC7ATLAS_NoMatch_NoCut_Cone0.4_Set1_pgs_events.lhco.gz" )
  , ( 1000, 800, 3.67e-3
    , "data20130222/ADMXQLD211MST50000.0MG1000.0MSQ800.0_gluinopair_stopdecayfull_LHC7ATLAS_NoMatch_NoCut_Cone0.4_Set1_pgs_events.lhco.gz") ] 
-}

header = 
 " \\begin{tabular}{cccccccc} \n \
 \ \\hline \n \
 \ \\hline \n \
 \ $(m_{\\tilde{g}},m_{\\tilde{c}})$ & ~~~~~~$\\sigma_{\\tilde{g}\\tilde{g}}$~~~~~ & total & \\multicolumn{3}{c}{single lepton} & \\multicolumn{2}{c} {multi lepton}  \\\\ \n \
 \   &  &  & ~~3 jets~~ & ~~4 jets~~ & ~~~soft~~~ & ~~2 jets~~ & ~~4 jets~~ \\\\ \n \
 \ \\hline \n \
 \ \\hline \n" 

footer = "\\end{tabular}\n"

template = 
  " ({{mgluino}},{{msquark}}) & {{xsec}} & {{totnum}} & {{singlelep3}} & {{singlelep4}} & {{singlelepsoft}} & {{multilep2}} & {{multilep4}} \\\\\n \
  \ @4.7 fb$^{-1}$ &  &  & {{normsinglelep3}} & {{normsinglelep4}} & {{normsinglelepsoft}} & {{normmultilep2}} & {{normmultilep4}} \\\\\n \
  \\\hline\n"

normalize :: Double -> Int -> Int -> Maybe Double
normalize xsec totnum x = 
  if x /= 0 then Just (xsec*4.7e3/(fromIntegral totnum)*(fromIntegral x)) else Nothing 


{-  
main = do 
  h <- openFile "output.dat" WriteMode 
  mapM_ (analysis h) filelist
  hClose h 
-}

analysis :: Handle -> (Double,Double,Double,FilePath) -> IO ()
analysis h (mgluino,msquark,xsec,fn) = do 
  putStrLn "atlas counting"
  -- args <- getArgs
  -- when (length args /= 1) $ error "./parsertest filename"
  -- let fn = args !! 0 
  --     basename = takeBaseName fn 
 
  bstr <- LB.readFile fn 
  let unzipped = decompress bstr 
      evts = parsestr unzipped
      signalevts = map (preselect HardLepton . taubjetMerge) evts 
      classified = mapMaybe classifyEvent signalevts 
      -- 
      totnum = length evts
      singlelep3 = (length . filter isSingleLep3) classified
      singlelep4 = (length . filter isSingleLep4) classified
      singlelepsoft = (length . filter isSingleLepSoft) classified
      multilep2 = (length . filter isMultiLep2) classified 
      multilep4 = (length . filter isMultiLep4) classified 
  putStrLn $ "total number = " ++ show totnum 
  putStrLn $ "single lep 3 = " ++ show singlelep3 
  putStrLn $ "single lep 4 = " ++ show singlelep4 
  putStrLn $ "single lep soft = " ++ show singlelepsoft
  putStrLn $ "multi lep 2 = " ++ show multilep2
  putStrLn $ "multi lep 4 = " ++ show multilep4 
  let normalize_ = normalize xsec totnum 
  let context "totnum" = MuVariable totnum 
      context "singlelep3" = MuVariable singlelep3
      context "singlelep4" = MuVariable singlelep4 
      context "singlelepsoft" = MuVariable singlelepsoft
      context "multilep2" = MuVariable multilep2 
      context "multilep4" = MuVariable multilep4 
      context "normsinglelep3" = (MuVariable . sciformat_ . normalize_) singlelep3
      context "normsinglelep4" = (MuVariable . sciformat_ . normalize_) singlelep4
      context "normsinglelepsoft" = (MuVariable . sciformat_ . normalize_) singlelepsoft
      context "normmultilep2" = (MuVariable . sciformat_ . normalize_) multilep2
      context "normmultilep4" = (MuVariable . sciformat_ . normalize_) multilep4
      context "mgluino" = MuVariable (floor mgluino :: Int)
      context "msquark" = MuVariable (floor msquark :: Int) 
      context "xsec" = MuVariable (sciformat_ (Just xsec))
      context _ = MuNothing
  res <- hastacheStr (defaultConfig {muEscapeFunc=emptyEscape}) (encodeStr template) (mkStrContext context) 
  LB.hPutStr h res



