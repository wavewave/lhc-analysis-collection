{-# LANGUAGE RecordWildCards, GADTs #-}

import           Codec.Compression.GZip
{- import           Control.Monad
import qualified Data.ByteString.Lazy.Char8 as LB
import           Data.Maybe
import qualified Data.Text.Lazy as T
import           Data.Text.Lazy.Builder 
import           Data.Text.Format 
import           System.Environment -}
import           System.FilePath
import           System.IO 
-- import           Text.Hastache
-- import           Text.Hastache.Context 
-- 
import           HEP.Parser.LHCOAnalysis.Parse
-- 
import           HEP.Physics.Analysis.ATLAS.SUSY
import           HEP.Physics.Analysis.ATLAS.SUSY.Format
-- 
import Debug.Trace

{-
log10 x = log x / log 10

getExponent10 x = floor (log10 x)

getBody10 x = 10**(log10 x - fromIntegral (getExponent10 x))

sciformat (Just x) = 
  let e = getExponent10 x
      b = getBody10 x 
      -- trunced = (fromIntegral (floor (b*100)) / 100.0) * (10.0**fromIntegral e) 
  in if e `elem` [-2,-1,0,1,2] 
     then (T.unpack . toLazyText . fixed (2+(-e))) x
     else "$" ++ ((T.unpack . toLazyText . fixed 2 . getBody10) x) ++ "\\times 10^{" 
                      ++ (show e) ++ "}$" 
sciformat (Nothing) = "0"
-}


filelist1000 = 
  [ ( 1000, 300, 3.67e-3
    , "data20130219/ADMXQLD311MST300.0MG1000.0MSQ50000.0_gluinopair_stopdecayfull_LHC7ATLAS_NoMatch_NoCut_Cone0.4_Set1_pgs_events.lhco.gz")
  , ( 1000, 400, 3.67e-3
    , "data20130219/ADMXQLD311MST400.0MG1000.0MSQ50000.0_gluinopair_stopdecayfull_LHC7ATLAS_NoMatch_NoCut_Cone0.4_Set1_pgs_events.lhco.gz")
  , ( 1000, 500, 3.67e-3
    , "data20130219/ADMXQLD311MST500.0MG1000.0MSQ50000.0_gluinopair_stopdecayfull_LHC7ATLAS_NoMatch_NoCut_Cone0.4_Set1_pgs_events.lhco.gz")
  , ( 1000, 600, 3.67e-3
    , "data20130219/ADMXQLD311MST600.0MG1000.0MSQ50000.0_gluinopair_stopdecayfull_LHC7ATLAS_NoMatch_NoCut_Cone0.4_Set1_pgs_events.lhco.gz")
  , ( 1000, 700, 3.67e-3
    , "data20130219/ADMXQLD311MST700.0MG1000.0MSQ50000.0_gluinopair_stopdecayfull_LHC7ATLAS_NoMatch_NoCut_Cone0.4_Set1_pgs_events.lhco.gz")
  , ( 1000, 800, 3.67e-3
    , "data20130219/ADMXQLD311MST800.0MG1000.0MSQ50000.0_gluinopair_stopdecayfull_LHC7ATLAS_NoMatch_NoCut_Cone0.4_Set1_pgs_events.lhco.gz") ]


filelist900 = 
  [ ( 900, 100, 0.0101
    , "data20130221/ADMXQLD311MST100.0MG900.0MSQ50000.0_gluinopair_stopdecayfull_LHC7ATLAS_NoMatch_NoCut_Cone0.4_Set1_pgs_events.lhco.gz")
  , ( 900, 200, 0.0101
    , "data20130221/ADMXQLD311MST200.0MG900.0MSQ50000.0_gluinopair_stopdecayfull_LHC7ATLAS_NoMatch_NoCut_Cone0.4_Set1_pgs_events.lhco.gz")
  , ( 900, 300, 0.0101
    , "data20130219/ADMXQLD311MST300.0MG900.0MSQ50000.0_gluinopair_stopdecayfull_LHC7ATLAS_NoMatch_NoCut_Cone0.4_Set1_pgs_events.lhco.gz")
  , ( 900, 400, 0.0101
    , "data20130219/ADMXQLD311MST400.0MG900.0MSQ50000.0_gluinopair_stopdecayfull_LHC7ATLAS_NoMatch_NoCut_Cone0.4_Set1_pgs_events.lhco.gz")
  , ( 900, 500, 0.0101
    , "data20130219/ADMXQLD311MST500.0MG900.0MSQ50000.0_gluinopair_stopdecayfull_LHC7ATLAS_NoMatch_NoCut_Cone0.4_Set1_pgs_events.lhco.gz")
  , ( 900, 600, 0.0101
    , "data20130219/ADMXQLD311MST600.0MG900.0MSQ50000.0_gluinopair_stopdecayfull_LHC7ATLAS_NoMatch_NoCut_Cone0.4_Set1_pgs_events.lhco.gz")
  , ( 900, 700, 0.0101
    , "data20130219/ADMXQLD311MST700.0MG900.0MSQ50000.0_gluinopair_stopdecayfull_LHC7ATLAS_NoMatch_NoCut_Cone0.4_Set1_pgs_events.lhco.gz")
  ]

filelist800 = 
  [ ( 800, 100, 0.0290
    , "data20130221/ADMXQLD311MST100.0MG800.0MSQ50000.0_gluinopair_stopdecayfull_LHC7ATLAS_NoMatch_NoCut_Cone0.4_Set1_pgs_events.lhco.gz")
  , ( 800, 200, 0.0290
    , "data20130221/ADMXQLD311MST200.0MG800.0MSQ50000.0_gluinopair_stopdecayfull_LHC7ATLAS_NoMatch_NoCut_Cone0.4_Set1_pgs_events.lhco.gz")
  , ( 800, 300, 0.0290
    , "data20130221/ADMXQLD311MST300.0MG800.0MSQ50000.0_gluinopair_stopdecayfull_LHC7ATLAS_NoMatch_NoCut_Cone0.4_Set1_pgs_events.lhco.gz")
  , ( 800, 400, 0.0290
    , "data20130221/ADMXQLD311MST400.0MG800.0MSQ50000.0_gluinopair_stopdecayfull_LHC7ATLAS_NoMatch_NoCut_Cone0.4_Set1_pgs_events.lhco.gz")
  , ( 800, 500, 0.0290
    , "data20130221/ADMXQLD311MST500.0MG800.0MSQ50000.0_gluinopair_stopdecayfull_LHC7ATLAS_NoMatch_NoCut_Cone0.4_Set1_pgs_events.lhco.gz")
  , ( 800, 600, 0.0290
    , "data20130221/ADMXQLD311MST600.0MG800.0MSQ50000.0_gluinopair_stopdecayfull_LHC7ATLAS_NoMatch_NoCut_Cone0.4_Set1_pgs_events.lhco.gz")
  ]


filelist700 = 
  [ ( 700, 100, 0.0885
    , "data20130221/ADMXQLD311MST100.0MG700.0MSQ50000.0_gluinopair_stopdecayfull_LHC7ATLAS_NoMatch_NoCut_Cone0.4_Set1_pgs_events.lhco.gz")
  , ( 700, 200, 0.0885
    , "data20130221/ADMXQLD311MST200.0MG700.0MSQ50000.0_gluinopair_stopdecayfull_LHC7ATLAS_NoMatch_NoCut_Cone0.4_Set1_pgs_events.lhco.gz")
  , ( 700, 300, 0.0885
    , "data20130221/ADMXQLD311MST300.0MG700.0MSQ50000.0_gluinopair_stopdecayfull_LHC7ATLAS_NoMatch_NoCut_Cone0.4_Set1_pgs_events.lhco.gz")
  , ( 700, 400, 0.0885
    , "data20130221/ADMXQLD311MST400.0MG700.0MSQ50000.0_gluinopair_stopdecayfull_LHC7ATLAS_NoMatch_NoCut_Cone0.4_Set1_pgs_events.lhco.gz")
  , ( 700, 500, 0.0885
    , "data20130221/ADMXQLD311MST500.0MG700.0MSQ50000.0_gluinopair_stopdecayfull_LHC7ATLAS_NoMatch_NoCut_Cone0.4_Set1_pgs_events.lhco.gz")
  ]



filelist600 = 
  [ ( 600, 100, 0.295
    , "data20130221/ADMXQLD311MST100.0MG600.0MSQ50000.0_gluinopair_stopdecayfull_LHC7ATLAS_NoMatch_NoCut_Cone0.4_Set1_pgs_events.lhco.gz")
  , ( 600, 200, 0.295
    , "data20130221/ADMXQLD311MST200.0MG600.0MSQ50000.0_gluinopair_stopdecayfull_LHC7ATLAS_NoMatch_NoCut_Cone0.4_Set1_pgs_events.lhco.gz")
  , ( 600, 300, 0.295
    , "data20130221/ADMXQLD311MST300.0MG600.0MSQ50000.0_gluinopair_stopdecayfull_LHC7ATLAS_NoMatch_NoCut_Cone0.4_Set1_pgs_events.lhco.gz")
  , ( 600, 400, 0.295
    , "data20130221/ADMXQLD311MST400.0MG600.0MSQ50000.0_gluinopair_stopdecayfull_LHC7ATLAS_NoMatch_NoCut_Cone0.4_Set1_pgs_events.lhco.gz")
  ]

filelist300400500 = 
  [ ( 300, 100, 32.0
    , "data20130219/ADMXQLD311MST100.0MG300.0MSQ50000.0_gluinopair_stopdecayfull_LHC7ATLAS_NoMatch_NoCut_Cone0.4_Set1_pgs_events.lhco.gz")
  , ( 400, 100, 5.14
    , "data20130219/ADMXQLD311MST100.0MG400.0MSQ50000.0_gluinopair_stopdecayfull_LHC7ATLAS_NoMatch_NoCut_Cone0.4_Set1_pgs_events.lhco.gz")
  , ( 400, 200, 5.14
    , "data20130219/ADMXQLD311MST200.0MG400.0MSQ50000.0_gluinopair_stopdecayfull_LHC7ATLAS_NoMatch_NoCut_Cone0.4_Set1_pgs_events.lhco.gz")
  , ( 500, 100, 1.12
    , "data20130219/ADMXQLD311MST100.0MG500.0MSQ50000.0_gluinopair_stopdecayfull_LHC7ATLAS_NoMatch_NoCut_Cone0.4_Set1_pgs_events.lhco.gz")
  , ( 500, 200, 1.12
    , "data20130219/ADMXQLD311MST200.0MG500.0MSQ50000.0_gluinopair_stopdecayfull_LHC7ATLAS_NoMatch_NoCut_Cone0.4_Set1_pgs_events.lhco.gz")
  , ( 500, 300, 1.12
    , "data20130219/ADMXQLD311MST300.0MG500.0MSQ50000.0_gluinopair_stopdecayfull_LHC7ATLAS_NoMatch_NoCut_Cone0.4_Set1_pgs_events.lhco.gz")
  ]


scharm300400500 = 
  [ ( 300, 100, 32.0
    , "data20130222/ADMXQLD211MST50000.0MG300.0MSQ100.0_gluinopair_scharmdecayfull_LHC7ATLAS_NoMatch_NoCut_Cone0.4_Set1_pgs_events.lhco.gz")
  , ( 400, 100, 5.14
    , "data20130222/ADMXQLD211MST50000.0MG400.0MSQ100.0_gluinopair_scharmdecayfull_LHC7ATLAS_NoMatch_NoCut_Cone0.4_Set1_pgs_events.lhco.gz")
  , ( 400, 200, 5.14
    , "data20130222/ADMXQLD211MST50000.0MG400.0MSQ200.0_gluinopair_scharmdecayfull_LHC7ATLAS_NoMatch_NoCut_Cone0.4_Set1_pgs_events.lhco.gz")
  , ( 500, 100, 1.12
    , "data20130222/ADMXQLD211MST50000.0MG500.0MSQ100.0_gluinopair_scharmdecayfull_LHC7ATLAS_NoMatch_NoCut_Cone0.4_Set1_pgs_events.lhco.gz")
  , ( 500, 200, 1.12
    , "data20130222/ADMXQLD211MST50000.0MG500.0MSQ200.0_gluinopair_scharmdecayfull_LHC7ATLAS_NoMatch_NoCut_Cone0.4_Set1_pgs_events.lhco.gz")
  , ( 500, 300, 1.12
    , "data20130222/ADMXQLD211MST50000.0MG500.0MSQ300.0_gluinopair_scharmdecayfull_LHC7ATLAS_NoMatch_NoCut_Cone0.4_Set1_pgs_events.lhco.gz")
  ]

scharm800 = 
  [ ( 800, 200, 0.0290
    , "data20130222/ADMXQLD211MST50000.0MG800.0MSQ200.0_gluinopair_scharmdecayfull_LHC7ATLAS_NoMatch_NoCut_Cone0.4_Set1_pgs_events.lhco.gz")
  , ( 800, 300, 0.0290
    , "data20130222/ADMXQLD211MST50000.0MG800.0MSQ300.0_gluinopair_scharmdecayfull_LHC7ATLAS_NoMatch_NoCut_Cone0.4_Set1_pgs_events.lhco.gz")
  , ( 800, 400, 0.0290
    , "data20130222/ADMXQLD211MST50000.0MG800.0MSQ400.0_gluinopair_scharmdecayfull_LHC7ATLAS_NoMatch_NoCut_Cone0.4_Set1_pgs_events.lhco.gz")
  , ( 800, 500, 0.0290
    , "data20130222/ADMXQLD211MST50000.0MG800.0MSQ500.0_gluinopair_scharmdecayfull_LHC7ATLAS_NoMatch_NoCut_Cone0.4_Set1_pgs_events.lhco.gz")
  , ( 800, 600, 0.0290
    , "data20130222/ADMXQLD211MST50000.0MG800.0MSQ600.0_gluinopair_scharmdecayfull_LHC7ATLAS_NoMatch_NoCut_Cone0.4_Set1_pgs_events.lhco.gz")
  ]

scharm900 = 
  [ ( 900, 200, 0.0101
    , "data20130222/ADMXQLD211MST50000.0MG900.0MSQ200.0_gluinopair_scharmdecayfull_LHC7ATLAS_NoMatch_NoCut_Cone0.4_Set1_pgs_events.lhco.gz")
  , ( 900, 300, 0.0101
    , "data20130222/ADMXQLD211MST50000.0MG900.0MSQ300.0_gluinopair_scharmdecayfull_LHC7ATLAS_NoMatch_NoCut_Cone0.4_Set1_pgs_events.lhco.gz")
  , ( 900, 400, 0.0101
    , "data20130222/ADMXQLD211MST50000.0MG900.0MSQ400.0_gluinopair_scharmdecayfull_LHC7ATLAS_NoMatch_NoCut_Cone0.4_Set1_pgs_events.lhco.gz")
  , ( 900, 500, 0.0101
    , "data20130222/ADMXQLD211MST50000.0MG900.0MSQ500.0_gluinopair_scharmdecayfull_LHC7ATLAS_NoMatch_NoCut_Cone0.4_Set1_pgs_events.lhco.gz")
  , ( 900, 600, 0.0101
    , "data20130222/ADMXQLD211MST50000.0MG900.0MSQ600.0_gluinopair_scharmdecayfull_LHC7ATLAS_NoMatch_NoCut_Cone0.4_Set1_pgs_events.lhco.gz")
  , ( 900, 700, 0.0101
    , "data20130222/ADMXQLD211MST50000.0MG900.0MSQ700.0_gluinopair_scharmdecayfull_LHC7ATLAS_NoMatch_NoCut_Cone0.4_Set1_pgs_events.lhco.gz")
  ]

scharm1000 = 
  [ ( 1000, 400, 3.67e-3
    , "data20130222/ADMXQLD211MST50000.0MG1000.0MSQ400.0_gluinopair_stopdecayfull_LHC7ATLAS_NoMatch_NoCut_Cone0.4_Set1_pgs_events.lhco.gz")
  , ( 1000, 500, 3.67e-3
    , "data20130222/ADMXQLD211MST50000.0MG1000.0MSQ500.0_gluinopair_stopdecayfull_LHC7ATLAS_NoMatch_NoCut_Cone0.4_Set1_pgs_events.lhco.gz")
  , ( 1000, 600, 3.67e-3
    , "data20130222/ADMXQLD211MST50000.0MG1000.0MSQ600.0_gluinopair_stopdecayfull_LHC7ATLAS_NoMatch_NoCut_Cone0.4_Set1_pgs_events.lhco.gz")
  , ( 1000, 700, 3.67e-3
    , "data20130222/ADMXQLD211MST50000.0MG1000.0MSQ700.0_gluinopair_stopdecayfull_LHC7ATLAS_NoMatch_NoCut_Cone0.4_Set1_pgs_events.lhco.gz")
  , ( 1000, 800, 3.67e-3
    , "data20130222/ADMXQLD211MST50000.0MG1000.0MSQ800.0_gluinopair_stopdecayfull_LHC7ATLAS_NoMatch_NoCut_Cone0.4_Set1_pgs_events.lhco.gz") ]






main = do 
  h <- openFile "output.dat" WriteMode 
  hPutStr h header
  mapM_ (analysis h) scharm1000
  hPutStr h footer 
  hClose h 

  

