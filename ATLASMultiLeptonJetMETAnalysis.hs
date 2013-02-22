{-# LANGUAGE RecordWildCards, GADTs #-}

import Codec.Compression.GZip
import Control.Monad
import qualified Data.ByteString.Lazy.Char8 as LB
import Data.Maybe
import System.FilePath
import System.Environment
import Text.Hastache
import Text.Hastache.Context
-- 
import HEP.Parser.LHCOAnalysis.Parse
-- 
import HEP.Physics.Analysis.ATLAS.SUSY
-- 
import Debug.Trace

template = " ({{mgluino}},{{mstop}}) & {{xsec}} & {{totnum}} & {{singlelep3}} & {{singlelep4}} & {{singlelepsoft}} & {{multilep2}} & {{multilep4}} \\\\" 

  
main = do 
  putStrLn "atlas counting"
  args <- getArgs
  when (length args /= 1) $ error "./parsertest filename"
  let fn = args !! 0 
      basename = takeBaseName fn 
 
  bstr <- LB.readFile fn 
  let unzipped = decompress bstr 

      evts = parsestr unzipped
      signalevts = map (preselect HardLepton . taubjetMerge) evts 
  -- print $ map meff signalevts
      classified = mapMaybe classifyEvent signalevts 
  -- print (length evts) 
  putStrLn fn 

  let totnum = length evts
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

  let context "totnum" = MuVariable totnum 
      context "singlelep3" = MuVariable singlelep3
      context "singlelep4" = MuVariable singlelep4 
      context "singlelepsoft" = MuVariable singlelepsoft
      context "multilep2" = MuVariable multilep2 
      context "multilep4" = MuVariable multilep4 
      context _ = MuNothing
  res <- hastacheStr defaultConfig (encodeStr template) (mkStrContext context) 
  LB.putStrLn res
 
  

    
  -- LB.putStrLn (LB.take 100 unzipped) 


