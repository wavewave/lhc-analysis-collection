module Main where

import Control.Monad
import Control.Monad.IO.Class
import Data.Conduit
import qualified Data.Conduit.List as CL
import qualified Data.Foldable as F
import qualified Data.IntMap as IM
import qualified Data.Text.IO as TIO
import System.IO
--
import Data.Conduit.Util.Print
import HEP.Parser.LHE.Conduit
import HEP.Parser.LHE.DecayTop
import HEP.Parser.LHE.Type
import HEP.Physics.TTBar.Analysis.PartonConstruction
-- import HEP.Physics.TTBar.Analysis.TopPairParton
import HEP.Physics.TTBar.Type
import Text.XML.Conduit.Parse.Util


main = do 
  withFile "unweighted_events.lhe" ReadMode $ \ih -> do
    let iter = do
          header <- textLHEHeader
          parseEvent =$ process
        process = decayTopConduit 
                  =$ CL.isolate 100
                  -- =$ CL.mapM_ print
                  =$ CL.map (analysis . lhet_dtops)
                  -- =$ CL.mapM_ customfunc
                  =$ printIter
                  -- =$ CL.sinkNull 

    parseXmlFile ih iter

analysis dtops = let (htops,_) = getHadronicTop dtops
                     (ltops,_) = getLeptonicTop dtops
                 in mkTopPairEvent ltops htops 


{-
customfunc :: (HadronicTopCollection, [DecayTop PtlIDInfo]) -> IO ()
customfunc x = let hcoll = fst x  
               in if (length (hadTop hcoll) + length (hadAntiTop hcoll) > 0  ) then putStrLn "YES" else putStrLn "NOT MATCHED" -- mapM_ print (snd x) 
-}

test :: DecayTop PtlIDInfo 
test = Decay (PIDInfo {pdgid = 6, ptlinfo = PtlInfo {ptlid = 3, idup = 6, istup = 2, mothup = (1,2), icolup = (503,0), pup = (9.5061764255,88.095886663,220.25150332,301.05286440000003,185.12387693), vtimup = 0.0, spinup = 0.0}},[Decay (PIDInfo {pdgid = 24, ptlinfo = PtlInfo {ptlid = 4, idup = 24, istup = 2, mothup = (3,3), icolup = (0,0), pup = (-41.800517995,43.725500003,209.93309773,232.26783640000002,78.849474614), vtimup = 0.0, spinup = 0.0}},[Terminal PIDInfo {pdgid = 2, ptlinfo = PtlInfo {ptlid = 7, idup = 2, istup = 1, mothup = (4,4), icolup = (502,0), pup = (-23.163426294,52.380257152,202.45587153,210.40108264,0.0), vtimup = 0.0, spinup = -1.0}},Terminal PIDInfo {pdgid = -1, ptlinfo = PtlInfo {ptlid = 8, idup = -1, istup = 1, mothup = (4,4), icolup = (0,502), pup = (-18.637091701,-8.6547571487,7.4772261967,21.866753758999998,0.0), vtimup = 0.0, spinup = 1.0}}]),Terminal PIDInfo {pdgid = 5, ptlinfo = PtlInfo {ptlid = 9, idup = 5, istup = 1, mothup = (3,3), icolup = (503,0), pup = (51.30669442,44.370386659,10.318405593,68.785028002,4.87877839), vtimup = 0.0, spinup = -1.0}}])

{-
test2 = Decay (PIDInfo {pdgid = 24, ptlinfo = PtlInfo {ptlid = 4, idup = 24, istup = 2, mothup = (3,3), icolup = (0,0), pup = (-41.800517995,43.725500003,209.93309773,232.26783640000002,78.849474614), vtimup = 0.0, spinup = 0.0}},[Terminal PIDInfo {pdgid = 2, ptlinfo = PtlInfo {ptlid = 7, idup = 2, istup = 1, mothup = (4,4), icolup = (502,0), pup = (-23.163426294,52.380257152,202.45587153,210.40108264,0.0), vtimup = 0.0, spinup = -1.0}},Terminal PIDInfo {pdgid = -1, ptlinfo = PtlInfo {ptlid = 8, idup = -1, istup = 1, mothup = (4,4), icolup = (0,502), pup = (-18.637091701,-8.6547571487,7.4772261967,21.866753758999998,0.0), vtimup = 0.0, spinup = 1.0}} ] )

test3 = Terminal PIDInfo {pdgid = 5, ptlinfo = PtlInfo {ptlid = 9, idup = 5, istup = 1, mothup = (3,3), icolup = (503,0), pup = (51.30669442,44.370386659,10.318405593,68.785028002,4.87877839), vtimup = 0.0, spinup = -1.0}}
-}

matcher_top :: DecayTop (Int,[PDGID])
matcher_top = (Decay ((1,[6]), [ matcher_W, Terminal (2,[5]) ]))

matcher_W = Decay ((3,[24]), [ Terminal (4,[-1,-2,-3,-4]),  Terminal (5,[1,2,3,4]) ])


main' = do
  print test
  let mr = (matchDecayTop matcher_top) test
  case mr of
    Nothing -> putStrLn "hell"
    Just r -> ({- F.mapM_ -} print . IM.fromList . F.toList) r

  

