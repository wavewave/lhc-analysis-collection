{-# LANGUAGE OverloadedStrings #-}

import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Data.Conduit
import qualified Data.Conduit.List as CL
import           Data.Either
import qualified Data.Foldable as F
import qualified Data.IntMap as IM
import qualified Data.Text.IO as TIO
import System.IO
--
import Data.Conduit.Util.Print
import HEP.Parser.LHE.Conduit
import HEP.Parser.LHE.DecayTop
import HEP.Parser.LHE.Type
-- import HEP.Physics.TTBar.Analysis.PartonConstruction
-- import HEP.Physics.TTBar.Analysis.TopPairParton
-- import HEP.Physics.TTBar.Type
import HEP.Util.Functions
import Text.XML.Conduit.Parse.Util

import HROOT

main = do 
  c1 <- newTCanvas "deltaeta" "deltaeta" 1024 768
  hist <- newTH1F "deltaeta" "deltaeta" 50 (-5) 5 

  withFile "hefthiggs2ttbar_1000.lhe" ReadMode $ \ih -> do
    let iter = do
          header <- textLHEHeader
          parseEvent =$ process
          -- liftIO $  (mapM_ print . lefts) lst
        process = -- decayTopConduit 
                  -- =$ CL.isolate 100
                  -- =$ CL.mapM_ print
                  -- =$ 
                  -- CL.map (fmap (view _2  . mom_2_pt_eta_phi . pupTo4mom . pup . ptlinfo . fst) .  ttbar . head . lhet_dtops)
                  CL.mapM_ (const (return ()) <=< fill1 hist . realToFrac . diff . {- mketa . fst . -}   ttbar)
                  -- =$ CL.mapM_ customfunc
                  -- =$ printIter
                  -- =$ CL.consume
                  =$ CL.sinkNull 

    parseXmlFile ih iter

    draw hist ""
    saveAs c1 "deltaeta.png" ""


mketa = view _2  . mom_2_pt_eta_phi . pupTo4mom . pup

diff (x,y) = mketa x - mketa y

-- ttbar :: DecayTop PtlIDInfo ->  Either (DecayTop PtlIDInfo) (PtlIDInfo,PtlIDInfo)
-- ttbar (Decay (_, [Terminal x, Terminal y])) = if pdgid x == 6 then Right (x,y) else Right (y,x)
-- ttbar x = Left x


ttbar :: LHEvent -> (PtlInfo,PtlInfo)
ttbar (LHEvent _ ps) = let t    = ( head . filter (\p -> idup p == 6) ) ps
                           tbar = ( head . filter (\p -> idup p == (-6)) ) ps 
                       in (t,tbar)
