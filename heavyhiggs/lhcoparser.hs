{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

import qualified Codec.Zlib as Zlib
import           Control.Applicative
import           Control.DeepSeq
-- import qualified Control.Foldl as Foldl
import           Control.Lens
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Maybe
import           Control.Monad.Trans.State.Strict
import qualified Data.Attoparsec.Text as PA
import qualified Data.ByteString.Lazy.Char8 as LB
import           Data.Default       
import qualified Data.Foldable as F
import           Data.List (foldl')
import           Data.Maybe (catMaybes,listToMaybe)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Traversable as Tr
import           Pipes
import qualified Pipes.ByteString  as PB
import qualified Pipes.Prelude as PPrelude
import qualified Pipes.Zlib as PZ
import           System.IO as IO
import           Text.Printf
--
import qualified HROOT
-- 
import HEP.Automation.EventGeneration.Config
import HEP.Automation.MadGraph.Model
import HEP.Automation.MadGraph.Model.SM
import HEP.Automation.MadGraph.SetupType
import HEP.Automation.MadGraph.Type
import HEP.Automation.MadGraph.Util
import HEP.Parser.LHCOAnalysis.PhysObj
import HEP.Parser.LHCOAnalysis.Parse
import HEP.Storage.WebDAV.CURL
import HEP.Storage.WebDAV.Type
--
import HEP.Physics.Analysis.Common.Lens
import HEP.Physics.Analysis.Common.Merge
import HEP.Physics.Analysis.Common.PhyEventNoTau
import HEP.Physics.Analysis.Common.PhyEventNoTauNoBJet
import Pipes.LHCO


-- | 
psetup :: ProcessSetup SM
psetup = PS {  
    model = SM
  , process = MGProc [] [ "e+ e- > j j QED=2" ]
  , processBrief = "eejj" 
  , workname   = "eejj"
  , hashSalt = HashSalt Nothing
  }

-- | 
pset :: ModelParam SM
pset = SMParam

-- | 
rsetup :: Double -> Int -> RunSetup
rsetup sqrts n = 
    RS { numevent = 100000
       , machine = Parton (0.5*sqrts) Tevatron -- ATLAS -- LHC14 ATLAS
       , rgrun   = Fixed 
       , rgscale = 200.0
       , match   = NoMatch
       , cut     = NoCut -- DefCut 
       , pythia  = RunPYTHIA 
       , lhesanitizer = [] 
       , pgs     = RunPGS (Cone 0.4, WithTau)
       , uploadhep = NoUploadHEP
       , setnum  = n
       } 

-- genset = [ (sqrts,n) | sqrts <- [200,400..3000],  n <- [1] ]

rdir :: WebDAVRemoteDir
rdir = WebDAVRemoteDir "montecarlo/HeavyHiggs/standardcandle"

checkAndDownload :: WebDAVConfig -> WebDAVRemoteDir -> FilePath -> MaybeT IO ()
checkAndDownload cfg rdir fpath = do
    b <- liftIO $ doesFileExistInDAV cfg rdir fpath
    guard b
    liftIO $ downloadFile False cfg rdir fpath 
    liftIO $ putStrLn $ fpath ++ " is successfully downloaded"


prepare sqrts = do
  let pkey = "/home/wavewave/temp/madgraph/priv.txt"
      pswd = "/home/wavewave/temp/madgraph/cred.txt" 
  Just cr <- getCredential pkey pswd
  let whost = "http://top.physics.lsa.umich.edu:10080/webdav/"
      wdavcfg = WebDAVConfig cr whost
      rname = makeRunName psetup pset (rsetup sqrts 1)
      fname = rname ++ "_pgs_events.lhco.gz"
  runMaybeT $ do 
    -- checkAndDownload wdavcfg rdir fname
    return fname

parseLHCO fpath = do 
    hin <- liftIO $ openFile fpath ReadMode
    pipesLHCOEvent (gunzip hin >-> PPrelude.map T.decodeUtf8)
    liftIO $ hClose hin

etaSlice e delta (x,_) = ((x > e-delta) && (x < e+delta)) || ( (x > -e-delta) && (x < -e+delta) )

maybeToBool (Just b) = b
maybeToBool Nothing = False


main :: IO ()
main = do
    -- withFile "standardcandle_ATLAS.dat" WriteMode $ \h -> mapM_ (analysis1File h) [200,400..3000]
    withFile "standardcandle_TEVATRON.dat" WriteMode $ \h -> mapM_ (analysis1File h) [100,200..1500]

ptrange e eta = let pt0 = e / cosh eta
                in (pt0*0.5, pt0*1.5)

analysis1File :: Handle -> Int -> IO ()
analysis1File hdl n = do
    Just fname <- prepare (fromIntegral n) -- 200
    let e = fromIntegral n * 0.5
        (pt1_025,pt2_025) = ptrange e 0.25
        (pt1_073,pt2_073) = ptrange e 0.73
        (pt1_120,pt2_120) = ptrange e 1.20
        (pt1_170,pt2_170) = ptrange e 1.70
        (pt1_220,pt2_220) = ptrange e 2.20
         
    h0 <- HROOT.newTH1F "h0" "h0" 100 0 1500
    h1 <- HROOT.newTH1F "h025" "h025" 100 pt1_025 pt2_025
    h2 <- HROOT.newTH1F "h073" "h073" 100 pt1_073 pt2_073
    h3 <- HROOT.newTH1F "h120" "h120" 100 pt1_120 pt2_120
    h4 <- HROOT.newTH1F "h170" "h170" 100 pt1_170 pt2_170
    h5 <- HROOT.newTH1F "h220" "h220" 100 pt1_220 pt2_220
    f0 <- HROOT.newTF1 "f0" "gaus" 0 2000
    f1 <- HROOT.newTF1 "f025" "gaus" 0 2000
    f2 <- HROOT.newTF1 "f073" "gaus" 0 2000
    f3 <- HROOT.newTF1 "f120" "gaus" 0 2000
    f4 <- HROOT.newTF1 "f170" "gaus" 0 2000
    f5 <- HROOT.newTF1 "f220" "gaus" 0 2000

    runEffect $ 
      parseLHCO fname 
      >-> PPrelude.tee (countmark 1000 0 >-> PPrelude.drain)
      >-> PPrelude.map (listToMaybe . map ((,)<$>eta<*>pt) . view jets . mkPhyEventNoTau)
      >-> pipeCatMaybes
      >-> PPrelude.tee (record h0 _2 >-> PPrelude.drain)
      >-> PPrelude.tee (PPrelude.filter (etaSlice 0.25 0.05) >-> record h1 _2 >-> PPrelude.drain)
      >-> PPrelude.tee (PPrelude.filter (etaSlice 0.73 0.05) >-> record h2 _2 >-> PPrelude.drain)
      >-> PPrelude.tee (PPrelude.filter (etaSlice 1.2  0.05) >-> record h3 _2 >-> PPrelude.drain)
      >-> PPrelude.tee (PPrelude.filter (etaSlice 1.7  0.05) >-> record h4 _2 >-> PPrelude.drain)
      >-> PPrelude.tee (PPrelude.filter (etaSlice 2.2  0.05) >-> record h5 _2 >-> PPrelude.drain)
      >-> PPrelude.drain
    mapM_ (\(h,f,x1,x2) -> HROOT.fit h f "" "" x1 x2) 
      [ (h0,f0,0,1500)
      , (h1,f1,pt1_025,pt2_025)
      , (h2,f2,pt1_073,pt2_073)
      , (h3,f3,pt1_120,pt2_120)
      , (h4,f4,pt1_170,pt2_170)
      , (h5,f5,pt1_220,pt2_220) ]
    (mean0,sigma0) <- (,) <$> getMean f0 <*> getSigma f0
    (mean1,sigma1) <- (,) <$> getMean f1 <*> getSigma f1
    (mean2,sigma2) <- (,) <$> getMean f2 <*> getSigma f2
    (mean3,sigma3) <- (,) <$> getMean f3 <*> getSigma f3
    (mean4,sigma4) <- (,) <$> getMean f4 <*> getSigma f4
    (mean5,sigma5) <- (,) <$> getMean f5 <*> getSigma f5
    mapM_ (\(e,m,s) -> hPutStrLn hdl (printf " %d  %.5f  %.5f   %.5f  " (n `div` 2) e m s))
      [ (0.00 :: Double, mean0, sigma0)
      , (0.25 :: Double ,mean1,sigma1)
      , (0.73,mean2,sigma2)
      , (1.20,mean3,sigma3)
      , (1.70,mean4,sigma4)
      , (2.20,mean5,sigma5) ]
    return ()

getMean :: HROOT.TF1 -> IO Double
getMean f = realToFrac <$> HROOT.tFormulaGetParameter (HROOT.upcastTFormula f) "Mean" 

getSigma :: HROOT.TF1 -> IO Double
getSigma f = realToFrac <$> HROOT.tFormulaGetParameter (HROOT.upcastTFormula f) "Sigma" 


record h1 l = do 
    x <- await
    liftIO $ HROOT.fill1 h1 (realToFrac (view l x))
    record h1 l

pipeCatMaybes = do 
    x <- await
    case x of
      Nothing -> pipeCatMaybes
      Just y -> yield y >> pipeCatMaybes


countmark :: (MonadIO m) => Int -> Int -> Pipe a a m r
countmark marker n = go n
  where go i = do when (i `mod` marker == 0) $ do 
                    liftIO (print i)
                  x <- await
                  yield x
                  go (i+1) 

printer :: (MonadIO m, Show a) => Pipe a a m r
printer = do x <- await 
             liftIO $ (print x)
             printer 
