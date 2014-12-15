{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

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
import           Data.Maybe (catMaybes)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Traversable as Tr
import           Pipes
import qualified Pipes.ByteString  as PB
import qualified Pipes.Prelude as PPrelude
import qualified Pipes.Zlib as PZ
import           System.Environment
import           System.IO as IO
import           Text.Printf
-- 
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


mergeBJetFromNoTauEv :: PhyEventNoTau -> PhyEventNoTauNoBJet
mergeBJetFromNoTauEv ev =
  let alljs = (map (\case JO_Jet j ->j ; JO_BJet b -> bJet2Jet b) . view jetBJets) ev
      es = view electrons $ ev  
      ms = view muons     $ ev
  in ( set electrons es 
     . set muons ms    
     . set jets alljs 
     . set eventId (view eventId ev)
     . set photons (view photons ev)
     . set missingET (view missingET ev) ) def


psetup :: ProcessSetup SM
psetup = PS { model = SM
            , process = MGProc [] [ "p p > t t~     QCD=99 QED=2 @0" 
                                  , "p p > t t~ j   QCD=99 QED=2 @1" 
                                  , "p p > t t~ j j QCD=99 QED=2 @2" ] 
            , processBrief = "tt012j"
            , workname = "tt012j"
            , hashSalt = HashSalt Nothing }

param :: ModelParam SM
param = SMParam

rsetupgen  :: Int -> RunSetup 
rsetupgen x = 
    RS { numevent = 10000 
       , machine  = LHC14 ATLAS
       , rgrun    = Auto 
       , rgscale  = 91.0
       , match    = MLM
       , cut      = DefCut
       , pythia   = RunPYTHIA
       , lhesanitizer  = []
       , pgs      = RunPGS (AntiKTJet 0.4, WithTau)
       , uploadhep = NoUploadHEP
       , setnum   = x 
       }


rdir :: WebDAVRemoteDir
rdir = WebDAVRemoteDir "newtest2"

checkAndDownload :: WebDAVConfig -> WebDAVRemoteDir -> FilePath -> MaybeT IO ()
checkAndDownload cfg rdir fpath = do
    b <- liftIO $ doesFileExistInDAV cfg rdir fpath
    guard b
    liftIO $ downloadFile False cfg rdir fpath 
    liftIO $ putStrLn $ fpath ++ " is successfully downloaded"


fourtopsimpl400 = map (\x->"data/fourtopsimpl_400_set" ++ x ++  "_pgs_events.lhco.gz") 
                      [ "1", "2", "3", "4", "5", "6", "7", "8", "9", "10" ]

fourtopsimpl750 = map (\x->"data/fourtopsimpl_750_set" ++ x ++  "_pgs_events.lhco.gz") 
                      [ "1", "2", "3", "4", "5", "6", "7", "8", "9", "10" ]

ttbarset = map (\x->"data/" ++ x ++ "_pgs_events.lhco.gz") . map (\x -> makeRunName psetup param (rsetupgen x)) $ [1..1000] 

    -- let fpaths = map (++ "_pgs_events.lhco.gz") . map (\x -> makeRunName psetup param (rsetupgen x)) $ [1..1000] 



data CutChoice = CutChoice { choice_ht :: Double
                           , choice_ptj1 :: Double
                           , choice_ptj234 :: Double
                           , choice_ptj56 :: Double }
               deriving (Show,Eq,Ord)

testset1 = CutChoice { choice_ht = 1500
                     , choice_ptj1 = 200
                     , choice_ptj234 = 100
                     , choice_ptj56 = 50 } 

testsets = [ CutChoice ht j1 j234 j56 | ht <- [500,800,1000,1200,1500] 
                                      , j1 <- [100,200,300]
                                      , j234 <- [50,100..j1]
                                      , j56 <- [25,50..j234] ]

work :: [FilePath] -> CutChoice -> IO (Int, Int, Int, Int, Int, Int, Int, Int, Int)
work fpaths CutChoice {..} = do
    hins <- mapM (\fpath -> openFile fpath ReadMode) fpaths  
    (_,r) <- flip runStateT (0,0,0,0,0,0,0,0,0) $ runEffect $ do
           F.forM_ fpaths $ \fpath -> do 
             hin <- liftIO $ openFile fpath ReadMode
             pipesLHCOEvent (gunzip hin >-> PPrelude.map T.decodeUtf8)
             liftIO $ hClose hin 
           >-> PPrelude.tee (count _1 >-> PPrelude.drain)
           >-> PPrelude.tee (countmark 5000 0 >-> PPrelude.drain)
           >-> PPrelude.map (((,) <$> filterEtaRange . mergeBJetFromNoTauEv <*> id) . mkPhyEventNoTau)
           >-> PPrelude.map ( over _2 (numOfB choice_ptj56) 
                            . over _1 
                               (\x->(checkj (choice_ptj1,choice_ptj234,choice_ptj56) x, checkl x, htcut choice_ht x)))
           >-> PPrelude.filter (view (_1._1._1)) >-> PPrelude.tee (count _2) -- pass1 
           >-> PPrelude.filter (view (_1._1._2)) >-> PPrelude.tee (count _3) -- pass2
           >-> PPrelude.filter (view (_1._1._3)) >-> PPrelude.tee (count _4) -- pass3
           >-> PPrelude.filter (view (_1._2))      >-> PPrelude.tee (count _5) -- pass4
           >-> PPrelude.filter (view (_1._3))      >-> PPrelude.tee (count _6) -- pass5
           >-> PPrelude.filter ((>=1) . view (_2)) >-> PPrelude.tee (count _7) -- 1 bjets, pT_bj > 50 
           >-> PPrelude.filter ((>=2) . view (_2)) >-> PPrelude.tee (count _8) -- 2 bjets, pT_bj > 50 
           >-> PPrelude.filter ((>=3) . view (_2)) >-> PPrelude.tee (count _9) -- 3 bjets, pT_bj > 50 

           -- >-> PPrelude.map (view (_1))
           -- >-> PPrelude.tee (PPrelude.print >-> PPrelude.drain)
           >-> PPrelude.drain
       
    return r

count :: Simple Lens s Int -> Consumer a (StateT s IO) ()
count l = forever $ do await 
                       x <- lift get 
                       let !y = x `seq` view l x
                           !x' = set l (y+1) x 
                       lift (put x')



ptlcut :: Double -> PhyEventNoTauNoBJet -> Bool
ptlcut cutv ev = let lst = view leptons ev
                     f x = abs (eta x) < 1.5 && pt x > cutv
                 in  any f lst


filterEtaRange :: PhyEventNoTauNoBJet -> PhyEventNoTauNoBJet
filterEtaRange = over jets (filter (\x -> abs (eta (JO_Jet x)) < 2.5)) 

jetpts :: PhyEventNoTauNoBJet -> [Double]
jetpts = map (pt . JO_Jet) . view jets

-- jetetas = map (eta . JO_Jet) . view jets

checkl :: PhyEventNoTauNoBJet -> Bool
checkl ev = let etalst = leptonetas ev
            in (not . null . filter (\x -> abs x < 1.5)) etalst

checkj :: (Double,Double,Double) -> PhyEventNoTauNoBJet -> (Bool,Bool,Bool)
checkj (j1,j234,j56) ev = 
    let ptlst = jetpts ev
    in case ptlst of
        [] -> (False,False,False)
        x:xs -> 
          let b1 = x > j1 -- 200
          in case xs of
               _:_:y:ys -> let b2 = y > j234 -- 100 
                           in case ys of 
                                _:z:_zs -> let b3 = z > j56 -- 50
                                           in (b1,b2,b3)
                                _ -> (b1,b2,False)
               _ -> (b1,False,False)


numOfB :: Double -> PhyEventNoTau -> Int
numOfB j56 ev = let ptlst = (filter (> j56{- 50 -}) . map pt . view bjets) ev
                in length ptlst 

leptonetas :: PhyEventNoTauNoBJet -> [Double]
leptonetas = map eta . view leptons

htcut :: Double -> PhyEventNoTauNoBJet -> Bool 
htcut v ev = let ptlst = jetpts ev
                 ht6 = sum (take 6 ptlst)
             in ht6 > v -- 1200


countmark :: (MonadIO m) => Int -> Int -> Pipe a a m r
countmark marker n = go n
  where go i = do when (i `mod` marker == 0) $ do 
                    liftIO (print i)
                  x <- await
                  yield x
                  go (i+1) 



format :: CutChoice -> (Int, Int, Int, Int, Int, Int, Int, Int, Int) -> String
format CutChoice {..} (full,c1,c2,c3,le,ht,b1,b2,b3) =
    printf "%6.1f %6.1f %6.1f %6.1f %7d %7d %7d %7d %7d %7d %7d %7d %7d" choice_ht choice_ptj1 choice_ptj234 choice_ptj56 full c1 c2 c3 le ht b1 b2 b3


-- main = print testsets
main :: IO ()
main = do
    args <- getArgs
    
    -- let (m,n) = (1,340)
    -- let (m,n) = (1,2)
    let m = read (args !! 0)
        n = read (args !! 1)
        filename = "outputttbar_" ++ show m ++ "_" ++ show n ++ ".dat"
    withFile filename WriteMode $ \h -> do
      F.forM_ ((take (n-m+1) . drop (m-1)) testsets) $ \x -> do
        r <- (x,) <$> work ttbarset {- fourtopsimpl400 -} x
        let str = uncurry format r
        putStrLn str
        hPutStrLn h str


