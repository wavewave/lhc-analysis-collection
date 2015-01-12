{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

import qualified Codec.Zlib as Zlib
import           Control.Applicative
import           Control.DeepSeq
-- import qualified Control.Foldl as Foldl
import           Control.Lens
import           Control.Lens.TH
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Maybe
import           Control.Monad.Trans.State.Strict
import qualified Data.Attoparsec.Text as PA
import qualified Data.ByteString.Lazy.Char8 as LB
import           Data.Default       
import qualified Data.Foldable as F
import           Data.List (foldl',sortBy)
import           Data.Maybe (catMaybes)
import qualified Data.Map as M
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

fourtopsimpl1000 = map (\x->"data/fourtopsimpl_1000_set" ++ x ++  "_pgs_events.lhco.gz") 
                       [ "1", "2", "3", "4", "5", "6", "7", "8", "9", "10" ]


ttbarset = map (\x->"data/" ++ x ++ "_pgs_events.lhco.gz") . map (\x -> makeRunName psetup param (rsetupgen x)) $ [1..1000] 

    -- let fpaths = map (++ "_pgs_events.lhco.gz") . map (\x -> makeRunName psetup param (rsetupgen x)) $ [1..1000] 



data CutChoice = CutChoice { choice_ht :: Double
                           , choice_ptj1 :: Double
                           , choice_ptj234 :: Double
                           , choice_ptj56 :: Double 
                           , choice_ptl :: Double
                           }

               deriving (Show,Eq,Ord)

testset1 = CutChoice { choice_ht = 1500
                     , choice_ptj1 = 200
                     , choice_ptj234 = 100
                     , choice_ptj56 = 50 
                     , choice_ptl = 0
                     } 

testset2 = CutChoice { choice_ht =  500
                     , choice_ptj1 = 100
                     , choice_ptj234 = 50
                     , choice_ptj56 = 25 
                     , choice_ptl = 0
                     } 


testsets = [ CutChoice ht j1 j234 j56 l | ht <- [500,800,1000,1200,1500] 
                                        , j1 <- [100,200,300]
                                        , j234 <- [50,100..j1]
                                        , j56 <- [25,50..j234] 
                                        , l <- [0,20,50,100,200,300]
                                        ]

data CounterState1 = 
       CounterState1      { _counterFull :: Int
                          , _counterPass1 :: Int
                          , _counterPass2 :: Int
                          , _counterPass3 :: Int
                          , _counterPass4 :: Int
                          , _counterPass5 :: Int
                          , _counterBjet1 :: Int
                          , _counterBjet2 :: Int
                          , _counterBjet3 :: Int
                          -- , _counterL20 :: Int
                          -- , _counterL50 :: Int
                          -- , _counterL100 :: Int
                          -- , _counterL200 :: Int
                          -- , _counterL300 :: Int
                          } deriving (Show,Eq,Ord)

emptyCS1 = CounterState1 0 0 0 0 0 0 0 0 0 

makeLenses ''CounterState1
                               
emptyCS :: [CutChoice] -> M.Map CutChoice CounterState1
emptyCS lst = foldr (\k -> M.insert k emptyCS1) M.empty lst

-- CutChoice {..}

work :: [FilePath] -> [CutChoice] -> IO (M.Map CutChoice CounterState1)
work fpaths cutsets = do
    hins <- mapM (\fpath -> openFile fpath ReadMode) fpaths  
    (_,r) <- flip runStateT (emptyCS cutsets) $ runEffect $ do
           F.forM_ fpaths $ \fpath -> do 
             hin <- liftIO $ openFile fpath ReadMode
             pipesLHCOEvent (gunzip hin >-> PPrelude.map T.decodeUtf8)
             liftIO $ hClose hin 
           -- >-> PPrelude.take 1000
           >-> PPrelude.tee (countmark 5000 0 >-> PPrelude.drain)
           >-> PPrelude.map (((,,) <$> filterEtaRange . mergeBJetFromNoTauEv <*> id <*> id) . mkPhyEventNoTau)
           >-> (foldr1 (>->) . map (PPrelude.tee. analysis)) cutsets
           >-> PPrelude.drain
    return r
  where 
    analysis cutchoice@CutChoice {..} = 
           PPrelude.tee (count cutchoice counterFull >-> PPrelude.drain)
           >-> PPrelude.map ( over _2 (numOfB choice_ptj56) 
                            . over _1 
                               (\x->( checkj (choice_ptj1,choice_ptj234,choice_ptj56) x
                                    , ptlcut choice_ptl x -- && etalcut x
                                    , htcut choice_ht x)))
           >-> PPrelude.filter (view (_1._1._1)) >-> PPrelude.tee (count cutchoice counterPass1) 
           >-> PPrelude.filter (view (_1._1._2)) >-> PPrelude.tee (count cutchoice counterPass2) 
           >-> PPrelude.filter (view (_1._1._3)) >-> PPrelude.tee (count cutchoice counterPass3) 
           >-> PPrelude.filter (view (_1._2))      >-> PPrelude.tee (count cutchoice counterPass4)   -- eta cut
           >-> PPrelude.filter (view (_1._3))      >-> PPrelude.tee (count cutchoice counterPass5)
           >-> PPrelude.filter ((>=1) . view (_2)) >-> PPrelude.tee (count cutchoice counterBjet1)
           >-> PPrelude.filter ((>=2) . view (_2)) >-> PPrelude.tee (count cutchoice counterBjet2)
           >-> PPrelude.filter ((>=3) . view (_2)) >-> PPrelude.tee (count cutchoice counterBjet3)
           >-> PPrelude.drain

           -- >-> PPrelude.map (over _3 (sortBy (flip compare)  . map pt . view leptons)) 
           -- >-> PPrelude.tee (PPrelude.print >-> PPrelude.drain)

           --  >-> PPrelude.filter ((>=20) .head . view _3) >-> PPrelude.tee (count counterL20)
           -- >-> PPrelude.filter ((>=50) .head . view _3) >-> PPrelude.tee (count counterL50)           
           --  >-> PPrelude.filter ((>=100) . head . view _3) >-> PPrelude.tee (count counterL100)
           --  >-> PPrelude.filter ((>=200) . head . view _3) >-> PPrelude.tee (count counterL200)
           --  >-> PPrelude.filter ((>=300) . head . view _3) >-> PPrelude.tee (count counterL300)

-- count :: Simple Lens s Int -> Consumer a (StateT s IO) ()
count :: CutChoice -> Simple Lens CounterState1 Int -> Consumer a (StateT (M.Map CutChoice CounterState1) IO) ()
count c l = forever $ do await 
                         m <- lift get
                         let mx = M.lookup c m
                         case mx of
                           Nothing -> error "no map"
                           Just x -> do
                             let !y = x `seq` view l x
                                 !x' = set l (y+1) x 
                                 newm = M.update (const (Just x')) c m
                             lift (put newm)

ptlcut :: Double -> PhyEventNoTauNoBJet -> Bool
ptlcut cutv ev = let lst = view leptons ev
                     f x = abs (eta x) < 1.5 && pt x > cutv
                 in  any f lst


-- etalcut :: Double -> PhyEventNoTauNoBJet -> Bool
-- etalcut ev = let etalst = leptonetas ev
--              in (not . null . filter (\x -> abs x < 1.5)) etalst


filterEtaRange :: PhyEventNoTauNoBJet -> PhyEventNoTauNoBJet
filterEtaRange = over jets (filter (\x -> abs (eta (JO_Jet x)) < 2.5)) 

jetpts :: PhyEventNoTauNoBJet -> [Double]
jetpts = map (pt . JO_Jet) . view jets

-- jetetas = map (eta . JO_Jet) . view jets


checkj :: (Double,Double,Double) -> PhyEventNoTauNoBJet -> (Bool,Bool,Bool)
checkj (j1,j234,j56) ev = 
    let ptlst = jetpts ev
    in case ptlst of
        [] -> (False,False,False)
        x:xs -> 
          let b1 = x > j1
          in case xs of
               _:_:y:ys -> let b2 = y > j234
                           in case ys of 
                                _:z:_zs -> let b3 = z > j56
                                           in (b1,b2,b3)
                                _ -> (b1,b2,False)
               _ -> (b1,False,False)


numOfB :: Double -> PhyEventNoTau -> Int
numOfB j56 ev = let ptlst = (filter (> j56) . map pt . view bjets) ev
                in length ptlst 

leptonetas :: PhyEventNoTauNoBJet -> [Double]
leptonetas = map eta . view leptons

htcut :: Double -> PhyEventNoTauNoBJet -> Bool 
htcut v ev = let ptlst = jetpts ev
                 ht6 = sum (take 6 ptlst)
             in ht6 > v


countmark :: (MonadIO m) => Int -> Int -> Pipe a a m r
countmark marker n = go n
  where go i = do when (i `mod` marker == 0) $ do 
                    liftIO (putStrLn (show i ++ "th event"))
                  x <- await
                  yield x
                  go (i+1) 



format :: CutChoice -> CounterState1 -> String
format CutChoice {..} cs =
    printf "%6.1f %6.1f %6.1f %6.1f %6.1f %7d %7d %7d %7d %7d %7d %7d %7d %7d" choice_ht choice_ptj1 choice_ptj234 choice_ptj56 choice_ptl full c1 c2 c3 le ht b1 b2 b3
  where full = view counterFull cs
        c1   = view counterPass1 cs
        c2   = view counterPass2 cs
        c3   = view counterPass3 cs
        le   = view counterPass4 cs
        ht   = view counterPass5 cs
        b1   = view counterBjet1 cs
        b2   = view counterBjet2 cs
        b3   = view counterBjet3 cs
        -- l20  = view counterL20 cs
        -- l50  = view counterL50 cs
        -- l100 = view counterL100 cs
        -- l200 = view counterL200 cs
        -- l300 = view counterL300 cs

{-
main' :: IO ()
main'  = do
    str <- getLine 
    let args0 : args1 : [] = words str 
        m = read args0
        n = read args1
    let filename = "optcutlepttbar_" ++ show m ++ "_" ++ show n ++ ".dat"
    withFile filename WriteMode $ \h -> do
      F.forM_ ((take (n-m+1) . drop (m-1)) testsets) $ \x -> do
        putStrLn $ "generating result for cut choice = " ++ show x
        r <- (x,) <$> work fourtopsimpl1000 x
        let str = uncurry format r
        hPutStrLn h str
-}

        -- ttbarset

main :: IO ()
main = do
    -- let filename = "optcutlepttbar_" ++ show m ++ "_" ++ show n ++ ".dat"
    ($ stdout) $ \h -> do
        -- putStrLn $ "generating result for cut choice = " ++ show x
        -- r <- (x,) <$> 
        r <- work fourtopsimpl1000 testsets -- [testset1,testset2]
        -- let str = uncurry format r
        mapM_ (hPutStrLn h . uncurry format) (M.toAscList r)
  
