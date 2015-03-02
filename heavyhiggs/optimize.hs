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
import           Control.Lens.Prism (_Just)
import           Control.Lens.TH
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Maybe
import           Control.Monad.Trans.State.Strict
import qualified Data.Attoparsec.Text as PA
import qualified Data.ByteString.Lazy.Char8 as LB
import           Data.Default       
import qualified Data.Foldable as F
import           Data.List (foldl',sortBy,sort, intercalate,(\\))
import           Data.List.Split
import           Data.Maybe (catMaybes,fromJust,isJust)
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
import HEP.Automation.EventGeneration.Config
import qualified HEP.Automation.EventGeneration.Work as EV
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
--
import qualified HeavyHiggs
import qualified SM

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



checkAndDownload :: WebDAVConfig -> WebDAVRemoteDir -> FilePath -> MaybeT IO ()
checkAndDownload cfg rdir fpath = do
    b <- liftIO $ doesFileExistInDAV cfg rdir fpath
    guard b
    liftIO $ downloadFile False cfg rdir fpath 
    liftIO $ putStrLn $ fpath ++ " is successfully downloaded"

{- 
fourtopsimpl400 = map (\x->"data/fourtopsimpl_400_set" ++ x ++  "_pgs_events.lhco.gz") 
                      [ "1", "2", "3", "4", "5", "6", "7", "8", "9", "10" ]

fourtopsimpl750 = map (\x->"data/fourtopsimpl_750_set" ++ x ++  "_pgs_events.lhco.gz") 
                      [ "1", "2", "3", "4", "5", "6", "7", "8", "9", "10" ]

fourtopsimpl1000 = map (\x->"data/fourtopsimpl_1000_set" ++ x ++  "_pgs_events.lhco.gz") 
                       [ "1", "2", "3", "4", "5", "6", "7", "8", "9", "10" ]


ttbarset = map (\x->"data/" ++ x ++ "_pgs_events.lhco.gz") . map (\x -> makeRunName SM.psetup SMParam (SM.rsetupgen x)) $ [1..1000] 
-}

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
--            j1       j234     j56      ptl      ht
type CCO = [(Double,[(Double,[(Double,[(Double,[Double])])])])]

testcco :: CCO
testcco = [testcco1,testcco2]

testcco1 = (100,[(50,[(25,[(0,[500])])])])
testcco2 = (200,[(50,[(25,[(0,[500])])])])


testht = [500,800,1000,1200,1500]
testl = map (\x->(x,testht)) [0,20,50,100,200,300]
testj56 j234 = map (\x->(x,testl)) [25,50..j234]
testj234 j1 = map (\x->(x,testj56 x)) [50,100..j1]
testj1 = map (\x->(x,testj234 x)) [100,200,300]

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
                          } deriving (Show,Eq,Ord)

emptyCS1 = CounterState1 0 0 0 0 0 0 0 0 0 

makeLenses ''CounterState1
                               
-- emptyCS :: [CutChoice] -> M.Map CutChoice CounterState1
-- emptyCS lst = foldr (\k -> M.insert k emptyCS1) M.empty lst

-- CutChoice {..}
data TotalCS = TotalCS { _fullNum :: Int
                       , _cut1Map :: M.Map Double Int
                       , _cut2Map :: M.Map (Double,Double) Int
                       , _cut3Map :: M.Map (Double,Double,Double) Int
                       , _cut4Map :: M.Map (Double,Double,Double,Double) Int
                       , _cut5Map :: M.Map (Double,Double,Double,Double,Double) Int
                       , _b1Map :: M.Map (Double,Double,Double,Double,Double) Int
                       , _b2Map :: M.Map (Double,Double,Double,Double,Double) Int
                       , _b3Map :: M.Map (Double,Double,Double,Double,Double) Int
                       , _cutStateMap :: M.Map CutChoice CounterState1 
                       }
             deriving (Show,Eq,Ord)

makeLenses ''TotalCS

emptyTCS :: CCO -> TotalCS
emptyTCS ccos = TotalCS 0 m1 m2 m3 m4 m5 mb1 mb2 mb3 M.empty -- emptyCS 
  where j1j234 = do
          cco <- ccos
          let j1 = fst cco
          j234s <- snd cco
          let j234 = fst j234s
          return (j1,j234)
        j1j234j56 = do
          cco <- ccos
          let j1 = fst cco
          j234s <- snd cco
          let j234 = fst j234s
          j56s <- snd j234s
          let j56 = fst j56s
          return (j1,j234,j56)
        j1j234j56l = do
          cco <- ccos
          let j1 = fst cco
          j234s <- snd cco
          let j234 = fst j234s
          j56s <- snd j234s
          let j56 = fst j56s
          ls <- snd j56s
          let l = fst ls
          return (j1,j234,j56,l)
        j1j234j56lh = do
          cco <- ccos
          let j1 = fst cco
          j234s <- snd cco
          let j234 = fst j234s
          j56s <- snd j234s
          let j56 = fst j56s
          ls <- snd j56s
          let l = fst ls
          h <- snd ls
          return (j1,j234,j56,l,h)



        m1 = foldr (\k->M.insert k 0) M.empty (map fst ccos)
        m2 = foldr (\k->M.insert k 0) M.empty j1j234
        m3 = foldr (\k->M.insert k 0) M.empty j1j234j56
        m4 = foldr (\k->M.insert k 0) M.empty j1j234j56l
        m5 = foldr (\k->M.insert k 0) M.empty j1j234j56lh
        mb1 = m5
        mb2 = m5
        mb3 = m5 

   
mkChoices :: CCO -> [CutChoice] 
mkChoices ccos  = [CutChoice h j1 j234 j56 l | (j1,j234,j56,l,h) <- j1j234j56lh ]
  where j1j234j56lh = do
          cco <- ccos
          let j1 = fst cco
          j234s <- snd cco
          let j234 = fst j234s
          j56s <- snd j234s
          let j56 = fst j56s
          ls <- snd j56s
          let l = fst ls
          h <- snd ls
          return (j1,j234,j56,l,h)


work :: [FilePath] -> CCO -> IO TotalCS -- (M.Map CutChoice CounterState1)
work fpaths ccos = do
    hins <- mapM (\fpath -> openFile fpath ReadMode) fpaths  
    (_,r) <- flip runStateT (emptyTCS ccos) $ runEffect $ do
           F.forM_ fpaths $ \fpath -> do 
             hin <- liftIO $ openFile fpath ReadMode
             pipesLHCOEvent (gunzip hin >-> PPrelude.map T.decodeUtf8)
             liftIO $ hClose hin 
           -- >-> PPrelude.take 10000
           >-> PPrelude.tee (countmark 1000 0 >-> PPrelude.drain)
           >-> PPrelude.map (((,,) <$> filterEtaRange . mergeBJetFromNoTauEv <*> id <*> id) . mkPhyEventNoTau)
           >-> analysisL0 ccos
           >-> PPrelude.drain
    return r
  where 
    analysisL0 lst = 
       PPrelude.tee (count fullNum >-> PPrelude.drain)
       >-> foldr1 (>->) (map (PPrelude.tee . analysisL1) lst)
    analysisL1 (j1,j234s) = 
      PPrelude.map (\ev->(ev, (checkj1 j1 . view _1) ev))
      >-> PPrelude.filter (isJust.view _2) >-> PPrelude.tee (countM1 (cut1Map.at j1))
      >-> PPrelude.map (\(ev,mx)->(ev, fromJust mx))
      >-> foldr1 (>->) (map (PPrelude.tee . analysisL2 j1) j234s)
      >-> PPrelude.drain 
    analysisL2 j1 (j234,j56s) = 
      PPrelude.map (\(ev,r)->(ev,checkj234 j234 r))
      >-> PPrelude.filter (isJust.view _2) >-> PPrelude.tee (countM1 (cut2Map.at (j1,j234)))
      >-> PPrelude.map (\(ev,mx)->(ev, fromJust mx))
      >-> foldr1 (>->) (map (PPrelude.tee . analysisL3 j1 j234) j56s)
      >-> PPrelude.drain
    analysisL3 j1 j234 (j56,ls) = 
      PPrelude.map (\(ev,r)->(ev,checkj56 j56 r))
      >-> PPrelude.filter (isJust.view _2) >-> PPrelude.tee (countM1 (cut3Map.at (j1,j234,j56)))
      >-> foldr1 (>->) (map (PPrelude.tee . analysisL4 j1 j234 j56) ls)
      >-> PPrelude.drain
    analysisL4 j1 j234 j56 (l,hs) = 
      PPrelude.map (\(ev,r)->(ev,(ptlcut l . view _1) ev))
      >-> PPrelude.filter (view _2) >-> PPrelude.tee (countM1 (cut4Map.at (j1,j234,j56,l)))
      >-> foldr1 (>->) (map (PPrelude.tee . analysisL5 j1 j234 j56 l) hs)
      >-> PPrelude.drain
    analysisL5 j1 j234 j56 l h = 
      PPrelude.map (\(ev,r)->(ev,(htcut h . view _1) ev))
      >-> PPrelude.filter (view _2) >-> PPrelude.tee (countM1 (cut5Map.at (j1,j234,j56,l,h)))
      -- >-> foldr1 (>->) (map (PPrelude.tee . analysisL5 j1 j234 j56 l) hs)
      >-> PPrelude.map (\(ev,r)->(ev, numOfB j56 (view _2 ev)))
      >-> PPrelude.filter ((>=1) . view (_2)) >-> PPrelude.tee (countM1 (b1Map.at (j1,j234,j56,l,h)))
      >-> PPrelude.filter ((>=2) . view (_2)) >-> PPrelude.tee (countM1 (b2Map.at (j1,j234,j56,l,h)))
      >-> PPrelude.filter ((>=3) . view (_2)) >-> PPrelude.tee (countM1 (b3Map.at (j1,j234,j56,l,h)))
      >-> PPrelude.drain
     
    

count :: Simple Lens TotalCS Int -> Consumer a (StateT TotalCS IO) ()
count l = forever $ do await 
                       m <- lift get
                       let !y = m `seq` view l m
                           !m' = set l (y+1) m 
                       lift (put m')


countm :: CutChoice -> Simple Lens CounterState1 Int -> Consumer a (StateT TotalCS IO) ()
countm c l = forever $ do 
               await 
               tcs <- lift get
               let m = view cutStateMap tcs 
               let mx = M.lookup c m
               case mx of
                 Nothing -> error "no map"
                 Just x -> do
                   let !y = m `seq` view l x
                   let !x' = set l (y+1) x 
                       m' = M.update (const (Just x')) c m
                   lift (put (set cutStateMap m' tcs))

countM1 :: Simple Lens TotalCS (Maybe Int) -> Consumer a (StateT TotalCS IO) ()
countM1 l = forever $ do 
                await 
                tcs <- lift get
                let mx = view l tcs 
                case mx of
                  Nothing -> error "no map"
                  Just x -> do
                    let !tcs' = set l (Just (x+1)) tcs 
                    lift (put tcs')

ptlcut :: Double -> PhyEventNoTauNoBJet -> Bool
ptlcut cutv ev = let lst = view leptons ev
                     f x = abs (eta x) < 1.5 && pt x > cutv
                 in  any f lst

filterEtaRange :: PhyEventNoTauNoBJet -> PhyEventNoTauNoBJet
filterEtaRange = over jets (filter (\x -> abs (eta (JO_Jet x)) < 2.5)) 

jetpts :: PhyEventNoTauNoBJet -> [Double]
jetpts = map (pt . JO_Jet) . view jets

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


checkj1 :: Double -> PhyEventNoTauNoBJet -> Maybe (Double,[Double])
checkj1 j1 ev = 
    let ptlst = jetpts ev
    in case ptlst of
        [] -> Nothing
        x:xs -> if x > j1 then Just (x,xs) else Nothing

checkj234 :: Double -> (Double,[Double]) -> Maybe (Double,Double,Double,Double,[Double])
checkj234 j234 (x1,ptlst) = 
    case ptlst of
      x2:x3:x4:xs -> if x4 > j234 then Just (x1,x2,x3,x4,xs) else Nothing
      _ -> Nothing


checkj56 :: Double -> (Double,Double,Double,Double,[Double]) -> Maybe (Double,Double,Double,Double,Double,Double,[Double])
checkj56 j56 (x1,x2,x3,x4,ptlst) = 
    case ptlst of
      x5:x6:xs -> if x6 > j56 then Just (x1,x2,x3,x4,x5,x6,xs) else Nothing
      _ -> Nothing


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



format :: CutChoice -> TotalCS -> String
format cut@CutChoice {..} tcs =
    printf "%6.1f %6.1f %6.1f %6.1f %6.1f %7d %7d %7d %7d %7d %7d %7d %7d %7d" choice_ht choice_ptj1 choice_ptj234 choice_ptj56 choice_ptl full c1 c2 c3 le ht b1 b2 b3
  where Just cs = view (cutStateMap.at cut) tcs
        full = view fullNum tcs
        Just c1 = view (cut1Map.at choice_ptj1) tcs
        Just c2 = view (cut2Map.at (choice_ptj1,choice_ptj234)) tcs
        Just c3 = view (cut3Map.at (choice_ptj1,choice_ptj234,choice_ptj56)) tcs
        Just le = view (cut4Map.at (choice_ptj1,choice_ptj234,choice_ptj56,choice_ptl)) tcs
        Just ht = view (cut5Map.at (choice_ptj1,choice_ptj234,choice_ptj56,choice_ptl,choice_ht)) tcs
        Just b1 = view (b1Map.at (choice_ptj1,choice_ptj234,choice_ptj56,choice_ptl,choice_ht)) tcs
        Just b2 = view (b2Map.at (choice_ptj1,choice_ptj234,choice_ptj56,choice_ptl,choice_ht)) tcs
        Just b3 = view (b3Map.at (choice_ptj1,choice_ptj234,choice_ptj56,choice_ptl,choice_ht)) tcs



main' :: IO ()
main' = do
    args <- getArgs
    let massparam :: Double = read (args !! 0)
    --  let massparam = 400 
        set1 = map (massparam,) [1..10]
         
    ws1 <- HeavyHiggs.getWSetup (massparam,1)
  
    let rname = makeRunName (ws_psetup ws1) (ws_param ws1) (ws_rsetup ws1)
        rname' = (intercalate "_" . init . splitOn "_") rname

    let filename = rname' ++ "_cut_count.dat"
    withFile filename WriteMode $ \h -> do
        sets <- mapM (prepare HeavyHiggs.getWSetup) set1
        tcs <- work sets testj1
        mapM_ (hPutStrLn h . flip format tcs) (sort (mkChoices testj1)) 



main :: IO ()
main = do
    --  let massparam = 400 
    let set1 = [9001..10000] \\ [9005,9036,9045,9086,9087,9090,9108,9131,9133,9148,9188,9212,9218,9264,9265,9312,9328,9357,9428,9458,9472,9502,9535,9553,9583,9588,9598,9616,9630,9638,9677,9696,9706,9718,9751,9753,9781,9804]
                -- [8001..9000] \\ [8052,8073,8083,8090,8103,8167,8186,8260,8272,8300,8313,8319,8325,8332,8341,8361,8378,8408,8503,8505,8543,8548,8562,8574,8594,8634,8679,8683,8770,8789,8807,8811,8821,8863,8874,8886,8949,8954,8967]
                -- [7001..8000] \\ [7001,7016,7031,7094,7097,7121,7279,7307,7343,7443,7485,7495,7502,7505,7522,7533,7699,7730,7748,7832,7883,7916,7933,7945]
                -- [6001..7000] \\ [6038,6164,6206,6236,6277,6363,6399,6428,6452,6520,6536,6581,6833,6892,6895,6897,6900,6914,6919]

                -- [5001..6000] \\ [5075,5085,5129,5186,5472,5489,5510,5515,5546,5772,5802,5803,5923]
                -- [1001..2000] Data.List.\\ [1059, 1061, 1064, 1065, 1213, 1802, 1805]
               
         
    ws1 <- SM.getWSetup 1
  
    let rname = makeRunName (ws_psetup ws1) (ws_param ws1) (ws_rsetup ws1)
        rname' = (intercalate "_" . init . splitOn "_") rname

    let filename = rname' ++ "_set9001to10000_cut_count.dat"
    withFile filename WriteMode $ \h -> do
        sets <- mapM (prepare SM.getWSetup) set1
        tcs <- work sets testj1
        mapM_ (hPutStrLn h . flip format tcs) (sort (mkChoices testj1)) 


prepare :: (Model b) => (a -> IO (WorkSetup b)) -> a -> IO FilePath
prepare getwsetup x = do
  let pkey = "/home/wavewave/temp/priv.txt"
      pswd = "/home/wavewave/temp/cred.txt"
  Just cr <- getCredential pkey pswd
  let whost = "http://top.physics.lsa.umich.edu:10080/webdav/"
      wdavcfg = WebDAVConfig cr whost
  ws <- getwsetup x
  -- EV.download wdavcfg ws "_pgs_events.lhco.gz" 
 
  let rname = makeRunName (ws_psetup ws) (ws_param ws) (ws_rsetup ws)
      fname = rname ++ "_pgs_events.lhco.gz"
 
  return fname
