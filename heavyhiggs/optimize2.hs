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
import           Data.IORef
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
import           System.Directory
import           System.Environment
import           System.FilePath
import           System.IO as IO
import           Text.Printf
-- 
import HEP.Automation.EventGeneration.Config
import qualified HEP.Automation.EventGeneration.Work as EV
import HEP.Automation.MadGraph.Model
-- import HEP.Automation.MadGraph.Model.SM
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
-- import qualified HeavyHiggs4T
import qualified HeavyHiggs2T2BInnerTop
-- import qualified HeavyHiggs2T2BOuterTop
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

data CutChoice = CutChoice { choice_ptj1  :: Double
                           , choice_ptj2  :: Double
                           , choice_ptj34 :: Double
                           , choice_ptl   :: Double
                           , choice_ht    :: Double
                           , choice_deta  :: Double
                           }

               deriving (Show,Eq,Ord)
--            j1       j2       j34      ptl      hT      deta
type CCO = [(Double,[(Double,[(Double,[(Double,[(Double,[Double])])])])])]


-- testht     = [500,1000,1500] -- [400,500..800]

testdeta   = [0, 1.5] -- , 3.0, 4.5] 
testh      = map (\x->(x,testdeta)) [100,200..1000] -- [400,600,800] -- [500,1000,1500]
testl      = map (\x->(x,testh)) [0,10..40]
testj34 j2 = map (\x->(x,testl)) [20,40..if j2 > 100 then 100 else j2]
testj2  j1 = map (\x->(x,testj34 x)) [20,40..if j1 > 100 then 100 else j1]
testj1     = map (\x->(x,testj2 x)) [50,100..300]

data CounterState1 = 
       CounterState1      { _counterFull :: !Int
                          , _counterPass1 :: !Int
                          , _counterPass2 :: !Int
                          , _counterPass3 :: !Int
                          , _counterPass4 :: !Int
                          , _counterPass5 :: !Int
                          , _counterPass6 :: !Int
                          , _counterBjet1 :: !Int
                          , _counterBjet2 :: !Int
                          , _counterBjet3 :: !Int
                          } deriving (Show,Eq,Ord)

emptyCS1 = CounterState1 0 0 0 0 0 0 0 0 0 

makeLenses ''CounterState1
                               
data TotalCS = TotalCS { _fullNum :: Int
                       , _cut1Map :: M.Map Double Int
                       , _cut2Map :: M.Map (Double,Double) Int
                       , _cut3Map :: M.Map (Double,Double,Double) Int
                       , _cut4Map :: M.Map (Double,Double,Double,Double) Int
                       , _cut5Map :: M.Map (Double,Double,Double,Double,Double) Int
                       , _cut6Map :: M.Map (Double,Double,Double,Double,Double,Double) Int
                       , _b1Map :: M.Map (Double,Double,Double,Double,Double,Double) Int
                       , _b2Map :: M.Map (Double,Double,Double,Double,Double,Double) Int
                       , _b3Map :: M.Map (Double,Double,Double,Double,Double,Double) Int
                       , _cutStateMap :: M.Map CutChoice CounterState1 
                       }
             deriving (Show,Eq,Ord)

makeLenses ''TotalCS

emptyTCS :: CCO -> TotalCS
emptyTCS ccos = TotalCS 0 m1 m2 m3 m4 m5 m6 mb1 mb2 mb3 M.empty
  where j1j2 = do
          cco <- ccos
          let j1 = fst cco
          j2s <- snd cco
          let j2 = fst j2s
          return (j1,j2)
        j1j2j34 = do
          cco <- ccos
          let j1 = fst cco
          j2s <- snd cco
          let j2 = fst j2s
          j34s <- snd j2s
          let j34 = fst j34s
          return (j1,j2,j34)
        j1j2j34l = do
          cco <- ccos
          let j1 = fst cco
          j2s <- snd cco
          let j2 = fst j2s
          j34s <- snd j2s
          let j34 = fst j34s
          ls <- snd j34s
          let l = fst ls           
          return (j1,j2,j34,l)
        j1j2j34lh = do
          cco <- ccos
          let j1 = fst cco
          j2s <- snd cco
          let j2 = fst j2s
          j34s <- snd j2s
          let j34 = fst j34s
          ls <- snd j34s
          let l = fst ls           
          hs <- snd ls
          let h = fst hs
          return (j1,j2,j34,l,h)
        j1j2j34lhdeta = do
          cco <- ccos
          let j1 = fst cco
          j2s <- snd cco
          let j2 = fst j2s
          j34s <- snd j2s
          let j34 = fst j34s
          ls <- snd j34s
          let l = fst ls           
          hs <- snd ls
          let h = fst hs
          deta <- snd hs
          return (j1,j2,j34,l,h,deta)


        m1 = foldr (\k->M.insert k 0) M.empty (map fst ccos)
        m2 = foldr (\k->M.insert k 0) M.empty j1j2
        m3 = foldr (\k->M.insert k 0) M.empty j1j2j34
        m4 = foldr (\k->M.insert k 0) M.empty j1j2j34l
        m5 = foldr (\k->M.insert k 0) M.empty j1j2j34lh
        m6 = foldr (\k->M.insert k 0) M.empty j1j2j34lhdeta
        mb1 = m6
        mb2 = m6
        mb3 = m6 

   
mkChoices :: CCO -> [CutChoice] 
mkChoices ccos  = [CutChoice j1 j2 j34 l h deta | (j1,j2,j34,l,h,deta) <- j1j2j34ldeta ]
  where j1j2j34ldeta = do
          cco <- ccos
          let j1 = fst cco
          j2s <- snd cco
          let j2 = fst j2s
          j34s <- snd j2s
          let j34 = fst j34s
          ls <- snd j34s
          let l = fst ls           
          hs <- snd ls
          let h = fst hs
          deta <- snd hs
          return (j1,j2,j34,l,h,deta)

work :: [FilePath] -> CCO -> IO TotalCS
work fpaths ccos = do
    hins <- mapM (\fpath -> openFile fpath ReadMode) fpaths  
    r <- 

      do let tcs = emptyTCS ccos
              {- flip runStateT (emptyTCS ccos) $ -} 
         tcsref <- newIORef tcs 
         runEffect $ do
           
           F.forM_ fpaths $ \fpath -> do 
             hin <- liftIO $ openFile fpath ReadMode
             pipesLHCOEvent (gunzip hin >-> PPrelude.map T.decodeUtf8)
             liftIO $ hClose hin 
           -- >-> PPrelude.take 100
           >-> PPrelude.tee (countmark 100 0 >-> PPrelude.drain)
           >-> PPrelude.map (((,,) <$> filterEtaRange . mergeBJetFromNoTauEv <*> id <*> id) . mkPhyEventNoTau)
           >-> analysisL0 tcsref ccos
           >-> PPrelude.drain
         readIORef tcsref
    return r
  where 
    analysisL0 ref lst = 
       PPrelude.tee (count ref fullNum >-> PPrelude.drain)
       >-> foldr1 (>->) (map (PPrelude.tee . analysisL1 ref) lst)
    analysisL1 ref (j1,j2s) = 
      PPrelude.map (\ev->(ev, (checkj1 j1 . view _1) ev))
      >-> PPrelude.filter (isJust.view _2) >-> PPrelude.tee (countM1 ref (cut1Map.at j1))
      >-> PPrelude.map (\(ev,mx)->(ev, fromJust mx))
      >-> foldr1 (>->) (map (PPrelude.tee . analysisL2 ref j1) j2s)
      >-> PPrelude.drain 
    analysisL2 ref j1 (j2,j34s) = 
      PPrelude.map (\(ev,r)->(ev,checkj2 j2 r))
      >-> PPrelude.filter (isJust.view _2) >-> PPrelude.tee (countM1 ref (cut2Map.at (j1,j2)))
      >-> PPrelude.map (\(ev,mx)->(ev, fromJust mx))
      >-> foldr1 (>->) (map (PPrelude.tee . analysisL3 ref j1 j2) j34s)
      >-> PPrelude.drain
    analysisL3 ref j1 j2 (j34,ls) = 
      PPrelude.map (\(ev,r)->(ev,checkj34 j34 r))
      >-> PPrelude.filter (isJust.view _2) >-> PPrelude.tee (countM1 ref (cut3Map.at (j1,j2,j34)))
      >-> foldr1 (>->) (map (PPrelude.tee . analysisL4 ref j1 j2 j34) ls)
      >-> PPrelude.drain
    analysisL4 ref j1 j2 j34 (l,hs) = 
      PPrelude.map (\(ev,r)->(ev,(ptlcut l . view _1) ev))
      >-> PPrelude.filter (view _2) >-> PPrelude.tee (countM1 ref (cut4Map.at (j1,j2,j34,l)))
      >-> foldr1 (>->) (map (PPrelude.tee . analysisL5 ref j1 j2 j34 l) hs)
      >-> PPrelude.drain
    analysisL5 ref j1 j2 j34 l (h,detas) =  
      PPrelude.map (\(ev,r)->(ev,(htcut h . view _1) ev))
      >-> PPrelude.filter (view _2) >-> PPrelude.tee (countM1 ref (cut5Map.at (j1,j2,j34,l,h)))
      >-> foldr1 (>->) (map (PPrelude.tee . analysisL6 ref j1 j2 j34 l h) detas)
      >-> PPrelude.drain
    analysisL6 ref j1 j2 j34 l h deta =  
      PPrelude.map (\(ev,r)->(ev,(detacut deta . view _1) ev))
      >-> PPrelude.filter (view _2) >-> PPrelude.tee (countM1 ref (cut6Map.at (j1,j2,j34,l,h,deta)))
      >-> PPrelude.map (\(ev,r)->(ev, numOfB (view _2 ev)))
      >-> PPrelude.filter ((>=1) . view (_2)) >-> PPrelude.tee (countM1 ref (b1Map.at (j1,j2,j34,l,h,deta)))
      >-> PPrelude.filter ((>=2) . view (_2)) >-> PPrelude.tee (countM1 ref (b2Map.at (j1,j2,j34,l,h,deta)))
      >-> PPrelude.filter ((>=3) . view (_2)) >-> PPrelude.tee (countM1 ref (b3Map.at (j1,j2,j34,l,h,deta)))
      >-> PPrelude.drain
     
    

count :: IORef TotalCS -> Simple Lens TotalCS Int -> Consumer a IO ()
count ref l = forever $ do 
                await 
                m <- lift (readIORef ref)
                let !y = view l m
                    !m' = set l (y+1) m 
                lift (writeIORef ref m') 


countM1 :: IORef TotalCS -> Simple Lens TotalCS (Maybe Int) -> Consumer a IO ()
countM1 ref l = forever $ do 
                  await 
                  tcs <- lift (readIORef ref)
                  let !mx = view l tcs 
                  case mx of
                    Nothing -> error "no map"
                    Just !x -> do
                      let !tcs' = set l (Just (x+1)) tcs 
                      lift (writeIORef ref tcs')

ptlcut :: Double -> PhyEventNoTauNoBJet -> Bool
ptlcut cutv ev = let lst = view leptons ev
                     f x = abs (eta x) < 2.5 && pt x > cutv
                 in  any f lst

filterEtaRange :: PhyEventNoTauNoBJet -> PhyEventNoTauNoBJet
filterEtaRange = over jets (filter (\x -> abs (eta (JO_Jet x)) < 3)) 

jetpts :: PhyEventNoTauNoBJet -> [Double]
jetpts = map (pt . JO_Jet) . view jets


checkj1 :: Double -> PhyEventNoTauNoBJet -> Maybe (Double,[Double])
checkj1 j1 ev = 
    let ptlst = jetpts ev
    in case ptlst of
        [] -> Nothing
        x:xs -> if x > j1 then Just (x,xs) else Nothing

checkj2 :: Double -> (Double,[Double]) -> Maybe (Double,Double,[Double])
checkj2 j2 (x1,ptlst) = 
    case ptlst of
      x2:xs -> if x2 > j2 then Just (x1,x2,xs) else Nothing
      _ -> Nothing


checkj34 :: Double -> (Double,Double,[Double]) 
         -> Maybe (Double,Double,Double,Double,[Double])
checkj34 j34 (x1,x2,ptlst) =
    case ptlst of
      x3:x4:xs -> if x4 > j34 then Just (x1,x2,x3,x4,xs) else Nothing
      _ -> Nothing


numOfB :: PhyEventNoTau -> Int
numOfB ev = let ptlst = (filter (> 20) . map pt . view bjets) ev
            in length ptlst 

leptonetas :: PhyEventNoTauNoBJet -> [Double]
leptonetas = map eta . view leptons


deltaeta :: PhyEventNoTauNoBJet -> Double
deltaeta e = 
    let es = (map eta . filter (\x -> pt x > 20) . map JO_Jet . view jets) e
    in (maximum es - minimum es)


detacut :: Double -> PhyEventNoTauNoBJet -> Bool
detacut c e = let deta = deltaeta e
              in deta > c



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
    printf "%6.1f %6.1f %6.1f %6.1f %6.1f %6.1f   %7d %7d %7d %7d %7d %7d %7d %7d %7d %7d" choice_ptj1 choice_ptj2 choice_ptj34 choice_ptl choice_ht choice_deta full c1 c2 c3 le ht deta b1 b2 b3
  where Just cs = view (cutStateMap.at cut) tcs
        full = view fullNum tcs
        Just c1 = view (cut1Map.at choice_ptj1) tcs
        Just c2 = view (cut2Map.at (choice_ptj1,choice_ptj2)) tcs
        Just c3 = view (cut3Map.at (choice_ptj1,choice_ptj2,choice_ptj34)) tcs
        Just le = view (cut4Map.at (choice_ptj1,choice_ptj2,choice_ptj34,choice_ptl)) tcs
        Just ht = view (cut5Map.at (choice_ptj1,choice_ptj2,choice_ptj34,choice_ptl,choice_ht)) tcs
        Just deta = view (cut6Map.at (choice_ptj1,choice_ptj2,choice_ptj34,choice_ptl,choice_ht,choice_deta)) tcs

        Just b1 = view (b1Map.at (choice_ptj1,choice_ptj2,choice_ptj34,choice_ptl,choice_ht,choice_deta)) tcs
        Just b2 = view (b2Map.at (choice_ptj1,choice_ptj2,choice_ptj34,choice_ptl,choice_ht,choice_deta)) tcs
        Just b3 = view (b3Map.at (choice_ptj1,choice_ptj2,choice_ptj34,choice_ptl,choice_ht,choice_deta)) tcs



main = do
  mapM_ signal [450,550..950] 

signal :: Double -> IO ()
signal massparam = do
    args <- getArgs
    let -- massparam :: Double = read (args !! 0)
        set1 = map (massparam,) [1..10]
         
    ws1 <- HeavyHiggs2T2BInnerTop.getWSetup (massparam,1)
           -- HeavyHiggs4T.getWSetup (massparam,1)
    let rname = makeRunName (ws_psetup ws1) (ws_param ws1) (ws_rsetup ws1)
        rname' = (intercalate "_" . init . splitOn "_") rname

    let filename = resultdir </> rname' ++ "_cut_count_wdeta_add1.dat"
    withFile filename WriteMode $ \h -> do
      sets <- map (eventdir </>) <$> mapM (prepare True HeavyHiggs2T2BInnerTop.getWSetup) set1
      tcs <- work sets testj1
      hPutStrLn h "====================================================================================================================================="
      hPutStrLn h " pt_j1     j2    j34   pt_l   H_T    deta  ||  full   cutj1   cutj2  cutj34    cutl   cutHT cutdeta   bjet1   bjet2   bjet3   "
      hPutStrLn h "-------------------------------------------------------------------------------------------------------------------------------------"
      mapM_ (hPutStrLn h . flip format tcs) (mkChoices testj1) -- (sort (mkChoices testj1)) 



eventdir = "/home/wavewave/temp/heavyhiggs"

resultdir = "/home/wavewave/repo/src/lhc-analysis-collection/heavyhiggs/result"


smset = [ (   1,1000,[1..1000])
        , (1001,2000,[1001..2000] Data.List.\\ [1059, 1061, 1064, 1065, 1213, 1802, 1805])
        , (2001,3000,[2001..3000])
        , (3001,4000,[3001..4000])
        , (4001,5000,[4001..5000])
        , (5001,6000,[5001..6000] \\ [5075,5085,5129,5186,5472,5489,5510,5515,5546,5772,5802,5803,5923])
        , (6001,7000,[6001..7000] \\ [6038,6164,6206,6236,6277,6363,6399,6428,6452,6520,6536,6581,6833,6892,6895,6897,6900,6914,6919])
        , (7001,8000,[7001..8000] \\ [7001,7016,7031,7094,7097,7121,7279,7307,7343,7443,7485,7495,7502,7505,7522,7533,7699,7730,7748,7832,7883,7916,7933,7945])
        , (8001,9000,[8001..9000] \\ [8052,8073,8083,8090,8103,8167,8186,8260,8272,8300,8313,8319,8325,8332,8341,8361,8378,8408,8503,8505,8543,8548,8562,8574,8594,8634,8679,8683,8770,8789,8807,8811,8821,8863,8874,8886,8949,8954,8967])
        , (9001,10000,[9001..10000] \\ [9005,9036,9045,9086,9087,9090,9108,9131,9133,9148,9188,9212,9218,9264,9265,9312,9328,9357,9428,9458,9472,9502,9535,9553,9583,9588,9598,9616,9630,9638,9677,9696,9706,9718,9751,9753,9781,9804])
        ]

main' = mapM_ background smset
 
background :: (Int,Int,[Int]) -> IO ()
background (s,e,set1) = do
    args <- getArgs
    {- let set1 = -- [1..1000]
               -- [1001..2000] Data.List.\\ [1059, 1061, 1064, 1065, 1213, 1802, 1805]
               -- [2001..3000]
               -- [3001..4000]
               -- [4001..5000]
               -- [5001..6000] \\ [5075,5085,5129,5186,5472,5489,5510,5515,5546,5772,5802,5803,5923]
               -- [6001..7000] \\ [6038,6164,6206,6236,6277,6363,6399,6428,6452,6520,6536,6581,6833,6892,6895,6897,6900,6914,6919]
               -- [7001..8000] \\ [7001,7016,7031,7094,7097,7121,7279,7307,7343,7443,7485,7495,7502,7505,7522,7533,7699,7730,7748,7832,7883,7916,7933,7945]
               -- [8001..9000] \\ [8052,8073,8083,8090,8103,8167,8186,8260,8272,8300,8313,8319,8325,8332,8341,8361,8378,8408,8503,8505,8543,8548,8562,8574,8594,8634,8679,8683,8770,8789,8807,8811,8821,8863,8874,8886,8949,8954,8967]
         --        [9001..10000] \\ [9005,9036,9045,9086,9087,9090,9108,9131,9133,9148,9188,9212,9218,9264,9265,9312,9328,9357,9428,9458,9472,9502,9535,9553,9583,9588,9598,9616,9630,9638,9677,9696,9706,9718,9751,9753,9781,9804] -}

         
    ws1 <- SM.getWSetup 1
  
    let rname = makeRunName (ws_psetup ws1) (ws_param ws1) (ws_rsetup ws1)
        rname' = (intercalate "_" . init . splitOn "_") rname

    let filename = resultdir </> rname' ++ "_set" ++ show s ++ "to" ++ show e ++ "_cut_count_wdeta_add1.dat"
    withFile filename WriteMode $ \h -> do
      sets <- map (eventdir </>) <$> mapM (prepare False SM.getWSetup) set1
      tcs <- work sets testj1
      hPutStrLn h "====================================================================================================================================="
      hPutStrLn h " pt_j1     j2    j34   pt_l   H_T    deta  ||  full   cutj1   cutj2  cutj34    cutl   cutHT cutdeta   bjet1   bjet2   bjet3   "
      hPutStrLn h "-------------------------------------------------------------------------------------------------------------------------------------"
      mapM_ (hPutStrLn h . flip format tcs) (sort (mkChoices testj1))


prepare :: (Model b) => Bool -> (a -> IO (WorkSetup b)) -> a -> IO FilePath
prepare b getwsetup x = do
  let pkey = "/home/wavewave/temp/madgraph/priv.txt" -- "/afs/cern.ch/work/i/ikim/private/webdav/priv.txt"
      pswd = "/home/wavewave/temp/madgraph/cred.txt" -- "/afs/cern.ch/work/i/ikim/private/webdav/cred.txt"
  Just cr <- getCredential pkey pswd
  let whost = "http://top.physics.lsa.umich.edu:10080/webdav/"
      wdavcfg = WebDAVConfig cr whost
  ws <- getwsetup x
  cdir <- getCurrentDirectory
  setCurrentDirectory eventdir
  when b $ 
    EV.download wdavcfg ws "_pgs_events.lhco.gz" 
  setCurrentDirectory cdir
  let rname = makeRunName (ws_psetup ws) (ws_param ws) (ws_rsetup ws)
      fname = rname ++ "_pgs_events.lhco.gz"
 
  return fname
