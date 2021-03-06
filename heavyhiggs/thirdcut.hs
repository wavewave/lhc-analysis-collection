{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
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
import           Data.Maybe (catMaybes)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Traversable as Tr
import           Pipes
import qualified Pipes.ByteString  as PB
import qualified Pipes.Prelude as PPrelude
import qualified Pipes.Zlib as PZ
import           System.IO as IO
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


main :: IO ()
main = do
    -- let fpaths = map (++ "_pgs_events.lhco.gz") . map (\x -> makeRunName psetup param (rsetupgen x)) $ [1..1000] 
    let fpaths = ["fourtopsimpl_400_pgs_events.lhco.gz"]
    hins <- mapM (\fpath -> openFile fpath ReadMode) fpaths  
    r <- flip runStateT (0,0,0,0,0,0,0,0,0) $ runEffect $ do
           F.forM_ fpaths $ \fpath -> do 
             hin <- liftIO $ openFile fpath ReadMode
             pipesLHCOEvent (gunzip hin >-> PPrelude.map T.decodeUtf8)
             liftIO $ hClose hin 
           >-> PPrelude.tee (count _1 >-> PPrelude.drain)
           >-> PPrelude.tee (countmark 1000 0 >-> PPrelude.drain)
           >-> PPrelude.map (((,) <$> filterEtaRange . mergeBJetFromNoTauEv <*> id) . mkPhyEventNoTau)
           >-> PPrelude.map (over _2 numOfB . over _1 (\x->(checkj x, checkl x, htcut 1500 x)))
           >-> PPrelude.filter (view (_1._1._1)) >-> PPrelude.tee (count _2) -- pass1 
           >-> PPrelude.filter (view (_1._1._2)) >-> PPrelude.tee (count _3) -- pass2
           >-> PPrelude.filter (view (_1._1._3)) >-> PPrelude.tee (count _4) -- pass3
           >-> PPrelude.filter (view (_1._2))      >-> PPrelude.tee (count _5) -- pass4
           >-> PPrelude.filter (view (_1._3))      >-> PPrelude.tee (count _6) -- pass5
           >-> PPrelude.filter ((>=1) . view (_2)) >-> PPrelude.tee (count _7) -- 1 bjets, pT_bj > 50 
           >-> PPrelude.filter ((>=2) . view (_2)) >-> PPrelude.tee (count _8) -- 2 bjets, pT_bj > 50 
           >-> PPrelude.filter ((>=3) . view (_2)) >-> PPrelude.tee (count _9) -- 3 bjets, pT_bj > 50 

           -- >-> PPrelude.map (view (_1))
           >-> PPrelude.tee (PPrelude.print >-> PPrelude.drain)
           >-> PPrelude.drain
       
    print r                              

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

checkj :: PhyEventNoTauNoBJet -> (Bool,Bool,Bool)
checkj ev = let ptlst = jetpts ev
            in case ptlst of
                [] -> (False,False,False)
                x:xs -> 
                  let b1 = x > 200
                  in case xs of
                       _:_:y:ys -> let b2 = y > 100 
                                   in case ys of 
                                        _:z:_zs -> let b3 = z > 50
                                                   in (b1,b2,b3)
                                        _ -> (b1,b2,False)
                       _ -> (b1,False,False)


numOfB :: PhyEventNoTau -> Int
numOfB ev = let ptlst = (filter (> 50) . map pt . view bjets) ev
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





{-
  
main' :: IO ()
main' = do
    let fpaths = map (++ "_pgs_events.lhco.gz") . map (\x -> makeRunName psetup param (rsetupgen x)) $ [1..1000] 
    withFile "dummy.txt" WriteMode $ \hout -> do
      lst <- ({- fmap catMaybes . -} Tr.forM fpaths) $ \fpath ->  
               withFile fpath ReadMode $ \hin -> do 
                 putStrLn fpath
                 -- let testfold = Foldl.FoldM (\acc _ -> return (acc+1)) (return 0) return
                 r <- PPrelude.fold (\acc _ -> acc+1) 0 id  ( (pipesLHCOEvent (gunzip hin >-> PPrelude.map T.decodeUtf8) >> return () )
                                                              >-> PPrelude.tee (countmark 1000 0 >-> PPrelude.drain )
                                                              >-> PPrelude.map (filterEtaRange . mergeBJetFromNoTauEv . mkPhyEventNoTau)
                                                              >-> PPrelude.map ((,,) <$> checkj <*> checkl <*> htcut) 
                                                              >-> PPrelude.tee (PPrelude.print >-> PPrelude.drain)
                                                              >-> PPrelude.drain
                                                            )
                 --                >-> testsum 
                 --                >-> PPrelude.drain)    --  >-> PB.count
                 return r 
                 -- print r
                 -- print txt
                 {- runMaybeT $ do 
                   r' <- analysis bstr
                   r' `deepseq` return r' -- (fpath,r') -}
      -- hPutStrLn hout (show (combine3 lst))
      print (sum lst)
      return ()

-}




{- 
main1 :: IO ()
main1 = runMaybeT (analysis "fourtopsimpl_pgs_events.lhco.gz") >>= print 

-}

{-
combine :: [(String,Int,Int,Int,Int,Int,Int,[(Double,Int)])]  -> (Int,Int,Int,Int,Int,Int,[(Double,Int)])
combine  = foldl' f (0,0,0,0,0,0,map (\x->(x,0)) [10,20..300]) 
  where f (b1,b2,b3,b4,b5,b6,acc) (_,a1,a2,a3,a4,a5,a6,lst) = 
              (b1+a1,b2+a2,b3+a3,b4+a4,b5+a5,b6+a6,zipWith (\(x1,y1) (_x2,y2) -> (x1,y1+y2)) acc lst)

combine2 :: [(Int,Int,Int,Int,Int,Int,[(Double,Int)])]  -> (Int,Int,Int,Int,Int,Int,[(Double,Int)])
combine2  = foldl' f (0,0,0,0,0,0,map (\x->(x,0)) [10,20..300]) 
  where f (b1,b2,b3,b4,b5,b6,acc) (a1,a2,a3,a4,a5,a6,lst) = 
              (b1+a1,b2+a2,b3+a3,b4+a4,b5+a5,b6+a6,zipWith (\(x1,y1) (_x2,y2) -> (x1,y1+y2)) acc lst)

combine3 :: [(Int,Int,Int,Int,Int,Int,[(Double,Int)])]  -> (Int,Int,Int,Int,Int,Int,[(Double,Int)])
combine3 = foldl' f (0,0,0,0,0,0,map (\x->(x,0)) [10,20..300]) 
  where f arg1@(!b1,!b2,!b3,!b4,!b5,!b6,!acc) arg2@(!a1,!a2,!a3,!a4,!a5,!a6,!lst) = 
              arg1 `seq` arg2 `seq` 
                (b1+a1,b2+a2,b3+a3,b4+a4,b5+a5,b6+a6,zipWith (\(x1,y1) (_x2,y2) -> (x1,y1+y2)) acc lst)


analysis :: (MonadIO m) => LB.ByteString -> MaybeT m (Int,Int,Int,Int,Int,Int,[(Double,Int)]) 
analysis lbstr = do
    
    let bstr = LB.toStrict lbstr
        txt = T.decodeUtf8 bstr
        unmergedevts = case PA.parseOnly lhco txt of -- -- parsestr bstr
                         Left err -> error "lhco parse error"
                         Right evts -> evts

        taumergedevts = map mkPhyEventNoTau unmergedevts
        fullmergedevts = map mergeBJetFromNoTauEv taumergedevts

        etarangeevts = map filterEtaRange fullmergedevts

        test = (,,) <$> checkj <*> checkl <*> htcut
        pass1 x = view (_1._1) x
        pass2 x = pass1 x && view (_1._2) x
        pass3 x = pass1 x && pass2 x && view (_1._3) x
        pass4 x = pass1 x && pass2 x && pass3 x && view _2 x
        pass5 x = pass1 x && pass2 x && pass3 x && pass4 x && view _3 x

        pass1evts = filter (pass1.test) etarangeevts 
        pass2evts = filter (pass2.test) etarangeevts 
        pass3evts = filter (pass3.test) etarangeevts 
        pass4evts = filter (pass4.test) etarangeevts 
        pass5evts = filter (pass5.test) etarangeevts 

        assoclist = map (\x -> (x, (length . filter (ptlcut x)) pass5evts)) [10,20..300] 
    return (length fullmergedevts, length pass1evts, length pass2evts, length pass3evts, length pass4evts, length pass5evts, assoclist)


-- pipesAnalysis :: Pipes PhyEventClassified ( ) m r 
-}
