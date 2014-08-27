{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

import qualified Codec.Zlib as Zlib
import           Control.Applicative
import           Control.DeepSeq
import           Control.Lens
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Maybe
import qualified Data.ByteString.Lazy.Char8 as LB
import           Data.Default       
import           Data.List (foldl')
import           Data.Maybe (catMaybes)
import qualified Data.Traversable as Tr
import qualified Pipes.ByteString  as PB
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
    let fpaths = map (++ "_pgs_events.lhco.gz") . map (\x -> makeRunName psetup param (rsetupgen x)) $ [1..1000] 
    withFile "dummy.txt" WriteMode $ \hout -> do
      lst <- (fmap catMaybes . Tr.forM fpaths) $ \fpath ->  
               withFile fpath ReadMode $ \hin -> do 
                 putStrLn fpath
                 bstr <- PB.toLazyM (PZ.decompress (Zlib.WindowBits 31) (PB.fromHandle hin))   --  >-> PB.count
                 runMaybeT $ do 
                   r' <- analysis bstr
                   r' `deepseq` return r' -- (fpath,r')
      hPutStrLn hout (show (combine3 lst))

{- 
main1 :: IO ()
main1 = runMaybeT (analysis "fourtopsimpl_pgs_events.lhco.gz") >>= print 

-}

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
analysis bstr = do
    let unmergedevts = parsestr bstr
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

leptonetas :: PhyEventNoTauNoBJet -> [Double]
leptonetas = map eta . view leptons

htcut :: PhyEventNoTauNoBJet -> Bool 
htcut ev = let ptlst = jetpts ev
               ht6 = sum (take 6 ptlst)
           in ht6 > 1200