{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

import           Codec.Compression.GZip
import           Control.Applicative
import           Control.Lens
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Maybe
import qualified Data.ByteString.Lazy.Char8 as LB
import           Data.Default       
import qualified Data.Foldable as F
import           Data.List (foldl')
import           System.Environment
import           System.IO as IO
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


main0 :: IO ()
main0 = do
    -- args <- getArgs 
    -- let pkey = args !! 0 
    --     pswd = args !! 1
   --      whost = args !! 2
    let fpaths = map (++ "_pgs_events.lhco.gz") . map (\x -> makeRunName psetup param (rsetupgen x)) $ [901..1000] 
    -- Just cr <- getCredential pkey pswd
    -- let cfg = WebDAVConfig { webdav_credential = cr, webdav_baseurl = whost }
    withFile "dummy.txt" WriteMode $ \h -> 
      runMaybeT $ do  
        r <- flip mapM fpaths $ \fpath -> do
          -- checkAndDownload cfg rdir fpath 
          {- analysis h fpath -}
          r' <- analysis fpath 
          liftIO $ print r'
          return r'
        liftIO $ print (combine r)
        return r
    return ()

main1 :: IO ()
main1 = runMaybeT (analysis "fourtopsimpl_pgs_events.lhco.gz") >>= print 

main :: IO ()
main = do
  str <- readFile "result2.txt"

  let rf = read :: String -> (Int,Int,Int,Int,Int,Int,[(Double,Int)])
   
  print $ lines str !! 1
  print $  rf (lines str !! 1)
  
  let rs = map rf . take 10 $ lines str
  print (combine2 rs)


combine :: [(String,Int,Int,Int,Int,Int,Int,[(Double,Int)])]  -> (Int,Int,Int,Int,Int,Int,[(Double,Int)])
combine  = foldl' f (0,0,0,0,0,0,map (\x->(x,0)) [10,20..300]) 
  where f (b1,b2,b3,b4,b5,b6,acc) (_,a1,a2,a3,a4,a5,a6,lst) = 
              (b1+a1,b2+a2,b3+a3,b4+a4,b5+a5,b6+a6,zipWith (\(x1,y1) (x2,y2) -> (x1,y1+y2)) acc lst)

combine2 :: [(Int,Int,Int,Int,Int,Int,[(Double,Int)])]  -> (Int,Int,Int,Int,Int,Int,[(Double,Int)])
combine2  = foldl' f (0,0,0,0,0,0,map (\x->(x,0)) [10,20..300]) 
  where f (b1,b2,b3,b4,b5,b6,acc) (a1,a2,a3,a4,a5,a6,lst) = 
              (b1+a1,b2+a2,b3+a3,b4+a4,b5+a5,b6+a6,zipWith (\(x1,y1) (x2,y2) -> (x1,y1+y2)) acc lst)


analysis fpath = do
    liftIO $ putStrLn $ "analyzing " ++ fpath 
    bstr <- liftIO $ LB.readFile fpath
    let unzipped = decompress bstr
        unmergedevts = parsestr unzipped
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
    return (fpath, length fullmergedevts, length pass1evts, length pass2evts, length pass3evts, length pass4evts, length pass5evts, assoclist)



ptlcut :: Double -> PhyEventNoTauNoBJet -> Bool
ptlcut cut ev = let lst = view leptons ev
                    f x = abs (eta x) < 1.5 && pt x > cut   
                in  any f lst


filterEtaRange = over jets (filter (\x -> abs (eta (JO_Jet x)) < 2.5)) 

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
                       _:_:y:ys -> let b2 = y > 70 
                                   in case ys of 
                                        _:z:zs -> let b3 = z > 40
                                                  in (b1,b2,b3)
                                        _ -> (b1,b2,False)
                       _ -> (b1,False,False)

leptonetas = map eta . view leptons


{- 
checkj2 :: PhyEventNoTauNoBJet -> (Bool,Bool)
checkj2 ev = let etalst = jetetas ev
             in case etalst of 
                  x1:x2:x3:x4:xs -> let b1 = abs x1 < 1.5 && abs x2 < 1.5 && abs x3 < 1.5 && abs x4 < 1.5
                                    in case xs of
                                         y1:y2:ys -> let b2 = abs y1 <2.5 && abs y2 < 2.5
                                                     in (b1,b2)
                                         _ -> (b1,False)
                  _ -> (False,False)
-}

htcut :: PhyEventNoTauNoBJet -> Bool 
htcut ev = let ptlst = jetpts ev
               ht6 = sum (take 6 ptlst)
           in ht6 > 750