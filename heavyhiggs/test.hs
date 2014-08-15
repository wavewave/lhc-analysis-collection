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

{-
eventsets :: [EventSet]
eventsets = [ EventSet psetup param (rsetupgen x) | x <- [1..1000] ] 
-}

rdir :: WebDAVRemoteDir
rdir = WebDAVRemoteDir "newtest2"

main :: IO ()
main = do
  putStrLn "test"
  args <- getArgs 
  let pkey = args !! 0 
      pswd = args !! 1
      whost = args !! 2
      -- rdir = WebDAVRemoteDir (args !! 3)
      -- fpath = (args !! 4)
  let fpaths = map (++ "_pgs_events.lhco.gz") . map (\x -> makeRunName psetup param (rsetupgen x)) $ [282..1000] -- [1..280]

  Just cr <- getCredential pkey pswd
  let cfg = WebDAVConfig { webdav_credential = cr, webdav_baseurl = whost }
  withFile "result282-1000.txt" WriteMode $ \h -> 
    runMaybeT $ do  
      F.forM_ fpaths $ \fpath -> do
        b <- liftIO $ doesFileExistInDAV cfg rdir fpath
        guard b

        liftIO $ downloadFile False cfg rdir fpath 

        liftIO $ putStrLn $ fpath ++ " is successfully downloaded"

        bstr <- liftIO $ LB.readFile fpath
        let unzipped = decompress bstr
            unmergedevts = parsestr unzipped
            taumergedevts = map mkPhyEventNoTau unmergedevts
            -- totnum = length evts 
            -- evt = head mergedevts :: PhyEventNoTau
            --- evt' = mergeBJetFromNoTauEv evt
            fullmergedevts = map mergeBJetFromNoTauEv taumergedevts
            test = (,,) <$> checkj <*> checkl <*> checkj2
            -- pass1 :: ((Bool,Bool,Bool),Bool,(Bool,Bool)) -> Bool
            pass1 x = view (_1._1) x
            -- pass2 :: ((Bool,Bool,Bool),Bool,(Bool,Bool)) -> Bool
            pass2 x = pass1 x && view (_1._2) x
            -- pass3 :: ((Bool,Bool,Bool),Bool,(Bool,Bool)) -> Bool
            pass3 x = pass1 x && pass2 x && view (_1._3) x
            -- pass4 :: ((Bool,Bool,Bool),Bool,(Bool,Bool)) -> Bool
            pass4 x = pass1 x && pass2 x && pass3 x && view _2 x
            -- pass5 :: ((Bool,Bool,Bool),Bool,(Bool,Bool)) -> Bool
            pass5 x = pass1 x && pass2 x && pass3 x && pass4 x && view (_3._1) x
            -- pass6 :: ((Bool,Bool,Bool),Bool,(Bool,Bool)) -> Bool
            pass6 x = pass1 x && pass2 x && pass3 x && pass4 x && pass5 x && view (_3._2) x

            pass1evts = filter (pass1.test) fullmergedevts
            pass2evts = filter (pass2.test) fullmergedevts
            pass3evts = filter (pass3.test) fullmergedevts
            pass4evts = filter (pass4.test) fullmergedevts
            pass5evts = filter (pass5.test) fullmergedevts
            pass6evts = filter (pass6.test) fullmergedevts


        -- liftIO $ mapM_ print (map (\ev->(jetpts ev,checkj ev)) fullmergedevts)  
        -- liftIO $ mapM_ print (map (\ev->(leptonetas ev,checkl ev)) fullmergedevts)  
        -- liftIO $ mapM_ print (map (\ev->(jetetas ev,checkj2 ev)) fullmergedevts)
        -- liftIO $ print (over (jets. mapped) show evt')

        liftIO $ IO.hPutStrLn h 
          (fpath ++ ": " ++ show (length pass1evts,length pass2evts,length pass3evts,length pass4evts,length pass5evts,length pass6evts))
  return ()





jetpts = map (pt . JO_Jet) . view jets

jetetas = map (eta . JO_Jet) . view jets

checkl :: PhyEventNoTauNoBJet -> Bool
checkl ev = let etalst = leptonetas ev
            in (not . null . filter (\x -> abs x < 1.5)) etalst

{- 
checkj :: PhyEventNoTauNoBJet -> Bool 
checkj ev = let etalst = jetetas ev
                eta_1_5 = filter (\x -> abs x < 1.5) etalst
                eta_2_5 = filter (\x -> abs x < 2.5) etalst
            in length eta_1_5 >= 4 && length eta_2_5 >= 6 
-}

checkj :: PhyEventNoTauNoBJet -> (Bool,Bool,Bool)
checkj ev = let ptlst = jetpts ev
            in case ptlst of
                [] -> (False,False,False)
                x:xs -> 
                  let b1 = x > 120
                  in case xs of
                       _:_:y:ys -> let b2 = y > 70 
                                   in case ys of 
                                        _:z:zs -> let b3 = z > 40
                                                  in (b1,b2,b3)
                                        _ -> (b1,b2,False)
                       _ -> (b1,False,False)

leptonetas = map eta . view leptons

checkj2 :: PhyEventNoTauNoBJet -> (Bool,Bool)
checkj2 ev = let etalst = jetetas ev
             in case etalst of 
                  x1:x2:x3:x4:xs -> let b1 = abs x1 < 1.5 && abs x2 < 1.5 && abs x3 < 1.5 && abs x4 < 1.5
                                    in case xs of
                                         y1:y2:ys -> let b2 = abs y1 <2.5 && abs y2 < 2.5
                                                     in (b1,b2)
                                         _ -> (b1,False)
                  _ -> (False,False)
