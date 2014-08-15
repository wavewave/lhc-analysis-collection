{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}

import           Codec.Compression.GZip
import           Control.Applicative
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Maybe
import qualified Data.ByteString.Lazy.Char8 as LB
import qualified Data.Foldable as F
import System.Environment
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
import HEP.Physics.Analysis.Common.Merge
import HEP.Physics.Analysis.Common.PhyEventNoTau

data PrunedEvent = Unpruned PhyEventClassified
                 | TauMerged PhyEventNoTau

mkPhyEventNoTau :: PhyEventClassified -> PhyEventNoTau
mkPhyEventNoTau PhyEventClassified {..} =
    PhyEventNoTau { nt_eventId = eventid 
                  , nt_photons = map snd photonlst
                  , nt_electrons = map snd electronlst
                  , nt_muons = map snd muonlst
                  , nt_jets = (map snd . ptordering . (jetlst ++) . map ((,) <$> fst <*> tau2Jet.snd)) taulst
                  , nt_bjets = map snd bjetlst
                  , nt_missingET = met
                  }


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
  let fpaths = map (++ "_pgs_events.lhco.gz") . map (\x -> makeRunName psetup param (rsetupgen x)) $ [1..3] -- [1..1000]

  Just cr <- getCredential pkey pswd
  let cfg = WebDAVConfig { webdav_credential = cr, webdav_baseurl = whost }
  runMaybeT $ do  
    F.forM_ fpaths $ \fpath -> do
      b <- liftIO $ doesFileExistInDAV cfg rdir fpath
      guard b

      liftIO $ downloadFile False cfg rdir fpath 

      liftIO $ putStrLn $ fpath ++ " is successfully downloaded"

      bstr <- liftIO $ LB.readFile fpath
      let unzipped = decompress bstr
          unmergedevts = parsestr unzipped
          mergedevts = map mkPhyEventNoTau unmergedevts
          -- totnum = length evts 
          evt = head mergedevts :: PhyEventNoTau
      
      liftIO $ print (nt_jets evt)

  return ()
