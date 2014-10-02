import Control.Applicative
import           Control.Monad.Trans
import           Control.Monad.State
import           Data.Conduit 
import qualified Data.Conduit.List as CL
import qualified Data.Foldable as F
import           Data.Traversable (sequenceA)
import           Data.Maybe
import qualified Data.Text.IO as TIO
import           System.IO
import           Text.XML.Conduit.Parse.Util
-- 
-- import Data.Conduit.Internal as CU
import Data.Conduit.Util.Count 
import HEP.Parser.LHE.Conduit
import HEP.Parser.LHE.Type
import HEP.Parser.LHE.DecayTop



main = do 
  putStrLn "test"

  withFile "unweighted_events_4toppart.lhe" ReadMode $ \ih -> do 
    let iter = do
          header <- textLHEHeader 
          liftIO $ mapM_ TIO.putStrLn header 
          parseEvent =$ process 
        process = decayTopConduit
                  =$ CL.isolate 100 
                  =$ getZipSink (ZipSink (tester 0) <* ZipSink countMarkerIter)
    r <- flip runStateT (0 :: Int) (parseXmlFile ih iter)
    putStrLn $ show r 



tester :: (MonadIO m) => Int -> Sink LHEventTop m Int
tester n = do
  ev <- await
  case ev of 
    Nothing -> return n 
    Just ev -> do liftIO $ putStrLn $ matchTest ev
                  tester (n+1)

{- 
testev :: LHEvent -> IO ()
testev ev@(LHEvent evinfo ptlinfos) = -- print (nup evinfo)
  putStrLn $ smplPrint $ getDecayTop ev
-}

smplPrint :: LHEventTop -> String
smplPrint = show . map (fmap getPDGID ) .  lhet_dtops


hhiggs= [35]
lep = [11,13,-11,-13]
neut = [12,14,-12,-14]

jets = [1,2,3,4,-1,-2,-3,-4]

tq = [6]
tbarq = [-6]
bq = [5]
bbarq= [-5]

wp = [24]
wm = [-24]

matchTest :: LHEventTop -> String
matchTest ev = 

    let m1 = matchDecayTop (Decay (( 1,tbarq), [ Decay (( 2,wm),[Terminal (3,jets),Terminal (4,jets)])
                                              , Terminal (5,bbarq)
                                              ]))
        m2 = matchDecayTop (Decay (( 6,tq), [ Decay (( 7,wp),[Terminal (8,jets),Terminal (9,jets)])
                                              , Terminal (10,bq)
                                             ]))
        m3 = matchDecayTop (Decay ((11,hhiggs), [ Decay ((12,tq), [ Decay ((13,wp), [ Terminal (14,lep)
                                                                                    , Terminal (15,neut)
                                                                                    ])
                                                                  , Terminal (16,bq) 
                                                                  ])
                                                , Decay ((17,tbarq), [ Decay ((18,wm), [ Terminal (19,lep)
                                                                                       , Terminal (20,neut)
                                                                                       , Terminal (21,bbarq)
                                                                                       ])
                                                                     ])
                                                ]))
        r1 = let r = mapMaybe m1 dtops in if null r then Nothing else Just (head r)
        r2 = let r = mapMaybe m2 dtops in if null r then Nothing else Just (head r)
        r3 = let r = mapMaybe m3 dtops in if null r then Nothing else Just (head r)
    in show (sequenceA [r1,r2,r3])
  where dtops = lhet_dtops ev


