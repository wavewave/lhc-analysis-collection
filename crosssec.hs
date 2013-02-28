module Main where 


import Control.Monad 
import Control.Monad.List 
import System.Environment (getArgs)
import System.FilePath
-- 
import HEP.Parser.XSec
import HEP.Util.Format
import HEP.Util.Table 

-- testdir = "/Users/iankim/repo/workspace/montecarlo/mc/Test28_20130227_ADMXQLD111" 

testdir = "/Users/iankim/repo/workspace/montecarlo/mc/Test29_20130227_ADMXQLD111" 

workname :: Double -> Double -> String 
workname mg mq = "ADMXQLD111MG" ++ show mg ++ "MQ" ++ show mq ++ "ML50000.0MN50000.0_supgluino_LHC7ATLAS_NoMatch_NoCut_Cone0.4_Set1"
-- workname mg mq = "ADMXQLD111MG" ++ show mg ++ "MQ" ++ show mq ++ "ML50000.0MN50000.0_suppair_LHC7ATLAS_NoMatch_NoCut_Cone0.4_Set1"

lhegzname :: Double -> Double -> String 
lhegzname mg mq = workname mg mq ++ "_unweighted_events.lhe.gz"  


fullPathLHEGZ :: Double -> Double -> String 
fullPathLHEGZ mg mq = testdir </> "Events" </> workname mg mq 
                              </> lhegzname mg mq 


xsec :: Double -> Double -> IO (Table Double)
xsec mg mq = do 
    getXSecFromLHEGZ (fullPathLHEGZ mg mq) >>= 
      either (\_ -> return nothingSingletonTable)
             (\x -> return (singletonTable (1000*x)))



main :: IO () 
main = do 
  putStrLn "simple cross section reading"
  cs <- runListT $ do 
    mg <- ListT (return [200,300..2000] )
    xs <- mapM (liftIO . xsec mg) [100,200..mg-100] 
    let combined = foldr (<|>) emptyTable xs
    return combined 

  let combined = foldr (<->) emptyTable cs
  putStrLn $ showLaTeXBy (sciformat 2 . Just) combined 

  -- print combined 
  --    t <- liftIO (xsec mg mq)

  -- let combined = foldr (<|>) emptyTable xs 
  -- mapM_ print cs 

  {- args <- getArgs 
  when (length args /= 2) $ error "crosssec filename"
  let mg = read (args !! 0) 
      mq = read (args !! 1)-}


  
    
