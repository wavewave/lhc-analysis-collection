module Main where 

import Control.Monad 
import System.Environment (getArgs)
-- 
import HEP.Parser.XSec

main :: IO () 
main = do 
  putStrLn "simple cross section reading"
  args <- getArgs 
  when (length args /= 1) $ error "crosssec filename"
  let filename = args !! 0 
  r <- getXSecFromLHEGZ filename 
  print r


  
    
