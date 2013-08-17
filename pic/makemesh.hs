import Control.Monad
import Data.List 
import Data.List.Split
import System.Environment

parse :: String -> (Double, Double, Double)
parse str = let x:y:z:_ = splitOn "," str
            in (read x, read y, read z) 


fillzero inc limit xs = let (x,y,_) = last xs 
                        in xs ++ [ (x,y',0) | y' <- [y+inc,y+2*inc..limit] ]  


main = do 
  args <- getArgs 
  str <- readFile (args !! 0)
  let inc = read (args !! 1) :: Double
      limit = read (args !! 2) :: Double 

  let ls = (map (fillzero inc limit) 
           . filter (not.null) 
           . fmap (fmap parse) 
           . splitWhen null 
           . map (dropWhile (== ' ')) 
           . lines) str 
  

  mapM_ (mapM_ (\(x,y,z) -> putStrLn (show x ++ ", " ++ show y ++ ", " ++ show z)) >=> \_ -> putStrLn " ")  ls
      
