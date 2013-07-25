import Control.Monad
import Data.List 
import Data.List.Split
import System.Environment

parse :: String -> (Double, Double, Double)
parse str = let x:y:z:_ = splitOn "," str
            in (read x, read y, read z) 


fillzero xs = let (x,y,_) = last xs 
              in xs ++ [ (x,y',0) | y' <- [y+50,y+100..1500] ]  


main = do 
  args <- getArgs 
  str <- readFile (args !! 0)

  let ls = (map fillzero . filter (not.null) . fmap (fmap parse) . splitWhen null . map (dropWhile (== ' ')) . lines) str 
  

  mapM_ (mapM_ (\(x,y,z) -> putStrLn (show x ++ ", " ++ show y ++ ", " ++ show z)) >=> \_ -> putStrLn " ")  ls
      
