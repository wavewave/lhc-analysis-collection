{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ScopedTypeVariables #-}

import           Data.Data
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TLIO
import           Text.Hastache
import           Text.Hastache.Context
import           Text.Printf

data Info = Info { contents :: TL.Text } deriving (Data,Typeable)

main :: IO ()
main = do
  str <- readFile "optcutlep1000.dat" 
  let ls = lines str
  let tblcnts = TL.pack (concatMap (renderOne.parseOne.words) ls)
  
  txt <- hastacheFile defaultConfig "longtable.tex.hastache" (mkGenericContext (Info tblcnts))
  TLIO.putStrLn txt


parseOne :: [String] -> (Double,Double,Double,Double,Double,Int,Int,Int,Int,Int,Int,Int,Int,Int)
parseOne (x1:x2:x3:x4:x5:y1:y2:y3:y4:y5:y6:y7:y8:y9:[]) = 
 (read x1,read x2,read x3,read x4,read x5,read y1,read y2,read y3,read y4,read y5,read y6,read y7,read y8,read y9)

integerize :: Double -> Int
integerize = round

renderOne :: (Double,Double,Double,Double,Double,Int,Int,Int,Int,Int,Int,Int,Int,Int) -> String
renderOne (x1,x2,x3,x4,x5,y1,y2,y3,y4,y5,y6,y7,y8,y9) = 
    let [x1',x2',x3',x4',x5'] = map integerize [x1,x2,x3,x4,x5]
    in printf "%5d & %5d & %5d & %5d & %5d & %7d & %7d & %7d & %7d & %7d & %7d & %7d & %7d & %7d \\\\\n" x1' x2' x3' x4' x5' y1 y2 y3 y4 y5 y6 y7 y8 y9 


