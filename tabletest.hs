module Main where 

import HEP.Util.Table

s1 :: Table Int 
s1 = singletonTable 1

s2 = singletonTable 2 

main :: IO ()
main = do 
  putStrLn "table test"
  let x = (s1 <|> s2 <|> s1)
  print (x <-> x)
  -- putStrLn (showLaTeX ((s1 <-> s2) <|> s1))
