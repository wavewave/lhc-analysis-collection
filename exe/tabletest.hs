module Main where 

import HEP.Util.Table

s1 :: Table Int 
s1 = singletonTable 1

s2 = singletonTable 2 

main :: IO ()
main = do 
  putStrLn "table test"
  let x = (s1 <|> s2 <|> s1)
      y = (s1 <|> s1 )
  print (x <-> y)

  print (resizeRow 3 [[Just 3]]) 
  -- putStrLn (showLaTeX ((s1 <-> s2) <|> s1))
