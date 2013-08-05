import System.Environment 
import System.FilePath
import System.Directory 
import Text.StringTemplate



-- exename = "2013-07-04-SimplifiedSUSY" -- "2013-07-05-XUDD" -- "2013-07-03-XQLD"

main :: IO ()
main = do 
  args <- getArgs 
  let exename = args !! 0  
      mode = args !! 1 
      njob = (read (args !! 2) :: Int) 
  mapM_ (work exename mode) [0..njob-1] 

work exename mode n = do 
  let (n1,n2) = case mode of  
                  "twenty" -> (n*20+1,(n+1)*20)
                  "ten" -> (n*10+1,(n+1)*10)
                  "five" -> (n*5 +1,(n+1)*5) 
                  "one" ->  (n+1,n+1)
  cdir <- getCurrentDirectory 
  tmpl <- (directoryGroup cdir :: IO (STGroup String))
  let Just t = getStringTemplate "kzurek.pbs" tmpl 
      str = (toString . flip setManyAttrib t) [ ("exename",exename), ("n1",show n1), ("n2",show n2) ]  
  writeFile ("kzurek"++(show (n+1)) <.> "pbs") str 

{-
  -- writeFile ("kzurek"++(show (n+101)) <.> "pbs") str 
  -- writeFile ("kzurek"++(show (n+201)) <.> "pbs") str 

  -- writeFile ("kzurek"++(show (n+301)) <.> "pbs") str 
  -- writeFile ("kzurek"++(show (n+401)) <.> "pbs") str 
  -- writeFile ("kzurek"++(show (n+501)) <.> "pbs") str 
-}

