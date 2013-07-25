import System.Environment
import System.Directory
import Text.StringTemplate

main :: IO ()
main = do 
  args <- getArgs
  cdir <- getCurrentDirectory
  tmpl <- (directoryGroup cdir :: IO (STGroup String))
  let mneutstr = args !! 0
      Just cntrtmpl = getStringTemplate "contour.gpl" tmpl
      Just cmbdtmpl = getStringTemplate "combined.gpl" tmpl

      simplstr = (toString . flip setManyAttrib cntrtmpl) 
                   [ ("mass", mneutstr) 
                   , ("xmin", "600")
                   , ("xmax", "3000")
                   , ("ymin", "600") 
                   , ("ymax", "3000")
                   , ("modelname", "simplifiedsusy")
                   ]
      {-
      xuddstr  = (toString . flip setManyAttrib cntrtmpl) 
                   [ ("mass", mneutstr) 
                   , ("xmin", "600")
                   , ("xmax", "3000")
                   , ("ymin", "600") 
                   , ("ymax", "3000")
                   , ("modelname", "xudd_neutLOSP")
                   ] 
     -}
      
      xqldstr  = (toString . flip setManyAttrib cntrtmpl) 
                   [ ("mass", mneutstr) 
                   , ("xmin", "600")
                   , ("xmax", "3000")
                   , ("ymin", "600") 
                   , ("ymax", "3000")
                   , ("modelname", "xqld_neutLOSP")
                   ] 
      cmbdstr = (toString . flip setManyAttrib cmbdtmpl)      
                   [ ("mass", mneutstr) 
                   , ("xmin", "600")
                   , ("xmax", "3000")
                   , ("ymin", "600") 
                   , ("ymax", "3000")
                   , ("modelname", "xqld_neutLOSP")
                   , ("modelalias", "Xqld")
                   ]

  writeFile "contour_simplifiedsusy.gpl" simplstr
  writeFile "contour_xqld.gpl" xqldstr
  -- writeFile "contour_xudd.gpl" xuddstr
  writeFile "combined.gpl" cmbdstr 
 



