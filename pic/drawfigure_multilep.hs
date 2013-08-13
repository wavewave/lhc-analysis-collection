import System.Environment
import System.Directory
import Text.StringTemplate

main :: IO ()
main = do 
  args <- getArgs
  cdir <- getCurrentDirectory
  tmpl <- (directoryGroup cdir :: IO (STGroup String))
  let mneutstr = args !! 0
      Just cntrtmpl = getStringTemplate "contour_multilep.gpl" tmpl
      Just cmbdtmpl = getStringTemplate "combined_multilep.gpl" tmpl

      simplstr_2sq = (toString . flip setManyAttrib cntrtmpl) 
                       [ ("xvar", "Squark")
                       , ("xmin", "200")
                       , ("xmax", "1300")
                       , ("yvar", "Neutralino")
                       , ("ymin", "0") 
                       , ("ymax", "1000")
                       , ("datname", "simplifiedsusylep_1step_2sq_8TeV")
                       ]

      
      xqldstr_2sq  = (toString . flip setManyAttrib cntrtmpl) 
                       [ ("xvar", "Squark") 
                       , ("xmin", "200")
                       , ("xmax", "1300")
                       , ("yvar", "Neutralino")
                       , ("ymin", "0") 
                       , ("ymax", "1000")
                       , ("datname", "xqld_neutlosp_mqmnscan_8TeV")
                       ] 
     
      
      cmbdstr = (toString . flip setManyAttrib cmbdtmpl)      
                  [ ("xvar", "Squark") 
                  , ("xmin", "200")
                  , ("xmax", "1300")
                  , ("yvar", "Neutralino")
                  , ("ymin", "0") 
                  , ("ymax", "1000")
                  , ("datnameA", "xqld_neutlosp_mqmnscan_8TeV")
                  , ("datnameB", "simplifiedsusylep_1step_2sq_8TeV")
                  , ("modelaliasA", "Xqld")
                  , ("modelaliasB", "Simplified")
                  , ("figurefilename", "multilep_mqmnscan")
                  ] 

  -- putStrLn simplstr_2sq
  writeFile "contour_multilep_2sq_simplifiedsusy.gpl" simplstr_2sq
  writeFile "contour_multilep_2sq_xqld.gpl" xqldstr_2sq
  writeFile "combined_multilep_2sq.gpl" cmbdstr 
  {-
  writeFile "contour_xqld.gpl" xqldstr
  -- writeFile "contour_xudd.gpl" xuddstr

  -}


