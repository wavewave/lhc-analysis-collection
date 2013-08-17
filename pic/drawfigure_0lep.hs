import System.Environment
import System.Directory
import Text.StringTemplate

main = main_xqld_squarklosp

main_xqld_squarklosp :: IO ()
main_xqld_squarklosp = do 
  args <- getArgs
  cdir <- getCurrentDirectory
  tmpl <- (directoryGroup cdir :: IO (STGroup String))
  let mneutstr = args !! 0
      Just cntrtmpl = getStringTemplate "contour_0lep.gpl" tmpl
      Just cmbdtmpl = getStringTemplate "combined_0lep.gpl" tmpl

      simplstr = (toString . flip setManyAttrib cntrtmpl) 
                   [ ("xvar", "Gluino")
                   , ("xmin", "500")
                   , ("xmax", "3000")
                   , ("yvar", "Squark")
                   , ("ymin", "500") 
                   , ("ymax", "3000")
                   , ("datname", "simplifiedsusy_sqsg_0lep_8TeV")
                   ]

      
      xqldstr = (toString . flip setManyAttrib cntrtmpl) 
                  [ ("xvar", "Gluino") 
                  , ("xmin", "500")
                  , ("xmax", "3000")
                  , ("yvar", "Squark")
                  , ("ymin", "500") 
                  , ("ymax", "3000")
                  , ("datname", "xqld_squarklosp_sqsgscan_0lep_8TeV")
                  ] 
     
      
      cmbdstr = (toString . flip setManyAttrib cmbdtmpl)      
                  [ ("xvar", "Gluino") 
                  , ("xmin", "500")
                  , ("xmax", "3000")
                  , ("yvar", "Squark")
                  , ("ymin", "500") 
                  , ("ymax", "3000")
                  , ("datnameA", "xqld_squarklosp_sqsgscan_0lep_8TeV")
                  , ("datnameB", "simplifiedsusy_sqsg_0lep_8TeV")
                  , ("modelaliasA", "Xqld")
                  , ("modelaliasB", "Simplified")
                  , ("figurefilename", "xqld_squarklosp_0lep_sqsgscan")
                  ] 

  writeFile "contour_0lep_sqsg_simplifiedsusy.gpl" simplstr
  writeFile "contour_0lep_sqsg_xqld_squarklosp.gpl" xqldstr
  writeFile "combined_0lep_sqsg_xqld_squarklosp.gpl" cmbdstr




