import System.Environment
import System.Directory
import Text.StringTemplate

main = main_xqld_neutlosp100_0lep

-- main_xqld_squarklosp



-- main_xudd_neutlosp100_0lep

-- main_xudd_squarklosp 

 -- main_xqld_squarklosp_1lep -- main_xudd_squarklosp -- main_xqld_squarklosp

main_xudd_neutlosp100_0lep :: IO ()
main_xudd_neutlosp100_0lep = do 
  args <- getArgs
  cdir <- getCurrentDirectory
  tmpl <- (directoryGroup cdir :: IO (STGroup String))
  let mneutstr = args !! 0
      Just cntrtmpl = getStringTemplate "contour_0lep.gpl" tmpl
      Just cmbdtmpl = getStringTemplate "combined_0lep.gpl" tmpl

      simplstr = (toString . flip setManyAttrib cntrtmpl) 
                   [ ("xvar", "Gluino")
                   , ("xmin", "400")
                   , ("xmax", "3000")
                   , ("yvar", "Squark")
                   , ("ymin", "400") 
                   , ("ymax", "3000")
                   , ("datname", "simplifiedsusy100.0_0lep_sqsg_8TeV")
                   ]

      
      xuddstr = (toString . flip setManyAttrib cntrtmpl) 
                  [ ("xvar", "Gluino") 
                  , ("xmin", "400")
                  , ("xmax", "3000")
                  , ("yvar", "Squark")
                  , ("ymin", "400") 
                  , ("ymax", "3000")
                  , ("datname", "xudd_neutLOSP100.0_sqsg_8TeV_0lep")
                  ] 
     
      
      cmbdstr = (toString . flip setManyAttrib cmbdtmpl)      
                  [ ("xvar", "Gluino") 
                  , ("xmin", "400")
                  , ("xmax", "3000")
                  , ("yvar", "Squark")
                  , ("ymin", "400") 
                  , ("ymax", "3000")
                  , ("datnameA", "xudd_neutLOSP100.0_sqsg_8TeV_0lep")
                  , ("datnameB", "simplifiedsusy100.0_0lep_sqsg_8TeV")
                  , ("modelaliasA", "Xudd")
                  , ("modelaliasB", "Simplified")
                  , ("figurefilename", "xudd_neutLOSP100_0lep_sqsgscan")
                  ] 

  writeFile "contour_0lep_sqsg_simplifiedsusy100.gpl" simplstr
  writeFile "contour_0lep_sqsg_xudd_neutLOSP100.gpl" xuddstr
  writeFile "combined_0lep_sqsg_xudd_neutLOSP100.gpl" cmbdstr



main_xudd_neutlosp300_0lep :: IO ()
main_xudd_neutlosp300_0lep = do 
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
                   , ("datname", "simplifiedsusy300.0_0lep_sqsg_8TeV")
                   ]

      
      xuddstr = (toString . flip setManyAttrib cntrtmpl) 
                  [ ("xvar", "Gluino") 
                  , ("xmin", "500")
                  , ("xmax", "3000")
                  , ("yvar", "Squark")
                  , ("ymin", "500") 
                  , ("ymax", "3000")
                  , ("datname", "xudd_neutLOSP300.0_sqsg_8TeV_0lep")
                  ] 
     
      
      cmbdstr = (toString . flip setManyAttrib cmbdtmpl)      
                  [ ("xvar", "Gluino") 
                  , ("xmin", "500")
                  , ("xmax", "3000")
                  , ("yvar", "Squark")
                  , ("ymin", "500") 
                  , ("ymax", "3000")
                  , ("datnameA", "xudd_neutLOSP300.0_sqsg_8TeV_0lep")
                  , ("datnameB", "simplifiedsusy300.0_0lep_sqsg_8TeV")
                  , ("modelaliasA", "Xudd")
                  , ("modelaliasB", "Simplified")
                  , ("figurefilename", "xudd_neutLOSP300_0lep_sqsgscan")
                  ] 

  writeFile "contour_0lep_sqsg_simplifiedsusy300.gpl" simplstr
  writeFile "contour_0lep_sqsg_xudd_neutLOSP300.gpl" xuddstr
  writeFile "combined_0lep_sqsg_xudd_neutLOSP300.gpl" cmbdstr



main_xudd_neutlosp500_0lep :: IO ()
main_xudd_neutlosp500_0lep = do 
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
                   , ("datname", "simplifiedsusy500.0_0lep_sqsg_8TeV")
                   ]

      
      xuddstr = (toString . flip setManyAttrib cntrtmpl) 
                  [ ("xvar", "Gluino") 
                  , ("xmin", "500")
                  , ("xmax", "3000")
                  , ("yvar", "Squark")
                  , ("ymin", "500") 
                  , ("ymax", "3000")
                  , ("datname", "xudd_neutLOSP500.0_sqsg_8TeV_0lep")
                  ] 
     
      
      cmbdstr = (toString . flip setManyAttrib cmbdtmpl)      
                  [ ("xvar", "Gluino") 
                  , ("xmin", "500")
                  , ("xmax", "3000")
                  , ("yvar", "Squark")
                  , ("ymin", "500") 
                  , ("ymax", "3000")
                  , ("datnameA", "xudd_neutLOSP500.0_sqsg_8TeV_0lep")
                  , ("datnameB", "simplifiedsusy500.0_0lep_sqsg_8TeV")
                  , ("modelaliasA", "Xudd")
                  , ("modelaliasB", "Simplified")
                  , ("figurefilename", "xudd_neutLOSP500_0lep_sqsgscan")
                  ] 

  writeFile "contour_0lep_sqsg_simplifiedsusy500.gpl" simplstr
  writeFile "contour_0lep_sqsg_xudd_neutLOSP500.gpl" xuddstr
  writeFile "combined_0lep_sqsg_xudd_neutLOSP500.gpl" cmbdstr


main_xqld_neutlosp500_0lep :: IO ()
main_xqld_neutlosp500_0lep = do 
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
                   , ("datname", "simplifiedsusy500.0_0lep_sqsg_8TeV")
                   ]

      
      xqldstr = (toString . flip setManyAttrib cntrtmpl) 
                  [ ("xvar", "Gluino") 
                  , ("xmin", "500")
                  , ("xmax", "3000")
                  , ("yvar", "Squark")
                  , ("ymin", "500") 
                  , ("ymax", "3000")
                  , ("datname", "xqld_neutLOSP500.0_sqsg_8TeV_0lep")
                  ] 
     
      
      cmbdstr = (toString . flip setManyAttrib cmbdtmpl)      
                  [ ("xvar", "Gluino") 
                  , ("xmin", "500")
                  , ("xmax", "3000")
                  , ("yvar", "Squark")
                  , ("ymin", "500") 
                  , ("ymax", "3000")
                  , ("datnameA", "xqld_neutLOSP500.0_sqsg_8TeV_0lep")
                  , ("datnameB", "simplifiedsusy500.0_0lep_sqsg_8TeV")
                  , ("modelaliasA", "Xqld")
                  , ("modelaliasB", "Simplified")
                  , ("figurefilename", "xqld_neutLOSP500_0lep_sqsgscan")
                  ] 

  writeFile "contour_0lep_sqsg_simplifiedsusy500.gpl" simplstr
  writeFile "contour_0lep_sqsg_xqld_neutLOSP500.gpl" xqldstr
  writeFile "combined_0lep_sqsg_xqld_neutLOSP500.gpl" cmbdstr



main_xqld_neutlosp500_1lep :: IO ()
main_xqld_neutlosp500_1lep = do 
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
                   , ("datname", "simplifiedsusy500.0_0lep_sqsg_8TeV")
                   ]

      
      xqldstr = (toString . flip setManyAttrib cntrtmpl) 
                  [ ("xvar", "Gluino") 
                  , ("xmin", "500")
                  , ("xmax", "3000")
                  , ("yvar", "Squark")
                  , ("ymin", "500") 
                  , ("ymax", "3000")
                  , ("datname", "xqld_neutLOSP500.0_sqsg_8TeV_1lep")
                  ] 
     
      
      cmbdstr = (toString . flip setManyAttrib cmbdtmpl)      
                  [ ("xvar", "Gluino") 
                  , ("xmin", "500")
                  , ("xmax", "3000")
                  , ("yvar", "Squark")
                  , ("ymin", "500") 
                  , ("ymax", "3000")
                  , ("datnameA", "xqld_neutLOSP500.0_sqsg_8TeV_1lep")
                  , ("datnameB", "simplifiedsusy500.0_0lep_sqsg_8TeV")
                  , ("modelaliasA", "Xqld")
                  , ("modelaliasB", "Simplified")
                  , ("figurefilename", "xqld_neutLOSP500_1lep_sqsgscan")
                  ] 

  writeFile "contour_1lep_sqsg_simplifiedsusy500.gpl" simplstr
  writeFile "contour_1lep_sqsg_xqld_neutLOSP500.gpl" xqldstr
  writeFile "combined_1lep_sqsg_xqld_neutLOSP500.gpl" cmbdstr



main_xqld_neutlosp300_0lep :: IO ()
main_xqld_neutlosp300_0lep = do 
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
                   , ("datname", "simplifiedsusy300.0_0lep_sqsg_8TeV")
                   ]

      
      xqldstr = (toString . flip setManyAttrib cntrtmpl) 
                  [ ("xvar", "Gluino") 
                  , ("xmin", "500")
                  , ("xmax", "3000")
                  , ("yvar", "Squark")
                  , ("ymin", "500") 
                  , ("ymax", "3000")
                  , ("datname", "xqld_neutLOSP300.0_sqsg_8TeV_0lep")
                  ] 
     
      
      cmbdstr = (toString . flip setManyAttrib cmbdtmpl)      
                  [ ("xvar", "Gluino") 
                  , ("xmin", "500")
                  , ("xmax", "3000")
                  , ("yvar", "Squark")
                  , ("ymin", "500") 
                  , ("ymax", "3000")
                  , ("datnameA", "xqld_neutLOSP300.0_sqsg_8TeV_0lep")
                  , ("datnameB", "simplifiedsusy300.0_0lep_sqsg_8TeV")
                  , ("modelaliasA", "Xqld")
                  , ("modelaliasB", "Simplified")
                  , ("figurefilename", "xqld_neutLOSP300_0lep_sqsgscan")
                  ] 

  writeFile "contour_0lep_sqsg_simplifiedsusy300.gpl" simplstr
  writeFile "contour_0lep_sqsg_xqld_neutLOSP300.gpl" xqldstr
  writeFile "combined_0lep_sqsg_xqld_neutLOSP300.gpl" cmbdstr



main_xqld_neutlosp300_1lep :: IO ()
main_xqld_neutlosp300_1lep = do 
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
                   , ("datname", "simplifiedsusy300.0_0lep_sqsg_8TeV")
                   ]

      
      xqldstr = (toString . flip setManyAttrib cntrtmpl) 
                  [ ("xvar", "Gluino") 
                  , ("xmin", "500")
                  , ("xmax", "3000")
                  , ("yvar", "Squark")
                  , ("ymin", "500") 
                  , ("ymax", "3000")
                  , ("datname", "xqld_neutLOSP300.0_sqsg_8TeV_1lep")
                  ] 
     
      
      cmbdstr = (toString . flip setManyAttrib cmbdtmpl)      
                  [ ("xvar", "Gluino") 
                  , ("xmin", "500")
                  , ("xmax", "3000")
                  , ("yvar", "Squark")
                  , ("ymin", "500") 
                  , ("ymax", "3000")
                  , ("datnameA", "xqld_neutLOSP300.0_sqsg_8TeV_1lep")
                  , ("datnameB", "simplifiedsusy300.0_0lep_sqsg_8TeV")
                  , ("modelaliasA", "Xqld")
                  , ("modelaliasB", "Simplified")
                  , ("figurefilename", "xqld_neutLOSP300_1lep_sqsgscan")
                  ] 

  writeFile "contour_1lep_sqsg_simplifiedsusy300.gpl" simplstr
  writeFile "contour_1lep_sqsg_xqld_neutLOSP300.gpl" xqldstr
  writeFile "combined_1lep_sqsg_xqld_neutLOSP300.gpl" cmbdstr


main_xqld_neutlosp100_1lep :: IO ()
main_xqld_neutlosp100_1lep = do 
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
                   , ("datname", "simplifiedsusy100_0lep_sqsg_8TeV")
                   ]

      
      xqldstr = (toString . flip setManyAttrib cntrtmpl) 
                  [ ("xvar", "Gluino") 
                  , ("xmin", "500")
                  , ("xmax", "3000")
                  , ("yvar", "Squark")
                  , ("ymin", "500") 
                  , ("ymax", "3000")
                  , ("datname", "xqld_neutLOSP100.0_sqsg_8TeV_1lep")
                  ] 
     
      
      cmbdstr = (toString . flip setManyAttrib cmbdtmpl)      
                  [ ("xvar", "Gluino") 
                  , ("xmin", "500")
                  , ("xmax", "3000")
                  , ("yvar", "Squark")
                  , ("ymin", "500") 
                  , ("ymax", "3000")
                  , ("datnameA", "xqld_neutLOSP100.0_sqsg_8TeV_1lep")
                  , ("datnameB", "simplifiedsusy100_0lep_sqsg_8TeV")
                  , ("modelaliasA", "Xqld")
                  , ("modelaliasB", "Simplified")
                  , ("figurefilename", "xqld_neutLOSP100_1lep_sqsgscan")
                  ] 

  writeFile "contour_1lep_sqsg_simplifiedsusy100.gpl" simplstr
  writeFile "contour_1lep_sqsg_xqld_neutLOSP100.gpl" xqldstr
  writeFile "combined_1lep_sqsg_xqld_neutLOSP100.gpl" cmbdstr


main_xqld_neutlosp100_0lep :: IO ()
main_xqld_neutlosp100_0lep = do 
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
                   , ("datname", "sim0_neut100.0_sqsg_8TeV_0lep_NLO")
                   ]

      
      xqldstr = (toString . flip setManyAttrib cntrtmpl) 
                  [ ("xvar", "Gluino") 
                  , ("xmin", "500")
                  , ("xmax", "3000")
                  , ("yvar", "Squark")
                  , ("ymin", "500") 
                  , ("ymax", "3000")
                  , ("datname", "xqld_neutLOSP100_sqsg_8TeV_0lep_NLO")
                  ] 
     
      
      cmbdstr = (toString . flip setManyAttrib cmbdtmpl)      
                  [ ("xvar", "Gluino") 
                  , ("xmin", "500")
                  , ("xmax", "3000")
                  , ("yvar", "Squark")
                  , ("ymin", "500") 
                  , ("ymax", "3000")
                  , ("datnameA", "xqld_neutLOSP100_sqsg_8TeV_0lep_NLO")
                  , ("datnameB", "sim0_neut100.0_sqsg_8TeV_0lep_NLO")
                  , ("modelaliasA", "Xqld")
                  , ("modelaliasB", "Simplified")
                  , ("figurefilename", "xqld_neutLOSP100_0lep_sqsgscan_NLO")
                  ] 

  writeFile "contour_0lep_sqsg_simplifiedsusy100.gpl" simplstr
  writeFile "contour_0lep_sqsg_xqld_neutLOSP100.gpl" xqldstr
  writeFile "combined_0lep_sqsg_xqld_neutLOSP100.gpl" cmbdstr



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
                   , ("datname", "sim0_neut10_sqsg_8TeV_0lep_NLO" ) -- "simplifiedsusy_sqsg_0lep_8TeV")
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
                  , ("datnameB", "sim0_neut10_sqsg_8TeV_0lep_NLO" ) -- "simplifiedsusy_sqsg_0lep_8TeV")
                  , ("modelaliasA", "Xqld")
                  , ("modelaliasB", "Simplified")
                  , ("figurefilename", "xqld_squarklosp_0lep_sqsgscan")
                  ] 

  writeFile "contour_0lep_sqsg_simplifiedsusy.gpl" simplstr
  writeFile "contour_0lep_sqsg_xqld_squarklosp.gpl" xqldstr
  writeFile "combined_0lep_sqsg_xqld_squarklosp.gpl" cmbdstr

main_xqld_squarklosp_1lep :: IO ()
main_xqld_squarklosp_1lep = do 
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
                  , ("datname", "xqld_squarklosp_sqsgscan_1lep_8TeV")
                  ] 
     
      
      cmbdstr = (toString . flip setManyAttrib cmbdtmpl)      
                  [ ("xvar", "Gluino") 
                  , ("xmin", "500")
                  , ("xmax", "3000")
                  , ("yvar", "Squark")
                  , ("ymin", "500") 
                  , ("ymax", "3000")
                  , ("datnameA", "xqld_squarklosp_sqsgscan_1lep_8TeV")
                  , ("datnameB", "simplifiedsusy_sqsg_0lep_8TeV")
                  , ("modelaliasA", "Xqld")
                  , ("modelaliasB", "Simplified")
                  , ("figurefilename", "xqld_squarklosp_1lep_sqsgscan")
                  ] 

  writeFile "contour_1lep_sqsg_simplifiedsusy.gpl" simplstr
  writeFile "contour_1lep_sqsg_xqld_squarklosp.gpl" xqldstr
  writeFile "combined_1lep_sqsg_xqld_squarklosp.gpl" cmbdstr



main_xudd_squarklosp :: IO ()
main_xudd_squarklosp = do 
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

      
      xuddstr = (toString . flip setManyAttrib cntrtmpl) 
                  [ ("xvar", "Gluino") 
                  , ("xmin", "500")
                  , ("xmax", "3000")
                  , ("yvar", "Squark")
                  , ("ymin", "500") 
                  , ("ymax", "3000")
                  , ("datname", "xudd_squarklosp_sqsgscan_0lep_8TeV")
                  ] 
     
      
      cmbdstr = (toString . flip setManyAttrib cmbdtmpl)      
                  [ ("xvar", "Gluino") 
                  , ("xmin", "500")
                  , ("xmax", "3000")
                  , ("yvar", "Squark")
                  , ("ymin", "500") 
                  , ("ymax", "3000")
                  , ("datnameA", "xudd_squarklosp_sqsgscan_0lep_8TeV")
                  , ("datnameB", "simplifiedsusy_sqsg_0lep_8TeV")
                  , ("modelaliasA", "Xudd")
                  , ("modelaliasB", "Simplified")
                  , ("figurefilename", "xudd_squarklosp_0lep_sqsgscan")
                  ] 

  writeFile "contour_0lep_sqsg_simplifiedsusy.gpl" simplstr
  writeFile "contour_0lep_sqsg_xudd_squarklosp.gpl" xuddstr
  writeFile "combined_0lep_sqsg_xudd_squarklosp.gpl" cmbdstr




