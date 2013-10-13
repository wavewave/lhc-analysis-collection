import System.Environment
import System.Directory
import Text.StringTemplate

main = main_xudd_neutlosp100_0lep

-- main_xudd_squarklosp_0lep

-- main_xqld_neutlosp500_1lep

-- main_xqld_squarklosp_1lep 




 
-- main_xqld_squarklosp_1lep





-- main_xqld_neutlosp500_0lep

-- main_xqld_squarklosp




-- 

 -- -- main_xudd_squarklosp -- main_xqld_squarklosp

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
                   , ("datname", "sim0_neut100.0_sqsg_8TeV_0lep_NLO")
                   ]

      
      xuddstr = (toString . flip setManyAttrib cntrtmpl) 
                  [ ("xvar", "Gluino") 
                  , ("xmin", "400")
                  , ("xmax", "3000")
                  , ("yvar", "Squark")
                  , ("ymin", "400") 
                  , ("ymax", "3000")
                  , ("datname", "xudd_neutLOSP100_sqsg_8TeV_0lep_NLO")
                  ] 
     
      
      cmbdstr = (toString . flip setManyAttrib cmbdtmpl)      
                  [ ("xvar", "Gluino") 
                  , ("xmin", "400")
                  , ("xmax", "3000")
                  , ("yvar", "Squark")
                  , ("ymin", "400") 
                  , ("ymax", "3000")
                  , ("datnameA", "xudd_neutLOSP100_sqsg_8TeV_0lep_NLO")
                  , ("datnameB", "sim0_neut100.0_sqsg_8TeV_0lep_NLO")
                  , ("modelaliasA", "UDD")
                  , ("modelaliasB", "Sim0")
                  , ("figurefilename", "xudd_neutLOSP100_0lep_sqsgscan_NLO")
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
                   , ("datname", "sim0_neut300_sqsg_8TeV_0lep_NLO")
                   ]

      
      xuddstr = (toString . flip setManyAttrib cntrtmpl) 
                  [ ("xvar", "Gluino") 
                  , ("xmin", "500")
                  , ("xmax", "3000")
                  , ("yvar", "Squark")
                  , ("ymin", "500") 
                  , ("ymax", "3000")
                  , ("datname", "xudd_neutLOSP300_sqsg_8TeV_0lep_NLO")
                  ] 
     
      
      cmbdstr = (toString . flip setManyAttrib cmbdtmpl)      
                  [ ("xvar", "Gluino") 
                  , ("xmin", "500")
                  , ("xmax", "3000")
                  , ("yvar", "Squark")
                  , ("ymin", "500") 
                  , ("ymax", "3000")
                  , ("datnameA", "xudd_neutLOSP300_sqsg_8TeV_0lep_NLO")
                  , ("datnameB", "sim0_neut300_sqsg_8TeV_0lep_NLO")
                  , ("modelaliasA", "UDD")
                  , ("modelaliasB", "Sim0")
                  , ("figurefilename", "xudd_neutLOSP300_0lep_sqsgscan_NLO")
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
                   , ("datname", "sim0_neut500_sqsg_8TeV_0lep_NLO")
                   ]

      
      xuddstr = (toString . flip setManyAttrib cntrtmpl) 
                  [ ("xvar", "Gluino") 
                  , ("xmin", "500")
                  , ("xmax", "3000")
                  , ("yvar", "Squark")
                  , ("ymin", "500") 
                  , ("ymax", "3000")
                  , ("datname", "xudd_neutLOSP500_sqsg_8TeV_0lep_NLO")
                  ] 
     
      
      cmbdstr = (toString . flip setManyAttrib cmbdtmpl)      
                  [ ("xvar", "Gluino") 
                  , ("xmin", "500")
                  , ("xmax", "3000")
                  , ("yvar", "Squark")
                  , ("ymin", "500") 
                  , ("ymax", "3000")
                  , ("datnameA", "xudd_neutLOSP500_sqsg_8TeV_0lep_NLO")
                  , ("datnameB", "sim0_neut500_sqsg_8TeV_0lep_NLO")
                  , ("modelaliasA", "UDD")
                  , ("modelaliasB", "Sim0")
                  , ("figurefilename", "xudd_neutLOSP500_0lep_sqsgscan_NLO")
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
                   , ("datname", "sim0_neut500_sqsg_8TeV_0lep_NLO")
                   ]

      
      xqldstr = (toString . flip setManyAttrib cntrtmpl) 
                  [ ("xvar", "Gluino") 
                  , ("xmin", "500")
                  , ("xmax", "3000")
                  , ("yvar", "Squark")
                  , ("ymin", "500") 
                  , ("ymax", "3000")
                  , ("datname", "xqld_neutLOSP500_sqsg_8TeV_0lep_NLO")
                  ] 
     
      
      cmbdstr = (toString . flip setManyAttrib cmbdtmpl)      
                  [ ("xvar", "Gluino") 
                  , ("xmin", "500")
                  , ("xmax", "3000")
                  , ("yvar", "Squark")
                  , ("ymin", "500") 
                  , ("ymax", "3000")
                  , ("datnameA", "xqld_neutLOSP500_sqsg_8TeV_0lep_NLO")
                  , ("datnameB", "sim0_neut500_sqsg_8TeV_0lep_NLO")
                  , ("modelaliasA", "QLD")
                  , ("modelaliasB", "Sim0")
                  , ("figurefilename", "xqld_neutLOSP500_0lep_sqsgscan_NLO")
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
                   , ("datname", "sim0_neut500_sqsg_8TeV_0lep_NLO")
                   ]

      
      xqldstr = (toString . flip setManyAttrib cntrtmpl) 
                  [ ("xvar", "Gluino") 
                  , ("xmin", "500")
                  , ("xmax", "3000")
                  , ("yvar", "Squark")
                  , ("ymin", "500") 
                  , ("ymax", "3000")
                  , ("datname", "xqld_neutLOSP500_sqsg_8TeV_1lep_NLO")
                  ] 
     
      
      cmbdstr = (toString . flip setManyAttrib cmbdtmpl)      
                  [ ("xvar", "Gluino") 
                  , ("xmin", "500")
                  , ("xmax", "3000")
                  , ("yvar", "Squark")
                  , ("ymin", "500") 
                  , ("ymax", "3000")
                  , ("datnameA", "xqld_neutLOSP500_sqsg_8TeV_1lep_NLO")
                  , ("datnameB", "sim0_neut500_sqsg_8TeV_0lep_NLO")
                  , ("modelaliasA", "QLD")
                  , ("modelaliasB", "Sim0 (0lep)")
                  , ("figurefilename", "xqld_neutLOSP500_1lep_sqsgscan_NLO")
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
                   , ("datname", "sim0_neut300_sqsg_8TeV_0lep_NLO")
                   ]

      
      xqldstr = (toString . flip setManyAttrib cntrtmpl) 
                  [ ("xvar", "Gluino") 
                  , ("xmin", "500")
                  , ("xmax", "3000")
                  , ("yvar", "Squark")
                  , ("ymin", "500") 
                  , ("ymax", "3000")
                  , ("datname", "xqld_neutLOSP300_sqsg_8TeV_0lep_NLO")
                  ] 
     
      
      cmbdstr = (toString . flip setManyAttrib cmbdtmpl)      
                  [ ("xvar", "Gluino") 
                  , ("xmin", "500")
                  , ("xmax", "3000")
                  , ("yvar", "Squark")
                  , ("ymin", "500") 
                  , ("ymax", "3000")
                  , ("datnameA", "xqld_neutLOSP300_sqsg_8TeV_0lep_NLO")
                  , ("datnameB", "sim0_neut300_sqsg_8TeV_0lep_NLO")
                  , ("modelaliasA", "QLD")
                  , ("modelaliasB", "Sim0")
                  , ("figurefilename", "xqld_neutLOSP300_0lep_sqsgscan_NLO")
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
                   , ("datname", "sim0_neut300_sqsg_8TeV_0lep_NLO")
                   ]

      
      xqldstr = (toString . flip setManyAttrib cntrtmpl) 
                  [ ("xvar", "Gluino") 
                  , ("xmin", "500")
                  , ("xmax", "3000")
                  , ("yvar", "Squark")
                  , ("ymin", "500") 
                  , ("ymax", "3000")
                  , ("datname", "xqld_neutLOSP300_sqsg_8TeV_1lep_NLO")
                  ] 
     
      
      cmbdstr = (toString . flip setManyAttrib cmbdtmpl)      
                  [ ("xvar", "Gluino") 
                  , ("xmin", "500")
                  , ("xmax", "3000")
                  , ("yvar", "Squark")
                  , ("ymin", "500") 
                  , ("ymax", "3000")
                  , ("datnameA", "xqld_neutLOSP300_sqsg_8TeV_1lep_NLO")
                  , ("datnameB", "sim0_neut300_sqsg_8TeV_0lep_NLO")
                  , ("modelaliasA", "QLD")
                  , ("modelaliasB", "Sim0 (0lep)")
                  , ("figurefilename", "xqld_neutLOSP300_1lep_sqsgscan_NLO")
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
                   , ("datname", "sim0_neut100.0_sqsg_8TeV_0lep_NLO")
                   ]

      
      xqldstr = (toString . flip setManyAttrib cntrtmpl) 
                  [ ("xvar", "Gluino") 
                  , ("xmin", "500")
                  , ("xmax", "3000")
                  , ("yvar", "Squark")
                  , ("ymin", "500") 
                  , ("ymax", "3000")
                  , ("datname", "xqld_neutLOSP100_sqsg_8TeV_1lep_NLO")
                  ] 
     
      
      cmbdstr = (toString . flip setManyAttrib cmbdtmpl)      
                  [ ("xvar", "Gluino") 
                  , ("xmin", "500")
                  , ("xmax", "3000")
                  , ("yvar", "Squark")
                  , ("ymin", "500") 
                  , ("ymax", "3000")
                  , ("datnameA", "xqld_neutLOSP100_sqsg_8TeV_1lep_NLO")
                  , ("datnameB", "sim0_neut100.0_sqsg_8TeV_0lep_NLO")
                  , ("modelaliasA", "QLD")
                  , ("modelaliasB", "Sim0 (0lep)")
                  , ("figurefilename", "xqld_neutLOSP100_1lep_sqsgscan_NLO")
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
                  , ("modelaliasA", "QLD")
                  , ("modelaliasB", "Sim0")
                  , ("figurefilename", "xqld_neutLOSP100_0lep_sqsgscan_NLO")
                  ] 

  writeFile "contour_0lep_sqsg_simplifiedsusy100.gpl" simplstr
  writeFile "contour_0lep_sqsg_xqld_neutLOSP100.gpl" xqldstr
  writeFile "combined_0lep_sqsg_xqld_neutLOSP100.gpl" cmbdstr



main_xqld_squarklosp_0lep :: IO ()
main_xqld_squarklosp_0lep = do 
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
                   , ("datname", "sim0_neut10_sqsg_8TeV_0lep_NLO" ) 
                   ]
      
      xqldstr = (toString . flip setManyAttrib cntrtmpl) 
                  [ ("xvar", "Gluino") 
                  , ("xmin", "500")
                  , ("xmax", "3000")
                  , ("yvar", "Squark")
                  , ("ymin", "500") 
                  , ("ymax", "3000")
                  , ("datname", "xqld_squarkLOSP_sqsg_8TeV_0lep_NLO")
                  ] 
     
      
      cmbdstr = (toString . flip setManyAttrib cmbdtmpl)      
                  [ ("xvar", "Gluino") 
                  , ("xmin", "500")
                  , ("xmax", "3000")
                  , ("yvar", "Squark")
                  , ("ymin", "500") 
                  , ("ymax", "3000")
                  , ("datnameA", "xqld_squarkLOSP_sqsg_8TeV_0lep_NLO")
                  , ("datnameB", "sim0_neut10_sqsg_8TeV_0lep_NLO" )
                  , ("modelaliasA", "QLD")
                  , ("modelaliasB", "Sim0")
                  , ("figurefilename", "xqld_squarkLOSP_0lep_sqsgscan_NLO")
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
                   , ("datname", "sim0_neut10_sqsg_8TeV_0lep_NLO")
                   ]

      
      xqldstr = (toString . flip setManyAttrib cntrtmpl) 
                  [ ("xvar", "Gluino") 
                  , ("xmin", "500")
                  , ("xmax", "3000")
                  , ("yvar", "Squark")
                  , ("ymin", "500") 
                  , ("ymax", "3000")
                  , ("datname", "xqld_squarkLOSP_sqsg_8TeV_1lep_NLO")
                  ] 
     
      
      cmbdstr = (toString . flip setManyAttrib cmbdtmpl)      
                  [ ("xvar", "Gluino") 
                  , ("xmin", "500")
                  , ("xmax", "3000")
                  , ("yvar", "Squark")
                  , ("ymin", "500") 
                  , ("ymax", "3000")
                  , ("datnameA", "xqld_squarkLOSP_sqsg_8TeV_1lep_NLO")
                  , ("datnameB", "sim0_neut10_sqsg_8TeV_0lep_NLO")
                  , ("modelaliasA", "QLD")
                  , ("modelaliasB", "Sim0 (0lep)")
                  , ("figurefilename", "xqld_squarkLOSP_1lep_sqsgscan_NLO")
                  ] 

  writeFile "contour_1lep_sqsg_simplifiedsusy.gpl" simplstr
  writeFile "contour_1lep_sqsg_xqld_squarklosp.gpl" xqldstr
  writeFile "combined_1lep_sqsg_xqld_squarklosp.gpl" cmbdstr



main_xudd_squarklosp_0lep :: IO ()
main_xudd_squarklosp_0lep = do 
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
                   , ("datname", "sim0_neut10_sqsg_8TeV_0lep_NLO")
                   ]

      
      xuddstr = (toString . flip setManyAttrib cntrtmpl) 
                  [ ("xvar", "Gluino") 
                  , ("xmin", "500")
                  , ("xmax", "3000")
                  , ("yvar", "Squark")
                  , ("ymin", "500") 
                  , ("ymax", "3000")
                  , ("datname", "xudd_squarkLOSP_sqsg_8TeV_0lep_NLO")
                  ] 
     
      
      cmbdstr = (toString . flip setManyAttrib cmbdtmpl)      
                  [ ("xvar", "Gluino") 
                  , ("xmin", "500")
                  , ("xmax", "3000")
                  , ("yvar", "Squark")
                  , ("ymin", "500") 
                  , ("ymax", "3000")
                  , ("datnameA", "xudd_squarkLOSP_sqsg_8TeV_0lep_NLO")
                  , ("datnameB", "sim0_neut10_sqsg_8TeV_0lep_NLO")
                  , ("modelaliasA", "UDD")
                  , ("modelaliasB", "Sim0")
                  , ("figurefilename", "xudd_squarkLOSP_0lep_sqsgscan_NLO")
                  ] 

  writeFile "contour_0lep_sqsg_simplifiedsusy.gpl" simplstr
  writeFile "contour_0lep_sqsg_xudd_squarklosp.gpl" xuddstr
  writeFile "combined_0lep_sqsg_xudd_squarklosp.gpl" cmbdstr




