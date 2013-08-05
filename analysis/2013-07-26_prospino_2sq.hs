import Control.Applicative
import System.Directory
import System.Environment 
import System.Process
import qualified Text.StringTemplate as ST

render :: String -> [(String,String)] -> String
render tmpl attribs = (ST.render . ST.setManyAttrib attribs . ST.newSTMP) tmpl 

strtmpl = "prospino {\n  basedir = \"$basedir$\"\n  resultFileName = \"$result$\"\n  remoteDir = \"$remotedir$\"\n}\n"



minfty :: Double 
minfty = 50000.0

createRdirBName procname (mq,mn) = 
  let rdir = "montecarlo/admproject/SimplifiedSUSYlep/8TeV/scan_" ++ procname 
      basename = "SimplifiedSUSYlepN" ++ show mn ++ "G"++show minfty ++ "QL" ++ show mq ++ "C"++show (0.5*(mn+mq))++ "L" ++ show minfty ++ "NN" ++ show minfty ++ "_" ++ procname ++ "_LHC8ATLAS_NoMatch_NoCut_AntiKT0.4_NoTau_Set1"
  in (rdir,basename)  

datalst :: [ (Double,Double) ]
datalst =  [ (850.0,750.0) ]

-- datalst = [ (mg,mn) | mg <- [ 200,250..1500 ], mn <- [ 50,100..mg-50] ]
-- datalst = [ (mq,mn) | mq <- [ 200,250..1300], mn <- [ 50,100..mq-50 ] ] 
-- datalst = [ (1300,300) ]

fst3 (a,_,_) = a
snd3 (_,a,_) = a
trd3 (_,_,a) = a

main :: IO ()
main = do 
  args <- getArgs
  let cfgfile = args !! 0  
      dirname = args !! 1
      n1 = read (args !! 2) :: Int 
      n2 = read (args !! 3) :: Int 
      datasublst = (zip [1..] . drop (n1-1) . take n2) datalst
      filenames = map ((,,) <$> fst <*> snd <*> createRdirBName "1step_2sq" . snd) datasublst   
      makecfg rdirbname =  render strtmpl [ ("basedir", dirname )
                                          , ("result", (snd rdirbname) ++"_xsecKfactor.json" ) 
                                          , ("remotedir", (fst rdirbname) )
                                          ]
      cfglst = map ((,,) <$> (\x-> "runprospino"++show x++".conf") . fst3  
                         <*> snd3 
                         <*> makecfg . trd3) 
                   filenames

  -- (error . show)  (length datalst)
  -- mapM_ print cfglst 

 
  setCurrentDirectory dirname 
  mapM_ (\(x,_,y)->writeFile x y) cfglst 
  mapM_ (\(x,(g,n),y) -> let c = cmd cfgfile x (g,n) in print c >> system c) cfglst
  
cmd cfgfile jobfile (q,n) = "/home2/iankim/repo/src/lhc-analysis-collection/analysis/runProspino squarkpair " ++ cfgfile ++ " " ++ show q ++ " " ++ show minfty ++ " --config=" ++ jobfile
   
