import Control.Applicative
import System.Directory
import System.Environment 
import System.Process
import qualified Text.StringTemplate as ST

render :: String -> [(String,String)] -> String
render tmpl attribs = (ST.render . ST.setManyAttrib attribs . ST.newSTMP) tmpl 

strtmpl = "prospino {\n  basedir = \"$basedir$\"\n  resultFileName = \"$result$\"\n  remoteDir = \"$remotedir$\"\n}\n"

m_neutralino :: Double 
m_neutralino = 100.0

minfty :: Double 
minfty = 50000.0

createRdirBName procname (mq,mg) = 
  let rdir = "montecarlo/admproject/XQLDdegen/8TeV/neutLOSP/scan_" ++ procname 
      basename = "ADMXQLD111degenMG" ++ show mg ++ "MQ" ++ show mq ++ "ML" ++ show minfty ++ "MN"++show m_neutralino ++ "_" ++ procname ++ "_LHC8ATLAS_NoMatch_NoCut_AntiKT0.4_NoTau_Set1"
  in (rdir,basename)  


datalst :: [ (Double,Double) ]
datalst = [ (q,g) | q<- [m_neutralino+100,m_neutralino+200..3000], g <- [m_neutralino+100,m_neutralino+200..3000] ]


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
      filenames = map ((,,) <$> fst <*> snd <*> createRdirBName "2sq_2l6j2x" . snd) datasublst   
      makecfg rdirbname =  render strtmpl [ ("basedir", dirname )
                                          , ("result", (snd rdirbname) ++"_xsecKfactor.json" ) 
                                          , ("remotedir", (fst rdirbname) )
                                          ]
      cfglst = map ((,,) <$> (\x-> "runprospino"++show x++".conf") . fst3  
                         <*> snd3 
                         <*> makecfg . trd3) 
                   filenames

  -- mapM_ print cfglst 
  -- error (show (length datalst)) 
 
  setCurrentDirectory dirname 
  mapM_ (\(x,_,y)->writeFile x y) cfglst 
  mapM_ (\(x,(q,g),y) -> let c = cmd cfgfile x (q,g) in print c >> system c) cfglst
  
cmd cfgfile jobfile (q,g) = "/home2/iankim/repo/src/lhc-analysis-collection/analysis/runProspino squarkpair " ++ cfgfile ++ " " ++ show q ++ " " ++ show g ++ " --config=" ++ jobfile
   
