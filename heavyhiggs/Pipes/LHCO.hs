module Pipes.LHCO where

import qualified Codec.Zlib as Zlib
import qualified Data.Text as T
import           Pipes
import           Pipes.Attoparsec as PA
import           Pipes.ByteString as PB
import qualified Pipes.Parse as PP
import qualified Pipes.Zlib as PZ
--
import HEP.Parser.LHCOAnalysis.PhysObj
import HEP.Parser.LHCOAnalysis.Parse

gunzip hin = (PZ.decompress (Zlib.WindowBits 31) (PB.fromHandle hin))

pipesLHCOEvent :: (Monad m) => Producer T.Text m r -> Producer PhyEventClassified m (Either (ParsingError, Producer T.Text m r) r)
pipesLHCOEvent = parsed (header >> event) 
