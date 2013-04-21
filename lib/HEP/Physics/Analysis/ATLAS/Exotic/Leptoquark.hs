{-# LANGUAGE RecordWildCards #-}

-----------------------------------------------------------------------------
-- |
-- Module      : HEP.Physics.Analysis.ATLAS.Exotic.Leptoquark
-- Copyright   : (c) 2013 Ian-Woo Kim
--
-- License     : GPL-3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-- LHC ATLAS Exotic/Leptoquark analysis code
-- 
-- 
-- 
-----------------------------------------------------------------------------

module HEP.Physics.Analysis.ATLAS.Exotic.Leptoquark where 

import Control.Applicative ((<$>))
import Control.Monad (MonadPlus(..))
import Control.Monad.Indexed (ireturn,(>>>=))
import Control.Monad.Indexed.State (IxStateT(..),imodify,iget)
-- 
import HEP.Parser.LHCOAnalysis.PhysObj (PhyEventClassified(..),pt)
-- 
import HEP.Physics.Analysis.ATLAS.Common 


twoelectron :: JetMergedEv -> Bool 
twoelectron (JetMerged PhyEventClassified {..}) = length electronlst >= 2  


classifyM :: MonadPlus m => JESParam -> IxStateT m RawEv JetMergedEv Double 
classifyM jes = 
    imodify taubjetMergeIx >>> 
    iget >>>= \ev@(JetMerged pev) -> 
    iguard (twoelectron ev) >>>
    (ireturn . pt . snd . head . electronlst) pev 

-- (ireturn "hello")

classify :: (Functor m, MonadPlus m) => JESParam -> PhyEventClassified -> m Double
classify jes ev = fst <$> runIxStateT (classifyM jes) (Raw ev)
