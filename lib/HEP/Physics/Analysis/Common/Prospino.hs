{-# LANGUAGE DeriveDataTypeable #-}

-----------------------------------------------------------------------------
-- |
-- Module      : HEP.Physics.Analysis.Common.Prospino
-- Copyright   : (c) 2013 Ian-Woo Kim
--
-- License     : GPL-3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-- Get cross section and total number from ME/PS matched events
-- 
-----------------------------------------------------------------------------
 
module HEP.Physics.Analysis.Common.Prospino where

import qualified Data.Aeson.Generic as G
import Data.Aeson.Types
import Data.Data


data CrossSectionResult = CrossSectionResult { xsecRefLO :: Double 
                                             , xsecRefNLO :: Double
                                             , xsecRefMultiSquarkLO :: Double
                                             , xsecRefMultiSquarkNLO :: Double           
                                             , xsecKFactor :: Double 
                                             }
                        deriving (Show, Eq, Data, Typeable)

instance ToJSON CrossSectionResult where
  toJSON = G.toJSON 
