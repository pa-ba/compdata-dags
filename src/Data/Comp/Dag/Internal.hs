--------------------------------------------------------------------------------
-- |
-- Module      :  Data.Comp.Dag.Internal
-- Copyright   :  (c) 2014 Patrick Bahr, Emil Axelsson
-- License     :  BSD3
-- Maintainer  :  Patrick Bahr <paba@di.ku.dk>
-- Stability   :  experimental
-- Portability :  non-portable (GHC Extensions)
--
-- This module defines the types for representing DAGs. However,
-- 'Dag's should only be constructed using the interface provided by
-- "Data.Comp.Dag".
--
--------------------------------------------------------------------------------

module Data.Comp.Dag.Internal where

import Data.Comp.Term
import Data.IntMap (IntMap)

type Node = Int
type Edges f = IntMap (f (Context f Node))


data Dag f = Dag { root      :: f (Context f Node)
                 , edges     :: Edges f
                 , nodeCount :: Int }
