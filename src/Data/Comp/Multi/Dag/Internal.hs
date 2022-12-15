{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
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

module Data.Comp.Multi.Dag.Internal where

import Data.Comp.Multi.Term
import Data.Comp.Multi.HFunctor
import qualified Data.Dependent.Map as M
import Data.GADT.Compare
import Data.Type.Equality
import Unsafe.Coerce

-- | The type of node in a 'Dag'.

type Node = K Int


-- | Warning!  this GEq instance is not type-safe!  Do not use it!
instance GEq Node where
    geq (K a) (K b) = if a==b then Just $ unsafeCoerce Refl else Nothing

-- | Warning!  this GCompare instance is not type-safe!  Do not use it!
instance GCompare Node where
    gcompare (K a) (K b) = case compare a b of 
        LT -> GLT
        EQ -> unsafeCoerce (GEQ :: GOrdering () ())
        GT -> GGT


-- | The type of the compact edge representation used in a 'Dag'.

type Edges f = M.DMap Node (f (Context f Node)) 

-- | The type of directed acyclic graphs (DAGs). 'Dag's are used as a
-- compact representation of 'Term's.

data Dag f i = Dag 
    { root      :: f (Context f Node) i -- ^ the entry point for the DAG
    , edges     :: Edges f           -- ^ the edges of the DAG
    , nodeCount :: Int                -- ^ the total number of nodes in the DAG
    }
