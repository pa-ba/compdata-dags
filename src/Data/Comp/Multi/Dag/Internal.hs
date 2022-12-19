{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE GADTs #-}
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
import Data.Comp.Multi.Derive
import qualified Data.Dependent.Map as M
import Data.GADT.Compare
import Data.Type.Equality
import Data.Typeable
import Data.Bifunctor
import Unsafe.Coerce

-- | The type of node in a 'Dag'.

data Node :: * -> * where
    Node :: Typeable t =>  Int -> Node t

getNode :: Node t -> Int
getNode (Node i) = i

instance Typeable t => Num (Node t) where
    fromInteger = Node . fromInteger
    (+) = fmap fmap fmap Node . (. getNode) $ (. getNode) . (+)
    (*) = fmap fmap fmap Node . (. getNode) $ (. getNode) . (*)
    abs = Node . abs . getNode
    signum = Node . signum . getNode
    negate = Node . negate . getNode

instance Typeable t => Ord (Node t) where
    compare = (. getNode) $ (. getNode) . compare

instance Typeable t => Real (Node t) where
    toRational = toRational . getNode

instance Typeable t => Enum (Node t) where
    toEnum = Node . toEnum
    fromEnum = fromEnum . getNode

instance Typeable t => Integral (Node t) where
    quotRem = fmap fmap fmap (bimap Node Node) . (. getNode) $ (. getNode) . quotRem
    toInteger = toInteger . getNode

instance GEq Node where
    geq a@(Node i) b@(Node j) = if i == j && typeOf a == typeOf b then Just $ unsafeCoerce Refl else Nothing

instance Eq (Node k) where a==b = case a `geq` b of Nothing -> False
                                                    Just Refl -> True

instance KShow Node where
    kshow (Node i) = K $ show i

instance GCompare Node where
    gcompare a@(Node i) b@(Node j) = case compare i j of
        LT -> GLT
        EQ -> case compare (typeOf a) (typeOf b) of
                LT -> GLT
                EQ -> unsafeCoerce (GEQ :: GOrdering () ())
                GT -> GGT
        GT -> GGT


-- | The type of the compact edge representation used in a 'Dag'.

type Edges f = M.DMap Node (f (Context f Node))

-- | The type of directed acyclic graphs (DAGs). 'Dag's are used as a
-- compact representation of 'Term's.

data Dag f i where
    Dag :: (Typeable f, Typeable i) =>
       f (Context f Node) i     -- ^ the entry point for the DAG
        -> Edges f              -- ^ the edges of the DAG
        -> Int                  -- ^ the total number of nodes in the DAG
        -> Dag f i

root :: Dag f i -> f (Context f Node) i
root (Dag c _ _) = c

edges :: Dag f i -> Edges f
edges (Dag _ e _) = e

nodeCount :: Dag f i -> Int
nodeCount (Dag _ _ n) = n
