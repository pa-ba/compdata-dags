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
import Data.GADT.Show
import Data.Type.Equality
import Data.Typeable
import Data.Bifunctor
import Unsafe.Coerce

-- | The type of node in a 'Dag'.

data Node :: * -> * where
    Node :: Typeable t =>  {getNode :: Int} -> Node t

instance Typeable t => Num (Node t) where
    fromInteger = Node . fromInteger
    (+) = fmap fmap fmap Node $ (. getNode) . (+) . getNode
    (*) = fmap fmap fmap Node $ (. getNode) . (*) . getNode
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

instance KEq Node where
    n `keq` m = case n `geq` m of Just _  -> True
                                  Nothing -> False

instance Eq (Node k) where a==b = case a `geq` b of Nothing -> False
                                                    Just Refl -> True

instance KShow Node where
    kshow (Node i) = K $ show i

instance GShow Node where
    gshowsPrec _i n@(Node j) str = show j ++ show (typeOf n) ++ str

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
      {
        root      :: f (Context f Node) i     -- ^ the entry point for the DAG
      , edges     :: Edges f                  -- ^ the edges of the DAG
      , nodeCount :: Int                      -- ^ the total number of nodes in the DAG
      } -> Dag f i

type Edges' f = M.DMap Node (f Node)

-- | Flat dags.
data Dag' f i where
    Dag' :: (Typeable f, Typeable i) =>
      {
        root' :: f Node i              -- ^ the entry point for the DAG
      , edges' ::  Edges' f             -- ^ the edges of the DAG
      , nodeCount' :: Int               -- ^ the total number of nodes in the DAG
      } -> Dag' f i
