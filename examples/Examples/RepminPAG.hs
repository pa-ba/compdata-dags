{-# LANGUAGE TypeOperators  #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}

-- This is an implementation of repmin as a PAG. The use of a PAG
-- allows us to implement repmin such that the result of repmin is a
-- DAG with only one leaf node, which is shared throughout the
-- DAG. This is achieved as follows: instead of only collecting the
-- minimum synthesised attribute and then turning it into an inherited
-- attribute, which propagates the minimum to the leaves of the graph,
-- we construct a single leaf node with the minimum labelling and
-- propagate it downwards as an inherited attribute.

module Examples.RepminPAG where

import Data.Comp.PAG
import Data.Comp.Dag
import qualified Data.Comp.Dag.PAG as Dag
import Data.Comp.Term
import Examples.Types
import Data.Comp.Multi.HFunctor 

import Data.Foldable


newtype MinS a = MinS {unMinS :: Int} deriving (Eq,Ord,Functor, Foldable, Traversable)
newtype MinI a = MinI a deriving (Functor, Foldable, Traversable)


minS ::  Syn IntTreeF atts MinS f
minS (Leaf i)    =  MinS i
minS (Node a b)  =  MinS $ min (unMinS $ below a) (unMinS $ below b)

minI :: Inh IntTreeF atts MinI f
minI _ = empty

rep ::  (MinI :< atts) => Syn IntTreeF atts I IntTreeF
rep (Leaf _)    =  let MinI n = above in I (Hole n)
rep (Node a b)  =  I $ iNode (Hole $ unI $ below a) (Hole $ unI $ below b)


repminG :: Dag IntTreeF -> Dag IntTreeF
repminG = unI . fsnd . Dag.runPAG const (minS |*| rep) minI  init
  where init (MinS i :*: _) = MinI (iLeaf i)


repmin :: Term IntTreeF -> Term IntTreeF
repmin = unI . fsnd . runPAG (minS |*| rep) minI  init
  where init (MinS i :*: _) = MinI (iLeaf i)

