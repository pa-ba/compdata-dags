{-# LANGUAGE TypeOperators  #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}


module Examples.Repmin where

import Data.Comp.PAG
import Data.Comp.Dag
import qualified Data.Comp.Dag.PAG as Dag
import Data.Comp.Term
import Examples.Types
import Data.Comp.Multi.HFunctor 

import Data.Foldable


newtype MinS a = MinS {unMinS :: Int} deriving (Eq,Ord,Functor, Foldable, Traversable)
newtype MinI a = MinI Int deriving (Functor, Foldable, Traversable)


globMin :: (?above :: atts a, MinI :< atts) => Int
globMin = let MinI i = above in i

minS ::  Syn IntTreeF atts MinS f
minS (Leaf i)    =  MinS i
minS (Node a b)  =  MinS $ min (unMinS $ below a) (unMinS $ below b)

minI :: Inh IntTreeF atts MinI f
minI _ = empty

rep ::  (MinI :< atts) => Syn IntTreeF atts I IntTreeF
rep (Leaf _)    =  I $ iLeaf globMin
rep (Node a b)  =  I $ iNode (Hole $ unI $ below a) (Hole $ unI $ below b)




repminG :: Dag IntTreeF -> Dag IntTreeF
repminG = unI . fsnd . Dag.runPAG const (minS |*| rep) minI  init
  where init (MinS i :*: _) = MinI i


repmin :: Term IntTreeF -> Term IntTreeF
repmin = unI . fsnd . runPAG (minS |*| rep) minI  init
  where init (MinS i :*: _) = MinI i

