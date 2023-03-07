{-# LANGUAGE TypeOperators  #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Examples.Multi.Repmin where

import Data.Comp.Multi.AG
import Data.Comp.Multi.Dag
import qualified Data.Comp.Multi.Dag.AG as Dag
import Data.Comp.Multi.Term
import Data.Comp.Multi.HFunctor
import Data.Comp.Projection
import Examples.Multi.Types

newtype MinS = MinS Int deriving (Eq,Ord)
newtype MinI = MinI Int


globMin :: (?above :: atts, MinI :< atts) => Int
globMin = let MinI i = above in i

minS ::  Syn IntTreeF atts MinS
minS (Leaf i)    =  MinS i
minS (TreeNode a b)  =  min (below a) (below b)

minI :: Inh IntTreeF atts MinI
minI _ = empty


-- | Repmin as a rewriting AG on dags.

repminG' :: Dag IntTreeF :-> Dag IntTreeF
repminG' = snd . Dag.runRewrite const minS minI rep' init
  where init (MinS i) = MinI i

rep' ::  (MinI :< atts) => Rewrite IntTreeF atts IntTreeF
rep' (Leaf _)    =  iLeaf globMin
rep' (TreeNode a b)  =  iTreeNode (Hole a) (Hole b)

repmin' :: Term IntTreeF :-> Term IntTreeF
repmin' = snd . runRewrite minS minI rep' init
  where init (MinS i) = MinI i
