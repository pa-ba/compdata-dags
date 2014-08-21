{-# LANGUAGE TypeOperators  #-}
{-# LANGUAGE ImplicitParams #-}


module Examples.Repmin where

import Data.Comp.AG
import Data.Comp.Dag
import qualified Data.Comp.Dag.AG as Dag
import Data.Comp.Term
import Examples.Types

newtype MinS = MinS Int deriving (Eq,Ord)
newtype MinI = MinI Int

-- | Repmin as an AG on terms.

repmin :: Term IntTreeF -> Term IntTreeF
repmin = snd . runAG (minS |*| rep) minI init
  where init (MinS i,_) = MinI i

-- | Repmin as an AG on dags.

repminG :: Dag IntTreeF -> Term IntTreeF
repminG =  snd . Dag.runAG const (minS |*| rep) minI init
  where init (MinS i,_) = MinI i


globMin :: (?above :: atts, MinI :< atts) => Int
globMin = let MinI i = above in i

minS ::  Syn IntTreeF atts MinS
minS (Leaf i)    =  MinS i
minS (Node a b)  =  min (below a) (below b)

minI :: Inh IntTreeF atts MinI
minI _ = empty

rep ::  (MinI :< atts) => Syn IntTreeF atts (Term IntTreeF)
rep (Leaf _)    =  iLeaf globMin
rep (Node a b)  =  iNode (below a) (below b)


-- | Repmin as a rewriting AG on dags.

repminG' :: Dag IntTreeF -> Dag IntTreeF
repminG' = snd . Dag.runRewrite const minS minI rep' init
  where init (MinS i) = MinI i

rep' ::  (MinI :< atts) => Rewrite IntTreeF atts IntTreeF
rep' (Leaf _)    =  iLeaf globMin
rep' (Node a b)  =  iNode (Hole a) (Hole b)

repmin' :: Term IntTreeF -> Term IntTreeF
repmin' = snd . runRewrite minS minI rep' init
  where init (MinS i) = MinI i
