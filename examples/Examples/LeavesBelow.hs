{-# LANGUAGE TypeOperators  #-}
{-# LANGUAGE ImplicitParams #-}


module Examples.LeavesBelow where

import Data.Comp.AG
import Data.Comp.Dag
import qualified Data.Comp.Dag.AG as Dag
import Data.Comp.Term
import Examples.Types
import Data.Set (Set)
import qualified Data.Set as Set


leavesBelowI :: Inh IntTreeF atts Int
leavesBelowI (Leaf _)      = empty
leavesBelowI (Node t1 t2)  = t1 |-> d' & t2 |-> d'
            where d' = above - 1

leavesBelowS :: (Int :< atts) => Syn IntTreeF atts (Set Int)
leavesBelowS (Leaf i)
    | (above :: Int) <= 0  =  Set.singleton i
    | otherwise            =  Set.empty
leavesBelowS (Node t1 t2)  =  below t1 `Set.union` below t2


-- | As AG on terms
leavesBelow :: Int -> Term IntTreeF -> Set Int
leavesBelow d = runAG leavesBelowS leavesBelowI (const d)

-- | As AG on dags
leavesBelowG :: Int -> Dag IntTreeF -> Set Int
leavesBelowG d = Dag.runAG min leavesBelowS leavesBelowI (const d)
