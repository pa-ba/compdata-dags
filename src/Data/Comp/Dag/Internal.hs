

module Data.Comp.Dag.Internal where

import Data.Comp.Term
import Data.IntMap (IntMap)

type Node = Int
data Dag f = Dag { root      :: f (Context f Node)
                 , edges     :: IntMap (f (Context f Node))
                 , nodeCount :: Int }
