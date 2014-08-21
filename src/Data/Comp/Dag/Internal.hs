

module Data.Comp.Dag.Internal where

import Data.Comp.Term
import Data.IntMap (IntMap)

type Node = Int
type Edges f = IntMap (f (Context f Node))


data Dag f = Dag { root      :: f (Context f Node)
                 , edges     :: Edges f
                 , nodeCount :: Int }
