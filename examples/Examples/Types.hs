{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TemplateHaskell   #-}

module Examples.Types where


import Data.Comp.Term
import Data.Comp.Dag
import Data.Comp.Derive
import System.IO.Unsafe



data IntTreeF a = Leaf Int | Node a a
  deriving (Eq, Show, Functor, Foldable, Traversable)

$(derive [smartConstructors] [''IntTreeF])


it1 :: Term IntTreeF
it1 = iNode (iNode x (iLeaf 10)) x
    where x = iNode y y
          y = iLeaf 20

i1 :: Dag IntTreeF
i1 = unsafePerformIO $ reifyDag it1

--     [ (0, Node 1 2)
--     , (1, Node 2 3)
--     , (2, Node 4 4)
--     , (3, Leaf 10)
--     , (4, Leaf 20)
--     ]


it2 :: Term IntTreeF
it2 = iNode x (iNode (iLeaf 5) x)
    where x = iNode (iNode (iLeaf 24) (iLeaf 3)) (iLeaf 4)

i2 :: Dag IntTreeF
i2 = unsafePerformIO $ reifyDag it2

--     [ (0, Node 2 1)
--     , (1, Node 4 2)
--     , (2, Node 3 5)
--     , (3, Node 6 7)
--     , (4, Leaf 5)
--     , (5, Leaf 4)
--     , (6, Leaf 24)
--     , (7, Leaf 3)
--     ]

