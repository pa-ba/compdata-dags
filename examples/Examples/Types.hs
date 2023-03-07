{-# LANGUAGE DeriveFoldable     #-}
{-# LANGUAGE DeriveFunctor      #-}
{-# LANGUAGE DeriveTraversable  #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE FlexibleContexts   #-}

module Examples.Types where


import Data.Comp.Term
import Data.Comp.Dag
import Data.Comp.Derive
import System.IO.Unsafe



data IntTreeF a = Leaf Int | TreeNode a a
  deriving (Eq, Show, Functor, Foldable, Traversable)

$(derive [smartConstructors, makeShowF, makeEqF] [''IntTreeF])


-- Example terms and dags

it1 :: Term IntTreeF
it1 = iTreeNode (iTreeNode x (iLeaf 10)) x
    where x = iTreeNode y y
          y = iLeaf 20

i1 :: Dag IntTreeF
i1 = unsafePerformIO $ reifyDag it1

--     [ (0, TreeNode 1 2)
--     , (1, TreeNode 2 3)
--     , (2, TreeNode 4 4)
--     , (3, Leaf 10)
--     , (4, Leaf 20)
--     ]


it2 :: Term IntTreeF
it2 = iTreeNode x (iTreeNode (iLeaf 5) x)
    where x = iTreeNode (iTreeNode (iLeaf 24) (iLeaf 3)) (iLeaf 4)

i2 :: Dag IntTreeF
i2 = unsafePerformIO $ reifyDag it2

--     [ (0, TreeNode 2 1)
--     , (1, TreeNode 4 2)
--     , (2, TreeNode 3 5)
--     , (3, TreeNode 6 7)
--     , (4, Leaf 5)
--     , (5, Leaf 4)
--     , (6, Leaf 24)
--     , (7, Leaf 3)
--     ]

