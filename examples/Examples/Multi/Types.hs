{-# LANGUAGE DeriveTraversable  #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Examples.Multi.Types where


import Data.Comp.Multi.Term
import Data.Comp.Multi.Dag
import Data.Comp.Multi.Derive
import System.IO.Unsafe
import Unsafe.Coerce


import Data.GADT.Compare
import Data.Type.Equality

data IntTreeF a i where
    Leaf :: Int -> IntTreeF a Int
    Node :: a i -> a j -> IntTreeF a (i,j)

instance GEq (IntTreeF (Term IntTreeF)) where
    (Leaf x) `geq` (Leaf y) = if x==y then Just Refl else Nothing
    (Node (Term x) (Term x')) `geq` (Node (Term y) (Term y')) = do x `geq` y
                                                                   x' `geq` y'
                                                                   return $ unsafeCoerce Refl
    _ `geq` _ = Nothing


$(derive [smartConstructors, makeShowHF, makeEqHF, makeHFunctor, makeHFoldable, makeHTraversable] [''IntTreeF])


-- Example terms and dags

it1 :: Term IntTreeF _
it1 = iNode (iNode x (iLeaf 10)) x
    where x = iNode y y
          y = iLeaf 20

i1 :: Dag IntTreeF _
{-# NOINLINE i1 #-}
i1 = unsafePerformIO $ reifyDag it1

--     [ (0, Node 1 2)
--     , (1, Node 2 3)
--     , (2, Node 4 4)
--     , (3, Leaf 10)
--     , (4, Leaf 20)
--     ]


it2 :: Term IntTreeF _
it2 = iNode x (iNode (iLeaf 5) x)
    where x = iNode (iNode (iLeaf 24) (iLeaf 3)) (iLeaf 4)

i2 :: Dag IntTreeF _
{-# NOINLINE i2 #-}
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

