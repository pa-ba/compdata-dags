{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}

module Test.Multi.Dag where

import Examples.Multi.Types
--import Examples.Multi.Repmin
import Examples.TypeInference
import Examples.Multi.LeavesBelow
import Test.HUnit
import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.Framework.Providers.HUnit
import Test.Multi.Utils
import Data.Comp.Multi.Term
import Data.Comp.Multi.Dag
import Data.Comp.Multi.HFunctor
import Data.Comp.Multi.Dag.Internal
import qualified Data.Dependent.Map as M
import qualified Data.Dependent.Sum as S
import Control.Monad
import Data.Typeable
import Unsafe.Coerce

tests =
    [ testGroup "reify"
      [ --testCase "unravel" case_reifyUnravel
      --  testCase "strongIso" case_reifyStrongIso
      --, testCase "iso" case_reifyIso
      --, testCase "bisim" case_reifyBisim
      ]
    ]


intTrees :: [T (Term IntTreeF)]
intTrees = [T it1,T it2,T it3,T it4] where
    it1 = iTreeNode (iTreeNode x (iLeaf 10)) x
        where x = iTreeNode y y
              y = iLeaf 20
    it2 = iTreeNode x (iTreeNode (iLeaf 5) x)
        where x = iTreeNode (iTreeNode (iLeaf 24) (iLeaf 3)) (iLeaf 4)
    it3 = iLeaf 5
    it4 = iTreeNode x x
        where x = iLeaf 0

instance Show (T (Term IntTreeF)) where show (T x) = show x

--case_reifyUnravel = testAllEq' intTrees T (T . unravel)


intGraphs :: [T (Dag IntTreeF)]
intGraphs = [T it1,T it2,T it3,T it4] where
    it1 :: Dag IntTreeF (((Int, Int), Int), (Int, Int))
    it1 = Dag (TreeNode (iTreeNode (Hole 0) (iLeaf 10)) (Hole 0))
              (M.fromList
                        [Node @(Int,Int) 0 S.:=> TreeNode (Hole 1) (Hole 1),
                         Node 1 S.:=> Leaf 20])
              2
    it2 :: Dag IntTreeF (((Int, Int), Int), (Int, ((Int, Int), Int)))
    it2 = Dag (TreeNode (Hole 0) (iTreeNode (iLeaf 5) (Hole 0)))
              (M.fromList [Node 0 S.:=> TreeNode (iTreeNode (iLeaf 24) (iLeaf 3)) (iLeaf 4)])
              1
    it3 = Dag (Leaf 5) M.empty 0
    it4 :: Dag IntTreeF (Int, Int)
    it4 = Dag (TreeNode (Hole 0) (Hole 0))
              (M.singleton 0 (Leaf 0))
              1

data DagPair f where DagPair  :: Typeable i => (Term f i, Dag f i) -> DagPair f

isoNotStrong :: [DagPair IntTreeF]
isoNotStrong = [DagPair (it1,ig1),DagPair (it2,ig2)] where
    it1 = iTreeNode z x
        where x = iTreeNode y y
              y = iLeaf 20
              z = iTreeNode x (iLeaf 10)
    ig1 = Dag (TreeNode (Hole 2) (Hole 0))
              (M.fromList
                        [Node @(Int,Int) 0 S.:=> TreeNode (Hole 1) (Hole 1),
                         Node 1 S.:=> Leaf 20,
                         Node @((Int,Int),Int) 2 S.:=> TreeNode (Hole 0) (iLeaf 10)])
              3
    it2 = iTreeNode x z
        where x = iTreeNode (iTreeNode (iLeaf 24) (iLeaf 3)) (iLeaf 4)
              z = iTreeNode (iLeaf 5) x
    ig2 = Dag (TreeNode (Hole 0) (Hole 1))
              (M.fromList
                        [ Node 0 S.:=> TreeNode (iTreeNode (iLeaf 24) (iLeaf 3)) (iLeaf 4)
                        , Node @(Int,((Int,Int),Int)) 1 S.:=> TreeNode (iLeaf 5) (Hole 0)])
              2

bisimNotIso :: [DagPair IntTreeF]
bisimNotIso = [DagPair (it1,ig1),DagPair (it2,ig2)] where
    it1 = iTreeNode z x
        where x = iTreeNode y y
              y = iLeaf 20
              z = iTreeNode x (iLeaf 10)
    ig1 = Dag (TreeNode (iTreeNode (Hole 0) (iLeaf 10)) (Hole 0))
              (M.fromList
                        [Node 0 S.:=> TreeNode (iLeaf 20) (iLeaf 20)])
              1

    it2 = iTreeNode x z
        where x = iTreeNode (iTreeNode y y) (iLeaf 4)
              y = iLeaf 3
              z = iTreeNode (iLeaf 5) x
    ig2 = Dag (TreeNode (Hole 0) (iTreeNode (iLeaf 5) (Hole 0)))
              (M.fromList [Node 0 S.:=> TreeNode (iTreeNode (iLeaf 3) (iLeaf 3)) (iLeaf 4)])
              1


{-
case_reifyStrongIso = zipWithM_ run intTrees intGraphs
    where run (T t) (T g) = do d <- reifyDag t
                               assertBool ("strongIso\n" ++ show d ++ "\n\n" ++ show (g :: Dag IntTreeF _)) (strongIso d $ unsafeCoerce g)

case_reifyIso = mapM_ run isoNotStrong
    where run (DagPair (t1, d2)) = do d1 <- reifyDag t1
                                      assertBool ("not strongIso\n" ++ show d1 ++ "\n\n" ++ show d2) (not (strongIso d1 d2))
                                      assertBool ("iso\n" ++ show d1 ++ "\n\n" ++ show d2) (iso d1 d2)


case_reifyBisim = mapM_ run bisimNotIso
    where run (DagPair (t1, d2)) = do
            d1 <- reifyDag t1
            assertBool ("not iso\n" ++ show d1 ++ "\n\n" ++ show d2) (not (iso d1 d2))
            assertBool ("bisim\n" ++ show d1 ++ "\n\n" ++ show d2) (bisim d1 d2)
-}
