module Test.Dag where

import Examples.Types
import Examples.Repmin
import Examples.TypeInference
import Examples.LeavesBelow
import Test.HUnit
import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.Framework.Providers.HUnit
import Test.Utils
import Data.Comp.Term
import Data.Comp.Dag
import Data.Comp.Dag.Internal
import qualified Data.IntMap as IntMap
import Control.Monad (zipWithM_)

tests =
    [ testGroup "reify"
      [ testCase "unravel" case_reifyUnravel
      , testCase "strongIso" case_reifyStrongIso
      , testCase "iso" case_reifyIso
      , testCase "bisim" case_reifyBisim
      ]
    ]


intTrees :: [Term IntTreeF]
intTrees = [it1,it2,it3,it4] where
    it1 = iTreeNode (iTreeNode x (iLeaf 10)) x
        where x = iTreeNode y y
              y = iLeaf 20
    it2 = iTreeNode x (iTreeNode (iLeaf 5) x)
        where x = iTreeNode (iTreeNode (iLeaf 24) (iLeaf 3)) (iLeaf 4)
    it3 = iLeaf 5
    it4 = iTreeNode x x
        where x = iLeaf 0


case_reifyUnravel = testAllEq' intTrees id unravel


intGraphs :: [Dag IntTreeF]
intGraphs = [it1,it2,it3,it4] where
    it1 = Dag { root = TreeNode (iTreeNode (Hole 0) (iLeaf 10)) (Hole 0)
              , edges = IntMap.fromList
                        [(0, TreeNode (Hole 1) (Hole 1)),
                         (1, Leaf 20)]
              , nodeCount = 2}
    it2 = Dag { root = TreeNode (Hole 0) (iTreeNode (iLeaf 5) (Hole 0))
              , edges = IntMap.fromList [(0, TreeNode (iTreeNode (iLeaf 24) (iLeaf 3)) (iLeaf 4))]
              , nodeCount = 1}
    it3 = Dag { root = Leaf 5, edges = IntMap.empty, nodeCount = 0 }
    it4 = Dag { root = TreeNode (Hole 0) (Hole 0)
              , edges = IntMap.singleton 0 (Leaf 0)
              , nodeCount = 1}


isoNotStrong :: [(Term IntTreeF,Dag IntTreeF)]
isoNotStrong = [(it1,ig1),(it2,ig2)] where
    it1 = iTreeNode z x
        where x = iTreeNode y y
              y = iLeaf 20
              z = iTreeNode x (iLeaf 10)
    ig1 = Dag { root = TreeNode (Hole 2) (Hole 0)
              , edges = IntMap.fromList
                        [(0, TreeNode (Hole 1) (Hole 1)),
                         (1, Leaf 20),
                         (2, TreeNode (Hole 0) (iLeaf 10))]
              , nodeCount = 3}
    it2 = iTreeNode x z
        where x = iTreeNode (iTreeNode (iLeaf 24) (iLeaf 3)) (iLeaf 4)
              z = iTreeNode (iLeaf 5) x
    ig2 = Dag { root = TreeNode (Hole 0) (Hole 1)
              , edges = IntMap.fromList
                        [ (0, TreeNode (iTreeNode (iLeaf 24) (iLeaf 3)) (iLeaf 4))
                        , (1, TreeNode (iLeaf 5) (Hole 0))]
              , nodeCount = 2}

bisimNotIso :: [(Term IntTreeF,Dag IntTreeF)]
bisimNotIso = [(it1,ig1),(it2,ig2)] where
    it1 = iTreeNode z x
        where x = iTreeNode y y
              y = iLeaf 20
              z = iTreeNode x (iLeaf 10)
    ig1 = Dag { root = TreeNode (iTreeNode (Hole 0) (iLeaf 10)) (Hole 0)
              , edges = IntMap.fromList
                        [(0, TreeNode (iLeaf 20) (iLeaf 20))]
              , nodeCount = 1}

    it2 = iTreeNode x z
        where x = iTreeNode (iTreeNode y y) (iLeaf 4)
              y = iLeaf 3
              z = iTreeNode (iLeaf 5) x
    ig2 = Dag { root = TreeNode (Hole 0) (iTreeNode (iLeaf 5) (Hole 0))
              , edges = IntMap.fromList [(0, TreeNode (iTreeNode (iLeaf 3) (iLeaf 3)) (iLeaf 4))]
              , nodeCount = 1}


case_reifyStrongIso = zipWithM_ run intTrees intGraphs
    where run t g = do d <- reifyDag t
                       assertBool ("strongIso\n" ++ show d ++ "\n\n" ++ show g) (strongIso d g)

case_reifyIso = mapM_ run isoNotStrong
    where run (t1, d2) = do d1 <- reifyDag t1
                            assertBool ("not strongIso\n" ++ show d1 ++ "\n\n" ++ show d2) (not (strongIso d1 d2))
                            assertBool ("iso\n" ++ show d1 ++ "\n\n" ++ show d2) (iso d1 d2)


case_reifyBisim = mapM_ run bisimNotIso
    where run (t1, d2) = do
            d1 <- reifyDag t1
            assertBool ("not iso\n" ++ show d1 ++ "\n\n" ++ show d2) (not (iso d1 d2))
            assertBool ("bisim\n" ++ show d1 ++ "\n\n" ++ show d2) (bisim d1 d2)
