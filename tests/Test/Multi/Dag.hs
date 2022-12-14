{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}

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
import Unsafe.Coerce

tests =
    [ testGroup "reify"
      [ testCase "unravel" case_reifyUnravel
      , testCase "strongIso" case_reifyStrongIso
      , testCase "iso" case_reifyIso
      , testCase "bisim" case_reifyBisim
      ]
    ]


intTrees :: [E (Term IntTreeF)]
intTrees = [E it1,E it2,E it3,E it4] where
    it1 = iNode (iNode x (iLeaf 10)) x
        where x = iNode y y
              y = iLeaf 20
    it2 = iNode x (iNode (iLeaf 5) x)
        where x = iNode (iNode (iLeaf 24) (iLeaf 3)) (iLeaf 4)
    it3 = iLeaf 5
    it4 = iNode x x
        where x = iLeaf 0

instance Show (E (Term IntTreeF)) where show (E x) = show x

case_reifyUnravel = testAllEq' intTrees E (E . unravel)


intGraphs :: [E (Dag IntTreeF)]
intGraphs = [E it1,E it2,E it3,E it4] where
    it1 = Dag { root = Node (iNode (Hole 0) (iLeaf 10)) (Hole 0)
              , edges = M.fromList
                        [K 0 S.:=> Node (Hole 1) (Hole 1),
                         K 1 S.:=> Leaf 20]
              , nodeCount = 2}
    it2 = Dag { root = Node (Hole 0) (iNode (iLeaf 5) (Hole 0))
              , edges = M.fromList [K 0 S.:=> Node (iNode (iLeaf 24) (iLeaf 3)) (iLeaf 4)]
              , nodeCount = 1}
    it3 = Dag { root = Leaf 5, edges = M.empty, nodeCount = 0 }
    it4 = Dag { root = Node (Hole 0) (Hole 0)
              , edges = M.singleton 0 (Leaf 0)
              , nodeCount = 1}

data DagPair f = forall i . DagPair {getDagPar :: (Term f i, Dag f i)}

isoNotStrong :: [DagPair IntTreeF]
isoNotStrong = [DagPair (it1,ig1),DagPair (it2,ig2)] where
    it1 = iNode z x
        where x = iNode y y
              y = iLeaf 20
              z = iNode x (iLeaf 10)
    ig1 = Dag { root = Node (Hole 2) (Hole 0)
              , edges = M.fromList
                        [K 0 S.:=> Node (Hole 1) (Hole 1),
                         K 1 S.:=> Leaf 20,
                         K 2 S.:=> Node (Hole 0) (iLeaf 10)]
              , nodeCount = 3}
    it2 = iNode x z
        where x = iNode (iNode (iLeaf 24) (iLeaf 3)) (iLeaf 4)
              z = iNode (iLeaf 5) x
    ig2 = Dag { root = Node (Hole 0) (Hole 1)
              , edges = M.fromList
                        [ K 0 S.:=> Node (iNode (iLeaf 24) (iLeaf 3)) (iLeaf 4)
                        , K 1 S.:=> Node (iLeaf 5) (Hole 0)]
              , nodeCount = 2}

bisimNotIso :: [DagPair IntTreeF]
bisimNotIso = [DagPair (it1,ig1),DagPair (it2,ig2)] where
    it1 = iNode z x
        where x = iNode y y
              y = iLeaf 20
              z = iNode x (iLeaf 10)
    ig1 = Dag { root = Node (iNode (Hole 0) (iLeaf 10)) (Hole 0)
              , edges = M.fromList
                        [K 0 S.:=> Node (iLeaf 20) (iLeaf 20)]
              , nodeCount = 1}

    it2 = iNode x z
        where x = iNode (iNode y y) (iLeaf 4)
              y = iLeaf 3
              z = iNode (iLeaf 5) x
    ig2 = Dag { root = Node (Hole 0) (iNode (iLeaf 5) (Hole 0))
              , edges = M.fromList [K 0 S.:=> Node (iNode (iLeaf 3) (iLeaf 3)) (iLeaf 4)]
              , nodeCount = 1}


case_reifyStrongIso = zipWithM_ run intTrees intGraphs
    where run (E t) (E g) = do d <- reifyDag t
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
