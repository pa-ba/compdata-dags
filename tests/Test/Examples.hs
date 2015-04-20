{-# LANGUAGE GADTs #-}

module Test.Examples where

import Examples.Types
import Examples.Repmin
import Examples.TypeInference
import Examples.LeavesBelow
import Test.QuickCheck
import Test.HUnit
import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.Framework.Providers.HUnit
import Test.Utils
import Test.Dag
import Data.Comp.Term
import Data.Comp.Dag
import qualified Data.Map as Map
import qualified Examples.RepminPAG as PAG
import Data.Comp.Dag.Internal
import qualified Data.IntMap as IntMap

tests = 
    [ testGroup "Repmin"
      [ testCase "AG" case_repminAG
      , testCase "Rewrite" case_repminRewrite
      , testCase "TreePAG" case_repminTreePAG
      , testCase "PAG bisim" case_repminPAG
      , testCase "PAG single leaf" case_repminPAG_singleLeaf
      , testCase "PAG iso" case_repminPAG_iso
      ]
    , testProperty "LeavesBelow" prop_leavesBelow
    , testCase "TypeInference" case_typeInf
    ]
    

case_repminAG = testAllEq' intTrees repmin repminG
case_repminRewrite = testAllEq' intTrees repmin (unravel . repminG')

-- Result of rewrite version and version are not iso but at least
-- bisimilar.
case_repminPAG = testAllDagBisim' intTrees repminG' PAG.repminG

-- The PAG version produces a result with only one leaf node.
case_repminPAG_singleLeaf = testAllDag' hasSingleLeaf message intTrees PAG.repminG
  where message g = show g ++ " has more than one leaf node"

-- The PAG version produces a result with only one leaf node.
case_repminPAG_iso = testAllDag2' p message intTrees repminG' PAG.repminG
  where p g1 g2 = g1 `iso` g2 || (not (hasSingleLeaf g1) && hasSingleLeaf g2)
        message g1 g2 = show g1 ++ " and " ++ show g2 ++ " should coincide since the former has only one leaf node"


-- | Checks whether the given dag has only one leaf node.
hasSingleLeaf :: Dag IntTreeF -> Bool
hasSingleLeaf Dag {root = r, edges = e} = IntMap.foldr (\t c -> countLeaves t + c) (countLeaves r) e == 1

-- | Counts the leaf nodes in the given context.
countLeaves :: IntTreeF (Context IntTreeF a) -> Int
countLeaves (Leaf _) = 1
countLeaves (Node x y) = countLeaves' x + countLeaves' y
  where
    countLeaves' (Term t) = countLeaves t
    countLeaves' (Hole _) = 0
        

case_repminTreePAG = mapM_ run intTrees 
    where run t = repmin t @=? PAG.repmin t

prop_leavesBelow d = testAllEq intTrees (leavesBelow d) (leavesBelowG d)


expTrees :: [Term ExpF]
expTrees = [t1,t2] where
    t1 = iIter "x" x x (iAdd (iIter "y" z z (iAdd z y)) y)
        where x = iLitI 10
              y = iVar "x"
              z = iLitI 5
    t2 = iAdd (iIter "x" x x z) (iIter "y" y y z)
        where x = iLitI 10
              y = iLitB False
              z = iVar "x"

case_typeInf = testAllEq' expTrees (typeInf Map.empty) (typeInfG Map.empty)
