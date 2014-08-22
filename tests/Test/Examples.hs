module Test.Examples where

import Examples.Types
import Examples.Repmin
import Examples.TypeInference
import Examples.LeavesBelow
import Test.QuickCheck
import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.Framework.Providers.HUnit
import Test.Utils
import Data.Comp.Term
import Data.Comp.Dag
import qualified Data.Map as Map

tests = 
    [ testGroup "Repmin"
      [ testCase "AG" case_repminAG
      , testCase "Rewrite" case_repminRewrite
      ]
    , testProperty "LeavesBelow" prop_leavesBelow
    , testCase "TypeInference" case_typeInf
    ]


intTrees :: [Term IntTreeF]
intTrees = [it1,it2,it3,it4] where
    it1 = iNode (iNode x (iLeaf 10)) x
        where x = iNode y y
              y = iLeaf 20
    it2 = iNode x (iNode (iLeaf 5) x)
        where x = iNode (iNode (iLeaf 24) (iLeaf 3)) (iLeaf 4)
    it3 = iLeaf 5
    it4 = iNode x x
        where x = iLeaf 0

    

case_repminAG = testAllEq' intTrees repmin repminG
case_repminRewrite = testAllEq' intTrees repmin (unravel . repminG')

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
