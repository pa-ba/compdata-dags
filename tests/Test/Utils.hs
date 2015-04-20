module Test.Utils where

import Test.HUnit
import Test.QuickCheck
import Data.Comp.Term
import Data.Comp.Dag
import Data.Comp.Equality
import Data.Comp.Show
import Data.Traversable

testAllEq' :: (Traversable f, Show a, Eq a) => [Term f] -> (Term f -> a) -> (Dag f -> a) -> Assertion
testAllEq' trees f1 f2 = mapM_ run trees
    where run t = do d <- reifyDag t
                     f1 t @=? f2 d

testAllDagEq' :: (Traversable f, EqF g, ShowF g, Traversable g) => [Term f] -> (Dag f -> Dag g) -> (Dag f -> Dag g) -> Assertion
testAllDagEq' trees f1 f2 = mapM_ run trees
    where run t = do d <- reifyDag t
                     assertBool (show (f1 d) ++ " =iso= " ++ show (f2 d)) (f1 d `iso` f2 d)

testAllDagBisim' :: (Traversable f, EqF g, ShowF g, Traversable g) => [Term f] -> (Dag f -> Dag g) -> (Dag f -> Dag g) -> Assertion
testAllDagBisim' trees f1 f2 = mapM_ run trees
    where run t = do d <- reifyDag t
                     assertBool (show (f1 d) ++ " =bisim= " ++ show (f2 d)) (f1 d `bisim` f2 d)


testAllDag' :: (Traversable f, Traversable g) => (Dag g -> Bool) -> (Dag g -> String) -> [Term f] -> (Dag f -> Dag g) -> Assertion
testAllDag' p message trees f1 = mapM_ run trees
    where run t = do d <- reifyDag t
                     assertBool (message (f1 d)) (p (f1 d))

testAllDag2' :: (Traversable f, Traversable g) => (Dag g -> Dag g -> Bool) -> (Dag g -> Dag g -> String) -> [Term f] -> (Dag f -> Dag g) -> (Dag f -> Dag g) -> Assertion
testAllDag2' p message trees f1 f2 = mapM_ run trees
    where run t = do d <- reifyDag t
                     assertBool (message (f1 d) (f2 d)) (p (f1 d) (f2 d))


testAllEq :: (Traversable f, Show a, Eq a) => [Term f] -> (Term f -> a) -> (Dag f -> a) -> Property
testAllEq trees f1 f2 = conjoin $ map run trees
    where run t = ioProperty $ do 
                    d <- reifyDag t
                    return (f1 t === f2 d)
