{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}

module Test.Multi.Utils where

import Test.HUnit
import Test.QuickCheck
import Data.GADT.Compare
import Data.Comp.Multi.Term
import Data.Comp.Multi.Dag
import Data.Comp.Multi.Equality
import Data.Comp.Multi.Show
import Data.Comp.Multi.HFunctor
import Data.Comp.Multi.HTraversable

testAllEq' :: (GEq (f (Term f)), HTraversable f, Show a, Eq a) => [E (Term f)] -> (Term f :=> a) -> (Dag f :=> a) -> Assertion
testAllEq' trees f1 f2 = mapM_ run trees
    where run (E t) = do d <- reifyDag t
                         f1 t @=? f2 d

testAllDagEq' :: (GEq (f (Term f)), HTraversable f, EqHF g, ShowHF g, HTraversable g) => [E (Term f)] -> (Dag f :-> Dag g) -> (Dag f :-> Dag g) -> Assertion
testAllDagEq' trees f1 f2 = mapM_ run trees
    where run (E t) = do d <- reifyDag t
                         assertBool (show (f1 d) ++ " =iso= " ++ show (f2 d)) (f1 d `iso` f2 d)

testAllDagBisim' :: (GEq (f (Term f)), HTraversable f, EqHF g, ShowHF g, HTraversable g) => [E (Term f)] -> (Dag f :-> Dag g) -> (Dag f :-> Dag g) -> Assertion
testAllDagBisim' trees f1 f2 = mapM_ run trees
    where run (E t) = do d <- reifyDag t
                         assertBool (show (f1 d) ++ " =bisim= " ++ show (f2 d)) (f1 d `bisim` f2 d)


testAllDag' :: (GEq (f (Term f)), HTraversable f, HTraversable g) => (Dag g :=> Bool) -> (Dag g :=> String) -> [E (Term f)] -> (Dag f :-> Dag g) -> Assertion
testAllDag' p message trees f1 = mapM_ run trees
    where run (E t) = do d <- reifyDag t
                         assertBool (message (f1 d)) (p (f1 d))

testAllDag2' :: (GEq (f (Term f)), HTraversable f, HTraversable g) => (Dag g :=> Dag g :=> Bool) -> (Dag g :=> Dag g :=> String) -> [E (Term f)] -> (Dag f :-> Dag g) -> (Dag f :-> Dag g) -> Assertion
testAllDag2' p message trees f1 f2 = mapM_ run trees
    where run (E t) = do d <- reifyDag t
                         assertBool (message (f1 d) (f2 d)) (p (f1 d) (f2 d))


testAllEq :: (GEq (f (Term f)), HTraversable f, Show a, Eq a) => [E (Term f)] -> (Term f :=> a) -> (Dag f :=> a) -> Property
testAllEq trees f1 f2 = conjoin $ map run trees
    where run (E t) = ioProperty $ do 
                        d <- reifyDag t
                        return (f1 t === f2 d)
