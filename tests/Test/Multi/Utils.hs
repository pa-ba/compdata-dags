{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

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
import Data.Typeable

data T f where T :: Typeable i => f i -> T f

testAllEq' :: (Typeable f, Typeable i, HFgeq f, HTraversable f, Show a, Eq a) => [T (Term f)] -> (Term f :=> a) -> (Dag f :=> a) -> Assertion
testAllEq' trees f1 f2 = mapM_ run trees
    where run (T t) = do d <- reifyDag t
                         f1 t @=? f2 d

testAllDagEq' :: (Typeable f, Typeable g, HFgeq f, HTraversable f, EqHF g, ShowHF g, HTraversable g) => [T (Term f)] -> (Dag f :-> Dag g) -> (Dag f :-> Dag g) -> Assertion
testAllDagEq' trees f1 f2 = mapM_ run trees
    where run (T t) = do d <- reifyDag t
                         assertBool (show (f1 d) ++ " =iso= " ++ show (f2 d)) (f1 d `iso` f2 d)

testAllDagBisim' :: (Typeable f, HFgeq f, HTraversable f, EqHF g, ShowHF g, HTraversable g) => [T (Term f)] -> (Dag f :-> Dag g) -> (Dag f :-> Dag g) -> Assertion
testAllDagBisim' trees f1 f2 = mapM_ run trees
    where run (T t) = do d <- reifyDag t
                         assertBool (show (f1 d) ++ " =bisim= " ++ show (f2 d)) (f1 d `bisim` f2 d)


testAllDag' :: (Typeable f, HFgeq f, HTraversable f, HTraversable g) => (Dag g :=> Bool) -> (Dag g :=> String) -> [T (Term f)] -> (Dag f :-> Dag g) -> Assertion
testAllDag' p message trees f1 = mapM_ run trees
    where run (T t) = do d <- reifyDag t
                         assertBool (message (f1 d)) (p (f1 d))

testAllDag2' :: (HFgeq f, HTraversable f, HTraversable g, Typeable f) => (Dag g :=> Dag g :=> Bool) -> (Dag g :=> Dag g :=> String) -> [T (Term f)] -> (Dag f :-> Dag g) -> (Dag f :-> Dag g) -> Assertion
testAllDag2' p message trees f1 f2 = mapM_ run trees
    where run (T t) = do d <- reifyDag t
                         assertBool (message (f1 d) (f2 d)) (p (f1 d) (f2 d))


testAllEq :: (HFgeq f, HTraversable f, Show a, Eq a, Typeable f) => [T (Term f)] -> (Term f :=> a) -> (Dag f :=> a) -> Property
testAllEq trees f1 f2 = conjoin $ map run trees
    where run (T t) = ioProperty $ do 
                        d <- reifyDag t
                        return (f1 t === f2 d)
