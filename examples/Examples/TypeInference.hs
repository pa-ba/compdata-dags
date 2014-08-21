{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImplicitParams             #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeOperators              #-}

module Examples.TypeInference where

import Data.Comp.AG
import Data.Comp.Dag
import qualified Data.Comp.Dag.AG as Dag
import Data.Comp.Term
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Comp.Derive

import System.IO.Unsafe


intersection :: (Ord k, Eq v) => Map k v -> Map k v -> Map k v
intersection = Map.mergeWithKey (\_ x1 x2 -> if x1 == x2 then Just x1 else Nothing)
                     (const Map.empty) (const Map.empty)




type Name = String

data  Type  = BoolType | IntType deriving (Eq, Show)
type  Env   = Map Name Type

insertEnv :: Name -> Maybe Type -> Env -> Env
insertEnv _ Nothing   env  =  env
insertEnv v (Just t)  env  =  Map.insert v t env

lookEnv :: Name -> Env -> Maybe Type
lookEnv = Map.lookup


data ExpF a  =  LitB Bool   |  LitI Int  |  Var Name
             |  Eq a a      |  Add a a   |  If a a a
             |  Iter Name a a a
  deriving (Eq, Show, Functor, Foldable, Traversable)
$(derive [smartConstructors] [''ExpF])

typeOf ::  (?below :: a -> atts, Maybe Type :< atts) =>
           a -> Maybe Type
typeOf = below

typeInfS :: (Env :< atts) => Syn ExpF atts (Maybe Type)
typeInfS (LitB _)                =  Just BoolType
typeInfS (LitI _)                =  Just IntType
typeInfS (Eq a b)
  |  Just ta        <-  typeOf a
  ,  Just tb        <-  typeOf b
  ,  ta == tb                     =  Just BoolType
typeInfS (Add a b)
  |  Just  IntType  <-  typeOf a
  ,  Just  IntType  <-  typeOf b  =  Just IntType
typeInfS (If c t f)
  |  Just BoolType  <-  typeOf c
  ,  Just tt        <-  typeOf t
  ,  Just tf        <-  typeOf f
  ,  tt == tf                     =  Just tt
typeInfS (Var v)                 =  lookEnv v above
typeInfS (Iter _ n i b)
  |  Just IntType   <-  typeOf n
  ,  Just ti        <-  typeOf i
  ,  Just tb        <-  typeOf b
  ,  ti == tb                     =  Just tb
typeInfS _                        =  Nothing

typeInfI :: (Maybe Type :< atts) => Inh ExpF atts Env
typeInfI (Iter v _ i b)  =  b |-> insertEnv v ti above
                               where ti = typeOf i
typeInfI _                =  empty

typeInf :: Env -> Term ExpF -> Maybe Type
typeInf env = runAG typeInfS typeInfI (const env)

typeInfG :: Env -> Dag ExpF -> Maybe Type
typeInfG env = Dag.runAG intersection typeInfS typeInfI (const env)

gt1 :: Term ExpF
gt1 = iIter "x" x x (iAdd (iIter "y" z z (iAdd z y)) y)
    where x = iLitI 10
          y = iVar "x"
          z = iLitI 5

g1 :: Dag ExpF
g1 = unsafePerformIO $ reifyDag gt1
--     [ (0, Iter "x" 1 1 2)
--     , (1, LitI 10)
--     , (2, Add 3 4)
--     , (3, Iter "y" 5 5 6)
--     , (4, Var "x")
--     , (5, LitI 5)
--     , (6, Add 5 4)
--     ]

typeTestG1 = typeInfG Map.empty g1
typeTestT1 = typeInf Map.empty (unravel g1)

gt2 :: Term ExpF
gt2 = iIter "x" x (iIter "x" x x y) y
    where x = iLitI 0
          y = iVar "x"

g2 :: Dag ExpF
g2 = unsafePerformIO $ reifyDag gt2

--     [ (0, Iter "x" 1 2 3)
--     , (1, LitI 0)
--     , (2, Iter "x" 1 1 3)
--     , (3, Var "x")
--     ]

typeTestG2 = typeInfG Map.empty g2
typeTestT2 = typeInf Map.empty (unravel g2)

gt3 :: Term ExpF
gt3 = iAdd (iIter "x" x x z) (iIter "x" y y z)
    where x = iLitI 10
          y = iLitB False
          z = iVar "x"

g3 :: Dag ExpF
g3 = unsafePerformIO $ reifyDag gt3

--     [ (0, Add 1 3)
--     , (1, Iter "x" 2 2 5)
--     , (2, LitI 10)
--     , (3, Iter "x" 4 4 5)
--     , (4, LitB False)
--     , (5, Var "x")
--     ]

typeTestG3 = typeInfG Map.empty g3
typeTestT3 = typeInf Map.empty (unravel g3)


