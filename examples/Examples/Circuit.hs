{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImplicitParams             #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeOperators              #-}

module Examples.Circuit where

import Data.Comp.AG
import Data.Comp.Dag
import qualified Data.Comp.Dag.AG as Dag
import Data.Comp.Term
import Data.Comp.Derive





data CircuitF a = Input | Nand a a
  deriving (Eq, Show, Functor, Foldable, Traversable)

$(derive [smartConstructors, makeShowF] [''CircuitF])


type Circuit = Dag CircuitF

newtype Delay  = Delay  Int  deriving (Eq,Ord,Show,Num)
newtype Load   = Load   Int  deriving (Eq,Ord,Show,Num)

gateDelay :: (Load :< atts) => Syn CircuitF atts Delay
gateDelay Input       = 0
gateDelay (Nand a b)  =
  max (below a) (below b) + 10 + Delay l
    where Load l = above

gateLoad :: Inh CircuitF atts Load
gateLoad (Nand a b)  = a |-> 1 & b |-> 1
gateLoad _           = empty

delay :: Circuit -> Load -> Delay
delay g l = Dag.runAG (+) gateDelay gateLoad (const l) g

delayTree :: Term CircuitF -> Load -> Delay
delayTree c l = runAG gateDelay gateLoad (const l) c
