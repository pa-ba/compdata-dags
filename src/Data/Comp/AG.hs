{-# LANGUAGE ImplicitParams        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Rank2Types            #-}
{-# LANGUAGE TypeOperators         #-}

module Data.Comp.AG where


import Data.Projection

import Data.Comp.Mapping
import Data.Comp.Term



-- | This function provides access to components of the states from
-- "below".

below :: (?below :: a -> q, p :< q) => a -> p
below = pr . ?below

-- | This function provides access to components of the state from
-- "above"

above :: (?above :: q, p :< q) => p
above = pr ?above

-- | Turns the explicit parameters @?above@ and @?below@ into explicit
-- ones.

explicit :: ((?above :: q, ?below :: a -> q) => b) -> q -> (a -> q) -> b
explicit x ab be = x where ?above = ab; ?below = be



type Rewrite f q g = forall a . (?below :: a -> q, ?above :: q) => f a -> Context g a


-- | Definition of a synthesized attribute.

type Syn' f p q = forall a . (?below :: a -> p, ?above :: p) => f a -> q
type Syn  f p q = (q :< p) => Syn' f p q
type SynExpl f p q = forall a . p -> (a -> p) -> f a -> q

prodSyn :: (p :< c, q :< c)
             => Syn f c p -> Syn f c q -> Syn f c (p,q)
prodSyn sp sq t = (sp t, sq t)

(|*|) :: (p :< c, q :< c)
             => Syn f c p -> Syn f c q -> Syn f c (p,q)
(|*|) = prodSyn



-- | Definition of an inherited attribute
type Inh' f p q = forall m i . (Mapping m i, ?below :: i -> p, ?above :: p)
                                => f i -> m q
type Inh f p q = (q :< p) => Inh' f p q

type InhExpl f p q = forall m i . Mapping m i => p -> (i -> p) -> f i -> m q

prodInh :: (p :< c, q :< c)
               => Inh f c p -> Inh f c q -> Inh f c (p,q)
prodInh sp sq t = prodMap above above (sp t) (sq t)

-- | This is a synonym for 'prodInh'.

(>*<) :: (p :< c, q :< c, Functor f)
         => Inh f c p -> Inh f c q -> Inh f c (p,q)
(>*<) = prodInh
