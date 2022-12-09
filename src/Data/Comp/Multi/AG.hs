{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}


--------------------------------------------------------------------------------
-- |
-- Module      :  Data.Comp.AG
-- Copyright   :  (c) 2014 Patrick Bahr, Emil Axelsson
-- License     :  BSD3
-- Maintainer  :  Patrick Bahr <paba@di.ku.dk>
-- Stability   :  experimental
-- Portability :  non-portable (GHC Extensions)
--
-- This module implements recursion schemes derived from attribute
-- grammars.
--
--------------------------------------------------------------------------------

module Data.Comp.Multi.AG
    ( runAG
    --, runRewrite
    , module I
    )  where

import Data.Comp.Multi.AG.Internal
import qualified Data.Comp.Multi.AG.Internal as I hiding (explicit)
import Data.Comp.Multi.Algebra
import Data.Comp.Multi.Mapping as I
import Data.Comp.Multi.Term
import Data.Comp.Multi.HFunctor
import Data.Comp.Multi.Projection as I




-- | This function runs an attribute grammar on a term. The result is
-- the (combined) synthesised attribute at the root of the term.

runAG :: forall f u d i . HTraversable f
      => Syn' f (u,d) u -- ^ semantic function of synthesised attributes
      -> Inh' f (u,d) d -- ^ semantic function of inherited attributes
      -> (u -> d)       -- ^ initialisation of inherited attributes
      -> Term f i       -- ^ input term
      -> u
runAG up down dinit t = uFin where
    uFin = run dFin t
    dFin = dinit uFin
    run :: d -> Term f :=> u
    run d (Term t) = u where
        t' = hfmap bel $ number t
        bel :: Numbered (Term f) :-> Numbered (K (u,d))
        bel (Numbered i s) =
            let d' = lookupNumMap d i m
            in Numbered i $ K (run d' s, d')
        m = explicit down (u,d) (unK . unNumbered) t'
        u = explicit up (u,d) (unK . unNumbered) t'

-- | This function runs an attribute grammar with rewrite function on
-- a term. The result is the (combined) synthesised attribute at the
-- root of the term and the rewritten term.

runRewrite :: forall f g u d i . (HTraversable f, HFunctor g)
           => Syn' f (u,d) u -> Inh' f (u,d) d -- ^ semantic function of synthesised attributes
           -> Rewrite f (u,d) g                -- ^ semantic function of inherited attributes
           -> (u -> d)                         -- ^ initialisation of inherited attributes
           -> Term f i                         -- ^ input term
           -> (u, Term g i)
runRewrite up down trans dinit t = res where
    res@(uFin,_) = run dFin t
    dFin = dinit uFin
    run :: d -> Term f i -> (u, Term g i)
    run d (Term t) = (u,t'') where
        t' = hfmap _bel $ number t
        bel (Numbered i s) =
            let d' = lookupNumMap d i m
                (u', s') = run d' s
            in Numbered i ((u', d'),s')
        m = undefined --explicit down (u,d) (fst . unNumbered) t'
        u = undefined --explicit up (u,d) (fst . unNumbered) t'
        t'' = undefined --appCxt $ fmap (snd . unNumbered) $ explicit trans (u,d) (fst . unNumbered) t'
