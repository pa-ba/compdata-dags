{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}


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

module Data.Comp.AG
    ( runAG
    , runSynAG
    , runRewrite
    , runSynRewrite
    , module I
    )  where

import Data.Comp.AG.Internal
import qualified Data.Comp.AG.Internal as I hiding (explicit)
import Data.Comp.Algebra
import Data.Comp.Mapping as I
import Data.Comp.Term
import Data.Comp.Projection as I




-- | This function runs an attribute grammar on a term. The result is
-- the (combined) synthesised attribute at the root of the term.

runAG :: forall f u d . Traversable f
      => Syn' f (u,d) u -- ^ semantic function of synthesised attributes
      -> Inh' f (u,d) d -- ^ semantic function of inherited attributes
      -> (u -> d)       -- ^ initialisation of inherited attributes
      -> Term f         -- ^ input term
      -> u
runAG up down dinit t = uFin where
    uFin = run dFin t
    dFin = dinit uFin
    run :: d -> Term f -> u
    run d (Term t) = u where
        t' = bel <$> number t
        bel (Numbered i s) =
            let d' = lookupNumMap d i m
            in Numbered i (run d' s, d')
        m :: NumMap (u, d) d
        m = explicit down (u,d) unNumbered t'
        u = explicit up (u,d) unNumbered t'

-- | This function runs an attribute grammar with no inherited attributes on a term. The result is
-- the (combined) synthesised attribute at the root of the term.

runSynAG :: forall f u . Traversable f
      => Syn' f u u -- ^ semantic function of synthesised attributes
      -> Term f         -- ^ input term
      -> u
runSynAG up t = run t where
    run :: Term f -> u
    run (Term t) = u where u = explicit up u id $ fmap run t

-- | This function runs an attribute grammar with rewrite function on
-- a term. The result is the (combined) synthesised attribute at the
-- root of the term and the rewritten term.

runRewrite :: forall f g u d . (Traversable f, Functor g)
           => Syn' f (u,d) u                   -- ^ semantic function of synthesised attributes
           -> Inh' f (u,d) d                   -- ^ semantic function of inherited attributes
           -> Rewrite f (u,d) g                -- ^ rewrite function (stateful tree homomorphism)
           -> (u -> d)                         -- ^ initialisation of inherited attributes
           -> Term f                           -- ^ input term
           -> (u, Term g)
runRewrite up down trans dinit t = res where
    res@(uFin,_) = run dFin t
    dFin = dinit uFin
    run :: d -> Term f -> (u, Term g)
    run d (Term t) = (u,t'') where
        t' = bel <$> number t
        bel (Numbered i s) =
            let d' = lookupNumMap d i m
                (u', s') = run d' s
            in Numbered i ((u', d'),s')
        m :: NumMap ((u, d), Term g) d
        m = explicit down (u,d) (fst . unNumbered) t'
        u = explicit up (u,d) (fst . unNumbered) t'
        t'' = appCxt $ snd . unNumbered <$> explicit trans (u,d) (fst . unNumbered) t'

-- | This function runs a synthesised attribute grammar with rewrite function on
-- a term. The result is the (combined) synthesised attribute at the
-- root of the term and the rewritten term.

runSynRewrite :: forall f g u . (Traversable f, Functor g)
           => Syn f u u                        -- ^ semantic function of synthesised attributes
           -> Rewrite f u g                    -- ^ rewrite function (stateful tree homomorphism)
           -> Term f                           -- ^ input term
           -> (u, Term g)
runSynRewrite up trans t = run t where
    run :: Term f -> (u, Term g)
    run (Term t) = (u,t'') where
        bel s = let (u', s') = run s in (u',s')
        u = explicit up u fst $ bel <$> t
        t'' = appCxt . fmap snd . explicit trans u fst $ fmap bel t
