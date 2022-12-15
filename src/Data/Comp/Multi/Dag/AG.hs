{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecursiveDo         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}


--------------------------------------------------------------------------------
-- |
-- Module      :  Data.Comp.Dag.AG
-- Copyright   :  (c) 2014 Patrick Bahr, Emil Axelsson
-- License     :  BSD3
-- Maintainer  :  Patrick Bahr <paba@di.ku.dk>
-- Stability   :  experimental
-- Portability :  non-portable (GHC Extensions)
--
-- This module implements the recursion schemes from module
-- "Data.Comp.AG" on 'Dag's. In order to deal with the sharing present
-- in 'Dag's, the recursion schemes additionally take an argument of
-- type @d -> d -> d@ that resolves clashing inherited attribute
-- values.
--
--------------------------------------------------------------------------------


module Data.Comp.Multi.Dag.AG
    ( runAG
    , runSynAG
    , runRewrite
    , runSynRewrite
    , module I
    ) where

import Control.Monad.ST
import Data.Comp.Multi.AG.Internal
import qualified Data.Comp.Multi.AG.Internal as I hiding (explicit)
import Data.Comp.Multi.Dag
import Data.Comp.Multi.Dag.Internal
import Data.Comp.Multi.Mapping as I
import Data.Comp.Multi.Projection as I
import Data.Comp.Multi.Term
import qualified Data.Dependent.Map as M
import qualified Data.Dependent.Sum as S
import Data.Maybe
import Data.STRef
import Data.Comp.Multi.Algebra
import Data.Comp.Multi.HFunctor
import Data.Comp.Multi.HTraversable
import Data.Vector (Vector,MVector)
import qualified Data.Vector as Vec
import qualified Data.Vector.Generic.Mutable as MVec

newtype DPair f i = DPair (K Int i, f (Context f Node) i)
type EPair f = E (DPair f)

-- | This function runs an attribute grammar on a dag. The result is
-- the (combined) synthesised attribute at the root of the dag.

runAG :: forall f d u i . HTraversable f
    => (d -> d -> d)   -- ^ resolution function for inherited attributes
    -> Syn' f (u,d) u  -- ^ semantic function of synthesised attributes
    -> Inh' f (u,d) d  -- ^ semantic function of inherited attributes
    -> (u -> d)        -- ^ initialisation of inherited attributes
    -> Dag f i         -- ^ input dag
    -> u
runAG res syn inh dinit Dag {edges,root,nodeCount} = uFin where
    uFin = runST runM
    dFin = dinit uFin
    runM :: forall s . ST s u
    runM = mdo
      -- construct empty mapping from nodes to inherited attribute values
      dmap <- MVec.new nodeCount
      MVec.set dmap Nothing
      -- allocate mapping from nodes to synthesised attribute values
      umap <- MVec.new nodeCount
      -- allocate counter for numbering child nodes
      count <- newSTRef 0
      let -- Runs the AG on an edge with the given input inherited
          -- attribute value and produces the output synthesised
          -- attribute value.
          run :: d -> f (Context f Node) :=> ST s u
          run d t = mdo
             -- apply the semantic functions
             let u = explicit syn (u,d) (unK . unNumbered) result
                 m = explicit inh (u,d) (unK . unNumbered) result
                 -- recurses into the child nodes and numbers them
                 run' :: forall j . Context f Node j -> ST s (Numbered (K (u,d)) j)
                 run' s = do i <- readSTRef count
                             writeSTRef count $! (i+1)
                             let d' = lookupNumMap d i m
                             u' <- runF d' s -- recurse
                             return (Numbered i $ K (u',d'))
             result <- hmapM run' t
             return u
          -- recurses through the tree structure
          runF :: d -> Context f Node :=> ST s u
          runF d (Hole (K x)) = do
             -- we found a node: update the mapping for inherited
             -- attribute values
             old <- MVec.unsafeRead dmap x
             let new = case old of
                         Just o -> res o d
                         _      -> d
             MVec.unsafeWrite dmap x (Just new)
             return (umapFin Vec.! x)
          runF d (Term t)  = run d t
          -- This function is applied to each edge
          iter :: EPair f -> ST s ()
          iter (E (DPair (n,t))) = do
            writeSTRef count 0  -- re-initialize counter
            u <- run (fromJust $ dmapFin Vec.! unK n) t
            MVec.unsafeWrite umap (unK n) u
      -- first apply to the root
      u <- run dFin root
      -- then apply to the edges
      mapM_ iter ((\(n S.:=> t) -> E $ DPair (n, t)) <$> M.toList edges)
      -- finalise the mappings for attribute values
      dmapFin <- Vec.unsafeFreeze dmap
      umapFin <- Vec.unsafeFreeze umap
      return u

-- | This function runs an attribute grammar with no inherited attributes on a dag. The result is
-- the (combined) synthesised attribute at the root of the dag.

runSynAG :: forall f u i . HTraversable f =>
       Syn' f u u  -- ^ semantic function of synthesised attributes
    -> Dag f i     -- ^ input dag
    -> u
runSynAG syn Dag {edges,root,nodeCount} = runST runM where
    runM :: forall s . ST s u
    runM = mdo
      -- allocate mapping from nodes to synthesised attribute values
      umap <- MVec.new nodeCount
      let -- Runs the AG on an edge with the given input inherited
          -- attribute value and produces the output synthesised
          -- attribute value.
          run :: f (Context f Node) :=> ST s u
          run t = mdo
             -- apply the semantic functions
             let u = explicit syn u unK result
             result <- hmapM runF t
             return u
          -- recurses through the tree structure
          runF :: forall j . Context f Node j -> ST s (K u j)
          runF (Hole (K x)) = return . K $ umapFin Vec.! x
          runF (Term t)     = K <$> run t
          -- This function is applied to each edge
          iter :: EPair f -> ST s ()
          iter (E (DPair (n,t))) = do
            u <- run  t
            MVec.unsafeWrite umap (unK n) u
      -- first apply to the root
      u <- run root
      -- then apply to the edges
      mapM_ iter ((\(n S.:=> t) -> E $ DPair (n, t)) <$> M.toList edges)
      -- finalise the mappings for attribute values
      umapFin <- Vec.unsafeFreeze umap
      return u


-- | This function runs an attribute grammar with rewrite function on
-- a dag. The result is the (combined) synthesised attribute at the
-- root of the dag and the rewritten dag.

runRewrite :: forall f g d u i .(HTraversable f, HTraversable g)
    => (d -> d -> d)       -- ^ resolution function for inherited attributes
    -> Syn' f (u,d) u      -- ^ semantic function of synthesised attributes
    -> Inh' f (u,d) d      -- ^ semantic function of inherited attributes
    -> Rewrite f (u, d) g  -- ^ rewrite function (stateful tree homomorphism)
    -> (u -> d)            -- ^ initialisation of inherited attributes
    -> Dag f i             -- ^ input dag
    -> (u, Dag g i)
runRewrite res syn inh rewr dinit Dag {edges,root,nodeCount} = result where
    result@(uFin,_) = runST runM
    dFin = dinit uFin
    runM :: forall s . ST s (u, Dag g i)
    runM = mdo
      -- construct empty mapping from nodes to inherited attribute values
      dmap <- MVec.new nodeCount
      MVec.set dmap Nothing
      -- allocate mapping from nodes to synthesised attribute values
      umap <- MVec.new nodeCount
      -- allocate counter for numbering child nodes
      count <- newSTRef 0
      -- allocate vector to represent edges of the target DAG
      allEdges <- MVec.new nodeCount
      let -- This function is applied to each edge
          iter :: EPair f -> ST s ()
          iter (E (DPair (K node,s))) = do
             let d = fromJust $ dmapFin Vec.! node
             writeSTRef count 0
             K u :*: t <- run d s
             MVec.unsafeWrite umap node u
             MVec.unsafeWrite allEdges node (E t)
          -- Runs the AG on an edge with the given input inherited
          -- attribute value and produces the output synthesised
          -- attribute value along with the rewritten subtree.
          run :: forall j . d -> f (Context f Node) j -> ST s ((K u :*: Context g Node) j)
          run d t = mdo
             -- apply the semantic functions
             let u = explicit syn (u,d) (unK . ffst . unNumbered) result
                 m = explicit inh (u,d) (unK . ffst . unNumbered) result
                 -- recurses into the child nodes and numbers them
                 run' :: NatM (ST s) (Context f Node) (Numbered (K (u,d) :*: Context g Node))
                 run' s = do i <- readSTRef count
                             writeSTRef count $! (i+1)
                             let d' = lookupNumMap d i m
                             K u' :*: t <- runF d' s
                             return (Numbered i (K (u',d') :*: t))
             result <- hmapM run' t
             let t' = appCxt $ hfmap (fsnd . unNumbered) $ explicit rewr (u,d) (unK . ffst . unNumbered) result
             return (K u :*: t')
          -- recurses through the tree structure
          runF :: d -> NatM (ST s) (Context f Node) (K u :*: Context g Node)
          runF d (Term t) = run d t
          runF d (Hole x) = do
             -- we found a node: update the mapping for inherited
             -- attribute values
             old <- MVec.unsafeRead dmap $ unK x
             let new = case old of
                         Just o -> res o d
                         _      -> d
             MVec.unsafeWrite dmap (unK x) (Just new)
             return $ K (umapFin Vec.! unK x) :*: Hole x
      -- first apply to the root
      K u :*: interRoot <- run dFin root
      -- then apply to the edges
      mapM_ iter . fmap (\(k S.:=> v) -> E $ DPair (k,v)) $ M.toList edges
      -- finalise the mappings for attribute values and target DAG
      dmapFin <- Vec.unsafeFreeze dmap
      umapFin <- Vec.unsafeFreeze umap
      allEdgesFin <- Vec.unsafeFreeze allEdges
      return (u, relabelNodes interRoot allEdgesFin nodeCount)


-- | This function runs a synthesised attribute grammar with rewrite function on
-- a dag. The result is the (combined) synthesised attribute at the
-- root of the dag and the rewritten dag.

runSynRewrite :: forall f g u i .(HTraversable f, HTraversable g) =>
       Syn' f u u      -- ^ semantic function of synthesised attributes
    -> Rewrite f u g  -- ^ rewrite function (stateful tree homomorphism)
    -> Dag f i             -- ^ input dag
    -> (u, Dag g i)
runSynRewrite syn rewr Dag {edges,root,nodeCount} = runST runM where
    runM :: forall s . ST s (u, Dag g i)
    runM = mdo
      -- allocate mapping from nodes to synthesised attribute values
      umap <- MVec.new nodeCount
      -- allocate vector to represent edges of the target DAG
      allEdges <- MVec.new nodeCount
      let -- This function is applied to each edge
          iter :: EPair f -> ST s ()
          iter (E (DPair (K node,s))) = do
             K u :*: t <- run s
             MVec.unsafeWrite umap node u
             MVec.unsafeWrite allEdges node (E t)
          -- Runs the AG on an edge with the given input inherited
          -- attribute value and produces the output synthesised
          -- attribute value along with the rewritten subtree.
          run :: NatM (ST s) (f (Context f Node)) (K u :*: Context g Node)
          run t = mdo
             -- apply the semantic functions
             let u = explicit syn u (unK . ffst) result
             result <- hmapM runF t
             let t' = appCxt $ hfmap fsnd $ explicit rewr u (unK . ffst) result
             return (K u :*: t')
          -- recurses through the tree structure
          runF :: NatM (ST s) (Context f Node) (K u :*: Context g Node)
          runF (Term t) = run t
          runF (Hole x) = return $ K (umapFin Vec.! unK x) :*: Hole x
      -- first apply to the root
      K u :*: interRoot <- run root
      -- then apply to the edges
      mapM_ iter . fmap (\(k S.:=> v) -> E $ DPair (k,v)) $ M.toList edges
      -- finalise the mappings for attribute values and target DAG
      umapFin <- Vec.unsafeFreeze umap
      allEdgesFin <- Vec.unsafeFreeze allEdges
      return (u, relabelNodes interRoot allEdgesFin nodeCount)

-- | This function relabels the nodes of the given dag. Parts that are
-- unreachable from the root are discarded. Instead of a 'DMap',
-- edges are represented by a 'Vector'.
relabelNodes :: forall f i . HTraversable f
             => Context f Node i
             -> Vector (E (Cxt Hole f (K Int)))
             -> Int
             -> Dag f i
relabelNodes root edges nodeCount = runST run where
    run :: forall s . ST s (Dag f i)
    run = do
      -- allocate counter for generating nodes
      curNode <- newSTRef 0
      newEdges <- newSTRef M.empty  -- the new graph
      -- construct empty mapping for mapping old nodes to new nodes
      newNodes :: MVector s (Maybe Int) <- MVec.new nodeCount
      MVec.set newNodes Nothing
      let -- Replaces node in the old graph with a node in the new
          -- graph. This function is applied to all nodes reachable
          -- from the given node as well.
          build :: forall j . Node j -> ST s (Node j)
          build node = do
            -- check whether we have already constructed a new node
            -- for the given node
             mnewNode <- MVec.unsafeRead newNodes $ unK node
             case mnewNode of
               Just newNode -> return $ K newNode
               Nothing ->
                   case edges Vec.! unK node of
                     (E (Hole  (K n))) -> do
                       -- We found an edge that just maps to another
                       -- node. We shortcut this edge.
                       newNode <- build $ K n
                       MVec.unsafeWrite newNodes (unK node) (Just $ unK newNode)
                       return newNode
                     E (Term f) -> do
                        -- Create a new node and call build recursively
                       newNode <- readSTRef curNode
                       writeSTRef curNode $! (newNode+1)
                       MVec.unsafeWrite newNodes (unK node) (Just newNode)
                       f' <- hmapM (hmapM build) f
                       modifySTRef newEdges (M.insert (K newNode) f')
                       return $ K newNode
          -- This function is only used for the root. If the root is
          -- only a node, we lookup the mapping for that
          -- node. In any case we apply build to all nodes.
          build' :: Context f Node i -> ST s (f (Context f Node) i)
          build' (Hole n) = do
                         n' <- build n
                         e <- readSTRef newEdges
                         return (e M.! n')
          build' (Term f) = hmapM (hmapM build) f
      -- start relabelling from the root
      root' <- build' root
      -- collect the final edges mapping and node count
      edges' <- readSTRef newEdges
      nodeCount' <- readSTRef curNode
      return Dag {edges = edges', root = root', nodeCount = nodeCount'}
