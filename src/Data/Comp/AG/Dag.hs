{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecursiveDo         #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Comp.AG.Dag where

import Data.Comp.AG
import Data.Comp.Dag
import Data.Comp.Mapping
import Data.Comp.Term
import qualified Data.IntMap as IntMap

import Control.Monad.ST
import Data.Maybe
import Data.STRef
import qualified Data.Vector as Vec
import qualified Data.Vector.Generic.Mutable as MVec

import qualified Data.Traversable as Traversable

import Control.Monad.State

-- | This function runs an attribute grammar on a dag. The result is
-- the (combined) synthesised attribute at the root of the dag.

runAG :: forall f d u .Traversable f
    => (d -> d -> d)   -- ^ resolution function for inherited attributes
    -> Syn' f (u,d) u  -- ^ semantic function of synthesised attributes
    -> Inh' f (u,d) d  -- ^ semantic function of inherited attributes
    -> (u -> d)        -- ^ initialisation of inherited attributes
    -> Dag f           -- ^ input dag
    -> u
runAG res syn inh dinit Dag {edges,root,nodeCount} = uFin where
    uFin = runST runM
    dFin = dinit uFin
    runM :: forall s . ST s u
    runM = mdo
      dmap <- MVec.new nodeCount
      MVec.set dmap Nothing
      umap <- MVec.new nodeCount
      count <- newSTRef 0
      let run :: d -> f (Context f Node) -> ST s u
          run d t = mdo
             let u = explicit syn (u,d) unNumbered result
             let m = explicit inh (u,d) unNumbered result
             writeSTRef count 0
             let run' :: Context f Node -> ST s (Numbered (u,d))
                 run' s = do i <- readSTRef count
                             writeSTRef count $! (i+1)
                             let d' = lookupNumMap d i m
                             u' <- runF d' s
                             return (Numbered i (u',d'))
             result <- Traversable.mapM run' t
             return u
          runF :: d -> Context f Node -> ST s u
          runF d (Hole x) = do
             old <- MVec.unsafeRead dmap x
             let new = case old of
                         Just o -> res o d
                         _      -> d
             MVec.unsafeWrite dmap x (Just new)
             return (umapFin Vec.! x)
          runF d (Term t)  = run d t
      let iter (n, t) = do
            u <- run (fromJust $ dmapFin Vec.! n) t
            MVec.unsafeWrite umap n u
      u <- run dFin root
      mapM_ iter (IntMap.toList edges)
      dmapFin <- Vec.unsafeFreeze dmap
      umapFin <- Vec.unsafeFreeze umap
      return u



-- | This function runs an attribute grammar with rewrite function on
-- a dag. The result is the (combined) synthesised attribute at the
-- root of the dag and the rewritten dag.

runRewrite :: forall f g d u .(Traversable f, Traversable g)
    => (d -> d -> d)       -- ^ resolution function for inherited attributes
    -> Syn' f (u,d) u      -- ^ semantic function of synthesised attributes
    -> Inh' f (u,d) d      -- ^ semantic function of inherited attributes
    -> Rewrite f (u, d) g  -- ^ initialisation of inherited attributes
    -> (u -> d)            -- ^ input term
    -> Dag f
    -> (u, Dag g)
runRewrite res syn inh rewr dinit Dag {edges,root,nodeCount} = result where
    result@(uFin,_) = runST runM
    dFin = dinit uFin
    runM :: forall s . ST s (u, Dag g)
    runM = mdo
      dmap <- MVec.new nodeCount
      MVec.set dmap Nothing
      umap <- MVec.new nodeCount
      count <- newSTRef 0
      allEdges <- MVec.new nodeCount
      let iter (node,s) = do
             let d = fromJust $ dmapFin Vec.! node
             (u,t) <- run d s
             MVec.unsafeWrite umap node u
             MVec.unsafeWrite allEdges node t
          run :: d -> f (Context f Node) -> ST s (u, Context g Node)
          run d t = mdo
             let u = explicit syn (u,d) (fst . unNumbered) result
                 m = explicit inh (u,d) (fst . unNumbered) result
                 run' :: Context f Node -> ST s (Numbered ((u,d), Context g Node))
                 run' s = do i <- readSTRef count
                             writeSTRef count $! (i+1)
                             let d' = lookupNumMap d i m
                             (u',t) <- runF d' s
                             return (Numbered i ((u',d'), t))
             writeSTRef count 0
             result <- Traversable.mapM run' t
             let t' = join $ fmap (snd . unNumbered) $ explicit rewr (u,d) (fst . unNumbered) result
             return (u, t')
          runF d (Term t) = run d t
          runF d (Hole x) = do
             old <- MVec.unsafeRead dmap x
             let new = case old of
                         Just o -> res o d
                         _      -> d
             MVec.unsafeWrite dmap x (Just new)
             return (umapFin Vec.! x, Hole x)
      (u,interRoot) <- run dFin root
      mapM_ iter $ IntMap.toList edges
      dmapFin <- Vec.unsafeFreeze dmap
      umapFin <- Vec.unsafeFreeze umap
      allEdgesFin <- Vec.unsafeFreeze allEdges
      curNode <- newSTRef 0
      edgesref <- newSTRef IntMap.empty  -- the new graph
      newNodes :: Vec.MVector s (Maybe Int) <- MVec.new nodeCount
      MVec.set newNodes Nothing
      let build node = do
             mnewNode <- MVec.unsafeRead newNodes node
             case mnewNode of
               Just newNode -> return newNode
               Nothing ->
                   case allEdgesFin Vec.! node of
                     Hole n -> do
                       newNode <- build n
                       MVec.unsafeWrite newNodes node (Just newNode)
                       return newNode
                     Term f -> do
                       newNode <- readSTRef curNode
                       writeSTRef curNode $! (newNode+1)
                       MVec.unsafeWrite newNodes node (Just newNode)
                       f' <- Traversable.mapM (Traversable.mapM build) f
                       modifySTRef edgesref (IntMap.insert newNode f')
                       return newNode
          build' (Hole n) = do
                         n' <- build n
                         e <- readSTRef edgesref
                         return (e IntMap.! n')
          build' (Term f) = Traversable.mapM (Traversable.mapM build) f
      root' <- build' interRoot
      edges' <- readSTRef edgesref
      nodeCount' <- readSTRef curNode
      return (u, Dag {edges = edges', root = root', nodeCount = nodeCount'})
