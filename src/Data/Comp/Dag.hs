{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE DoAndIfThenElse     #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}


--------------------------------------------------------------------------------
-- |
-- Module      :  Data.Comp.Dag
-- Copyright   :  (c) 2014 Patrick Bahr, Emil Axelsson
-- License     :  BSD3
-- Maintainer  :  Patrick Bahr <paba@di.ku.dk>
-- Stability   :  experimental
-- Portability :  non-portable (GHC Extensions)
--
-- This module implements a representation of directed acyclic graphs
-- (DAGs) as compact representations of trees (or 'Term's).
--
--------------------------------------------------------------------------------

module Data.Comp.Dag
    ( Dag (..)
    , Dag' (..)
    , termTree
    , termTree'
    , simpDag
    , reifyDag
    , unravel
    , flatten
    , bisim
    , iso
    , strongIso
    ) where

import Control.Exception.Base
import Control.Monad.State
import Data.Comp.Dag.Internal
import Data.Comp.Equality
import Data.Comp.Term
import Data.Foldable (Foldable)
import qualified Data.HashMap.Lazy as HashMap
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.IORef
import Data.Traversable (Traversable)
import qualified Data.Traversable as Traversable
import Data.Typeable
import System.Mem.StableName

import Control.Monad.ST
import Data.Comp.Show
import Data.Comp.Mapping
import Data.List
import Data.STRef
import qualified Data.Vector as Vec
import qualified Data.Vector.Generic.Mutable as MVec

instance (ShowF f, Functor f) => Show (Dag f)
  where
    show (Dag r es _) = unwords
        [ "mkDag"
        , show  (Term r)
        , showLst ["(" ++ show n ++ "," ++ show (Term f) ++ ")" | (n,f) <- IntMap.toList es ]
        ]
      where
        showLst ss = "[" ++ intercalate "," ss ++ "]"


instance (ShowF f, Functor f) => Show (Dag' f)
  where
    show (Dag' r es _) = unwords
        [ "mkDag'"
        , show $ simpCxt r
        , showLst ["(" ++ show n ++ "," ++ show (simpCxt f) ++ ")" | (n,f) <- IntMap.toList es ]
        ]
      where
        showLst ss = "[" ++ intercalate "," ss ++ "]"


-- | Turn a term into a graph without sharing.
termTree :: Functor f => Term f -> Dag f
termTree (Term t) = Dag (fmap toCxt t) IntMap.empty 0

-- | Turn a term into a flat graph without sharing.
termTree' :: forall f . (Traversable f, Functor f) => Term f -> Dag' f
termTree' (Term t) = Dag' r e n where
    s = number t
    r = fmap (\(Numbered j _) -> j) s
    m = 1+foldl' max 0 r
    (n, e) = execState (mapM_ run s) (m, IntMap.empty)
    run :: Numbered (Term f) -> State (Int, Edges' f) ()
    run (Numbered i (Term !t)) = do
        (n, e) <- get
        let t' = (\(Numbered j _) -> n+j) <$> number t
        let t'' = (\(Numbered j x) -> Numbered (n+j) x) <$> number t
        let e' = IntMap.insert i t' e
        let m = foldl' max 0 . fmap (\(Numbered j _) -> j+1) $ number t
        put (n+m, e')
        mapM_ run t''

-- | Convert a Dag' to a Dag.
simpDag :: Functor f => Dag' f -> Dag f
simpDag (Dag' r e n) = Dag (fmap Hole r) (fmap Hole <$> e) n

-- | This exception indicates that a 'Term' could not be reified to a
-- 'Dag' (using 'reifyDag') due to its cyclic sharing structure.
data CyclicException = CyclicException
    deriving (Show, Typeable)

instance Exception CyclicException

-- | This function takes a term, and returns a 'Dag' with the implicit
-- sharing of the input data structure made explicit. If the sharing
-- structure of the term is cyclic an exception of type
-- 'CyclicException' is thrown.
reifyDag :: Traversable f => Term f -> IO (Dag f)
reifyDag m = do
  tabRef <- newIORef HashMap.empty
  let findNodes (Term !j) = do
        st <- liftIO $ makeStableName j
        tab <- readIORef tabRef
        case HashMap.lookup st tab of
          Just (single,f) | single -> writeIORef tabRef (HashMap.insert st (False,f) tab)
                                      >> return st
                          | otherwise -> return st
          Nothing -> do res <- Traversable.mapM findNodes j
                        tab <- readIORef tabRef
                        if HashMap.member st tab
                          then throwIO CyclicException
                          else writeIORef tabRef (HashMap.insert st (True,res) tab)
                               >> return st
  st <- findNodes m
  tab <- readIORef tabRef
  counterRef <- newIORef 0
  edgesRef <- newIORef IntMap.empty
  nodesRef <- newIORef HashMap.empty
  let run st = do
        let (single,f) = tab HashMap.! st
        if single then Term <$> Traversable.mapM run f
        else do
          nodes <- readIORef nodesRef
          case HashMap.lookup st nodes of
            Just n -> return (Hole n)
            Nothing -> do
              n <- readIORef counterRef
              writeIORef counterRef $! (n+1)
              writeIORef nodesRef (HashMap.insert st n nodes)
              f' <- Traversable.mapM run f
              modifyIORef edgesRef (IntMap.insert n f')
              return (Hole n)
  Term root <- run st
  edges <- readIORef edgesRef
  count <- readIORef counterRef
  return (Dag root edges count)


-- | This function unravels a given graph to the term it
-- represents.

unravel :: forall f. Functor f => Dag f -> Term f
unravel Dag {edges, root} = Term $ build <$> root
    where build :: Context f Node -> Term f
          build (Term t) = Term $ build <$> t
          build (Hole n) = Term $ build <$> edges IntMap.! n

-- | Checks whether two dags are bisimilar. In particular, we have
-- the following equality
--
-- @
-- bisim g1 g2 = (unravel g1 == unravel g2)
-- @
--
-- That is, two dags are bisimilar iff they have the same unravelling.

bisim :: forall f . (EqF f, Functor f, Foldable f)  => Dag f -> Dag f -> Bool
bisim Dag {root=r1,edges=e1}  Dag {root=r2,edges=e2} = runF r1 r2
    where run :: (Context f Node, Context f Node) -> Bool
          run (t1, t2) = runF (step e1 t1) (step e2 t2)
          step :: Edges f -> Context f Node -> f (Context f Node)
          step e (Hole n) = e IntMap.! n
          step _ (Term t) = t
          runF :: f (Context f Node) -> f (Context f Node) -> Bool
          runF f1 f2 = case eqMod f1 f2 of
                         Nothing -> False
                         Just l -> all run l


-- | Checks whether the two given DAGs are isomorphic.

iso :: (Traversable f, Foldable f, EqF f) => Dag f -> Dag f -> Bool
iso g1 g2 = checkIso eqMod (let Dag' {root', edges', nodeCount'} = flatten g1 in (root', edges', nodeCount'))
                           (let Dag' {root', edges', nodeCount'} = flatten g2 in (root', edges', nodeCount'))


-- | Checks whether the two given DAGs are strongly isomorphic, i.e.
--   their internal representation is the same modulo renaming of
--   nodes.

strongIso :: (Functor f, Foldable f, EqF f) => Dag f -> Dag f -> Bool
strongIso Dag {root=r1,edges=e1,nodeCount=nx1}
          Dag {root=r2,edges=e2,nodeCount=nx2}
              = checkIso checkEq (r1,e1,nx1) (r2,e2,nx2)
    where checkEq t1 t2 = eqMod (Term t1) (Term t2)



-- | This function flattens the internal representation of a DAG. That
-- is, it turns the nested representation of edges into single layers.

flatten :: forall f . Traversable f => Dag f -> Dag' f
flatten Dag {root,edges,nodeCount} = runST run where
    run :: forall s . ST s (Dag' f)
    run = do
      count <- newSTRef 0
      nMap :: Vec.MVector s (Maybe Node) <- MVec.new nodeCount
      MVec.set nMap Nothing
      newEdges <- newSTRef IntMap.empty
      let build :: Context f Node -> ST s Node
          build (Hole n) = mkNode n
          build (Term t) = do
            n' <- readSTRef count
            writeSTRef count $! (n'+1)
            t' <- Traversable.mapM build t
            modifySTRef newEdges (IntMap.insert n' t')
            return n'
          mkNode n = do
            mn' <- MVec.unsafeRead nMap n
            case mn' of
              Just n' -> return n'
              Nothing -> do n' <- readSTRef count
                            writeSTRef count $! (n'+1)
                            MVec.unsafeWrite nMap n (Just n')
                            return n'
          buildF (n,t) = do
            n' <- mkNode n
            t' <- Traversable.mapM build t
            modifySTRef newEdges (IntMap.insert n' t')
      root' <- Traversable.mapM build root
      mapM_ buildF $ IntMap.toList edges
      edges' <- readSTRef newEdges
      nodeCount' <- readSTRef count
      return Dag' {root', edges', nodeCount'}



-- | Checks whether the two given dag representations are
-- isomorphic. This function is polymorphic in the representation of
-- the edges. The first argument is a function that checks whether two
-- edges have the same labelling and if so, returns the matching pairs
-- of outgoing nodes the two edges point to. Otherwise the function
-- returns 'Nothing'.

checkIso :: (e -> e -> Maybe [(Node,Node)])
         -> (e, IntMap e, Int)
         -> (e, IntMap e, Int) -> Bool
checkIso checkEq (r1,e1,nx1) (r2,e2,nx2) = runST run where
   run :: ST s Bool
   run = do
     -- create empty mapping from nodes in g1 to nodes in g2
     nMap :: Vec.MVector s (Maybe Node) <- MVec.new nx1
     MVec.set nMap Nothing
     -- create empty set of nodes in g2 that are "mapped to" by the
     -- mapping created above
     nSet :: Vec.MVector s Bool <- MVec.new nx2
     MVec.set nSet False
     let checkT t1 t2 = case checkEq t1 t2 of
                          Nothing -> return False
                          Just l -> liftM and $ mapM checkN l
         checkN (n1,n2) = do
           nm' <- MVec.unsafeRead nMap n1
           case nm' of
             Just n' -> return (n2 == n')
             _ -> do
               b <- MVec.unsafeRead nSet n2
               if b
               -- n2 is already mapped to by another node
               then return False
               -- n2 is not mapped to
               else do
                 -- create mapping from n1 to n2
                 MVec.unsafeWrite nMap n1 (Just n2)
                 MVec.unsafeWrite nSet n2 True
                 checkT (e1 IntMap.! n1) (e2 IntMap.! n2)
     checkT r1 r2
