{-# LANGUAGE BangPatterns         #-}
{-# LANGUAGE DeriveDataTypeable   #-}
{-# LANGUAGE DoAndIfThenElse      #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NamedFieldPuns #-}


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

module Data.Comp.Multi.Dag
    ( Dag (..)
    , Dag' (..)
    , HFgeq (..)
    , termTree
    , termTree'
    , simpDag
    , reifyDag
    , unravel
    , bisim
    --, iso
    --, strongIso
    , flatten

    , Node (..)
    , SName (..)
    , TermPair (..)
    ) where

import Control.Exception.Base
import Control.Monad.State
import Data.Comp.Multi.Dag.Internal
import Data.Comp.Multi.Equality
import Data.Comp.Multi.Term
import Data.Comp.Multi.HFunctor
import qualified Data.Dependent.HashMap as DH
import Data.IORef
import Data.Comp.Multi.HFoldable
import Data.Comp.Multi.HTraversable
import Data.Typeable
import System.Mem.StableName

import Control.Monad.ST
import Data.Comp.Multi.Ops
import Data.Comp.Multi.Show
import Data.Comp.Multi.Mapping
import Data.List
import Data.STRef
import Data.Some
import Data.Hashable
import qualified Data.Vector as Vec
import qualified Data.Vector.Generic.Mutable as MVec
import qualified Data.Dependent.Map as M
import qualified Data.Dependent.Sum as S
import Unsafe.Coerce
import Data.GADT.Compare

-- | This class makes Term f and f (Term f) instances of GEq, so they can be used in dags.
class HFgeq (f :: (* ->  *) -> * -> *) where
    -- | Like geq, this function compares both value and type.
    hfgeq :: GEq a => f a i -> f a j -> Maybe (i :~: j)

instance (HFgeq f, HFgeq g) => HFgeq (f :+: g) where
    Inl x `hfgeq` Inl y = case x `hfgeq` y of
                            Just Refl -> Just Refl
                            Nothing   -> Nothing
    Inr x `hfgeq` Inr y = case x `hfgeq` y of
                            Just Refl -> Just Refl
                            Nothing   -> Nothing
    _ `hfgeq` _ = Nothing

instance (HFgeq f, Eq q) => HFgeq (f :&: q) where
    (x :&: q) `hfgeq` (y :&: r) = if q==r then x `hfgeq` y else Nothing

instance {-# OVERLAPPING #-} HFgeq f => GEq (Term f) where
    Term x `geq` Term y = x `geq` y

instance (HFgeq f, GEq a) => GEq (f a) where
    x `geq` y = x `hfgeq` y

instance Show (Node a) where
    show = show . getNode

instance (ShowHF f, HFunctor f) => Show (Dag f i)
  where
    show Dag {root=r, edges=es} = unwords
        [ "mkDag"
        , show  (Term r)
        , showLst ["(" ++ show n ++ "," ++ show (Term f) ++ ")" | (n S.:=>f) <- M.toList es ]
        ]
      where
        showLst ss = "[" ++ intercalate "," ss ++ "]"

instance (ShowHF f, HFunctor f) => Show (Dag' f i)
  where
    show Dag' {root'=r, edges'=es} = unwords
        [ "mkDag'"
        , show  (simpCxt r)
        , showLst ["(" ++ show n ++ "," ++ show (simpCxt f) ++ ")" | (n S.:=>f) <- M.toList es ]
        ]
      where
        showLst ss = "[" ++ intercalate "," ss ++ "]"

-- | Turn a term into a graph without sharing.
termTree :: (Typeable f, HFunctor f, Typeable i) => Term f i -> Dag f i
termTree (Term t) = Dag (hfmap toCxt t) M.empty 0


-- | Turn a term into a flat graph without sharing.
termTree' :: forall f i . (HTraversable f, HFunctor f, Typeable i, Typeable f) => Term f i -> Dag' f i
termTree' (Term t) = Dag' r e n where
    s = number t
    r = hfmap (\(Numbered j _) -> unsafeCoerce (Node j :: Node ())) s
    m = 1+hfoldl ((. getNode) . max) 0 r
    (n, e) = execState (hmapM run s) (m, M.empty)
    run :: forall j . Numbered (Term f) j -> State (Int, Edges' f) (K () j)
    run (Numbered i (Term !t)) = do
        (n, e) <- get
        let t' = hfmap (\(Numbered j _) -> unsafeCoerce (Node $ n+j :: Node ())) $ number t
        let t'' = hfmap (\(Numbered j x) -> Numbered (n+j) x) $ number t
        let e' = M.insert (unsafeCoerce (Node i :: Node ())) t' e
        let (K m) = hfoldl (fmap fmap fmap (K . unK) max . (K . unK)) 0 . hfmap (\(Numbered j _) -> K $ j+1) $ number t
        put (n+m, e')
        hmapM run t''
        return $ K ()

-- | Convert a Dag' to a Dag.
simpDag :: HFunctor f => Dag' f :-> Dag f
simpDag Dag' {root', edges', nodeCount'} = Dag {
                                                 root = hfmap Hole root'
                                               , edges = M.fromList $ (\(a S.:=> b) -> a S.:=> hfmap Hole b) <$> M.toList edges'
                                               , nodeCount = nodeCount'
                                               }

-- | This exception indicates that a 'Term' could not be reified to a
-- 'Dag' (using 'reifyDag') due to its cyclic sharing structure.
data CyclicException = CyclicException
    deriving (Show, Typeable)

instance Exception CyclicException

newtype SName f i = SName {getSName :: StableName (f (Term f) i)}
instance Hashable (SName f i) where
    hashWithSalt s = hashWithSalt 239 . hashWithSalt s . getSName
instance Hashable (Some (SName f)) where
    hashWithSalt i (Some x) = hashWithSalt 789 $ hashWithSalt i x
instance GEq (SName f) where
    a `geq` b = if getSName a == unsafeCoerce (getSName b) then Just $ unsafeCoerce Refl else Nothing
newtype TermPair f i = TermPair {getTermPair :: (Bool, f (SName f) i)}

-- | This function takes a term, and returns a 'Dag' with the implicit
-- sharing of the input data structure made explicit. If the sharing
-- structure of the term is cyclic an exception of type
-- 'CyclicException' is thrown.
--reifyDag :: HTraversable f => NatM IO (Term f) (Dag f)
reifyDag :: forall f i . (HFgeq f, HTraversable f, Typeable f, Typeable i) => Term f i -> IO (Dag f i)
reifyDag m = do
  tabRef <- newIORef DH.empty
  let findNodes :: forall j. Term f j -> IO (SName f j)
      findNodes (Term !j) = do
        st <- liftIO $ makeStableName j
        let stKey = SName st
        tab <- readIORef tabRef
        case DH.lookup stKey tab of
          Just (TermPair (single,f)) | single -> writeIORef tabRef (DH.insert stKey (TermPair (False,f)) tab)
                                      >> return stKey
                          | otherwise -> return stKey
          Nothing -> do res <- hmapM findNodes j
                        tab <- readIORef tabRef
                        if DH.member stKey tab
                          then throwIO CyclicException
                          else writeIORef tabRef (DH.insert stKey (TermPair (True, res)) tab)
                               >> return stKey
  st <- findNodes m
  tab <- readIORef tabRef
  counterRef :: IORef Int <- newIORef 0
  edgesRef <- newIORef M.empty
  nodesRef <- newIORef DH.empty
  let run :: forall j. StableName (f (Term f) j) -> IO (Cxt Hole f Node j)
      run st = do
        let stKey = SName st
        let TermPair (single,f) = tab DH.! stKey
        if single then Term <$> hmapM (run . getSName) f
        else do
          nodes <- readIORef nodesRef
          case DH.lookup stKey nodes of
            Just n -> return (Hole n)
            Nothing -> do
              n <- readIORef counterRef
              writeIORef counterRef $! (n+1)
              let node = unsafeCoerce (Node n :: Node ()) :: Node j
              writeIORef nodesRef (DH.insert stKey node nodes)
              f' <- hmapM (run . getSName) f
              modifyIORef edgesRef (M.insert node f')
              return (Hole node)
  Term root <- run (getSName st)
  edges <- readIORef edgesRef
  count <- readIORef counterRef
  return (Dag root edges count)


-- | This function unravels a given graph to the term it
-- represents.

unravel :: forall f i . (Typeable i, HFunctor f) => Dag f i -> Term f i
unravel Dag {root, edges} = Term $ hfmap build root
    where build :: forall i . Context f Node i -> Term f i
          build (Term t) = Term $ hfmap build t
          build (Hole n) = Term . hfmap build $ edges M.! n

-- | Checks whether two dags are bisimilar. In particular, we have
-- the following equality
--
-- @
-- bisim g1 g2 = (unravel g1 == unravel g2)
-- @
--
-- That is, two dags are bisimilar iff they have the same unravelling.

bisim :: forall f i . (EqHF f, HFunctor f, HFoldable f)  => Dag f i -> Dag f i -> Bool
bisim Dag {root=r1, edges=e1}  Dag {root=r2, edges=e2} = runF r1 r2
    where run :: forall j k . (Context f Node j, Context f Node k) -> Bool
          run (t1, t2) = runF (step e1 t1) (step e2 t2)
          step :: forall j . Edges f -> Context f Node j -> f (Context f Node) j
          step e (Hole n) = e M.! n
          step _ (Term t) = t
          runF :: forall j k . f (Context f Node) j -> f (Context f Node) k -> Bool
          runF f1 f2 = case heqMod f1 f2 of
                         Nothing -> False
                         Just l -> all @[] (\(E x, E y) -> curry run x y) l


-- | Checks whether the two given DAGs are isomorphic.

{-
iso :: (HTraversable f, HFoldable f, EqHF f, Typeable i, Typeable f) => Dag f i -> Dag f i -> Bool
iso g1 g2 = checkIso (fmap (fmap (fmap $ \(E (Node x), E (Node y)) -> (Node x, Node y))) . heqMod) (flatten g1) (flatten g2)


-- | Checks whether the two given DAGs are strongly isomorphic, i.e.
--   their internal representation is the same modulo renaming of
--   nodes.

strongIso :: (HFunctor f, HFoldable f, EqHF f, Typeable i) => Dag f i -> Dag f i -> Bool
strongIso Dag {root=r1, edges=e1, nodeCount=nx1}
          Dag {root=r2, edges=e2, nodeCount=nx2}
              = checkIso (fmap (fmap (fmap $ \(E (Node x), E (Node y)) -> (Node x, Node y))) . checkEq) (r1,e1,nx1) (r2,e2,nx2)
    where checkEq t1 t2 = heqMod (Term t1) (Term t2)
-}



-- | This function flattens the internal representation of a DAG. That
-- is, it turns the nested representation of edges into single layers.

flatten :: forall f i . (HTraversable f, Typeable i, Typeable f) => Dag f i -> Dag' f i
flatten Dag {root, edges, nodeCount} = runST run where
    run :: forall s . ST s (Dag' f i)
    run = do
      count <- newSTRef 0
      nMap :: Vec.MVector s (Maybe (Node i)) <- MVec.new nodeCount
      MVec.set nMap Nothing
      newEdges <- newSTRef M.empty
      let build :: forall j . Context f Node j -> ST s (Node j)
          build (Hole (Node n)) = mkNode n
          build (Term t) = do
            n' <- readSTRef count
            writeSTRef count $! (n'+1)
            t' <- hmapM build t
            let node = unsafeCoerce (Node n' :: Node ()) :: Node j
            modifySTRef newEdges (M.insert node t')
            return node
          mkNode :: forall j . Typeable j => Int -> ST s (Node j)
          mkNode n = do
            mn' <- MVec.unsafeRead nMap n
            case mn' of
              Just (Node n') -> return $ Node n'
              Nothing -> do n' <- readSTRef count
                            writeSTRef count $! (n'+1)
                            MVec.write nMap n (Just $ Node n')
                            return $ Node n'
          buildF (Node n S.:=> t) = do
            n' <- mkNode n
            t' <- hmapM build t
            modifySTRef newEdges (M.insert n' t')
      root' <- hmapM build root
      fmap void $ mapM buildF $ M.toList edges
      edges' <- readSTRef newEdges
      nodeCount' <- readSTRef count
      return Dag' {root', edges', nodeCount'}



{-
-- | Checks whether the two given dag representations are
-- isomorphic. This function is polymorphic in the representation of
-- the edges. The first argument is a function that checks whether two
-- edges have the same labelling and if so, returns the matching pairs
-- of outgoing nodes the two edges point to. Otherwise the function
-- returns 'Nothing'.

checkIso :: forall i j e . (e i -> e j -> Maybe [(Node i,Node j)])
         -> (e i, M.DMap Node e, Int)
         -> (e j, M.DMap Node e, Int) -> Bool
checkIso checkEq (r1,e1,nx1) (r2,e2,nx2) = runST run where
   run :: ST s Bool
   run = do
     -- create empty mapping from nodes in g1 to nodes in g2
     nMap :: Vec.MVector s (Maybe (E Node)) <- MVec.new nx1
     MVec.set nMap Nothing
     -- create empty set of nodes in g2 that are "mapped to" by the
     -- mapping created above
     nSet :: Vec.MVector s Bool <- MVec.new nx2
     MVec.set nSet False
     let checkT t1 t2 = case checkEq t1 t2 of
                          Nothing -> return False
                          Just l -> liftM and $ mapM checkN l
         checkN (n1,n2) = do
           nm' <- MVec.unsafeRead nMap (getNode n1)
           case nm' of
             Just n' -> return (E n2 == n')
             Nothing -> do
               b <- MVec.unsafeRead nSet (getNode n2)
               if b
               -- n2 is already mapped to by another node
               then return False
               -- n2 is not mapped to
               else do
                 -- create mapping from n1 to n2
                 MVec.write nMap (getNode n1) (Just $ E n2)
                 MVec.write nSet (getNode n2) True
                 checkT (e1 M.! n1) (e2 M.! n2)
     checkT r1 r2
-}
