{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE DoAndIfThenElse     #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Comp.Dag
    ( Dag
    , termTree
    , reifyDag
    , unravel
    , bisim
    ) where


import Control.Applicative
import Control.Exception.Base
import Control.Monad.State
import Data.Comp.Dag.Internal
import Data.Comp.Term
import qualified Data.HashMap.Lazy as HashMap
import qualified Data.IntMap as IntMap
import Data.IORef
import Data.Foldable (Foldable)
import Data.Traversable (Traversable)
import qualified Data.Traversable as Traversable
import Data.Typeable
import System.Mem.StableName
import Data.Comp.Equality


-- | Turn a term into a graph without sharing.
termTree :: Functor f => Term f -> Dag f
termTree (Term t) = Dag (fmap toCxt t) IntMap.empty 0


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
