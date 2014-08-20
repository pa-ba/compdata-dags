{-# LANGUAGE BangPatterns       #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DoAndIfThenElse    #-}

module Data.Comp.Dag
    ( Dag
    , termTree
    , reifyDag
    ) where


import Control.Applicative
import Control.Exception.Base
import Control.Monad.State
import Data.Comp.Dag.Internal
import Data.Comp.Term
import qualified Data.HashMap.Lazy as HashMap
import qualified Data.IntMap as IntMap
import Data.IORef
import Data.Traversable (Traversable)
import qualified Data.Traversable as Traversable
import Data.Typeable
import System.Mem.StableName

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
