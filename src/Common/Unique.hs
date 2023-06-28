-----------------------------------------------------------------------------
-- Copyright 2012-2021, Microsoft Research, Daan Leijen.
--
-- This is free software; you can redistribute it and/or modify it under the
-- terms of the Apache License, Version 2.0. A copy of the License can be
-- found in the LICENSE file at the root of this distribution.
-----------------------------------------------------------------------------
{-
    Useful monad classes.
-}
-----------------------------------------------------------------------------
module Common.Unique( -- * Unique
                     HasUnique(updateUnique,setUnique,unique,uniques,uniqueId,uniqueIds,uniqueName,uniqueNameFrom)
                   -- ** Instances
                   , Unique, runUnique, runUniqueWith, liftUnique, withUnique
                   , UniqueT, runUniqueT
                   ) where

import Common.Id   ( Id, genId, idNumber )
import Common.Name
import Control.Monad
import Control.Monad.Trans
import Control.Arrow

class (Monad m, Functor m) => HasUnique m where
  updateUnique :: (Int -> Int) -> m Int
  -- getUnique    :: m Int
  setUnique    :: Int -> m ()
  
  unique  :: m Int
  uniques :: Int -> m [Int]
  uniqueId :: String -> m Id
  uniqueIds :: String -> Int -> m [Id]
  uniqueName :: String -> m Name
  uniqueNameFrom :: Name -> m Name

  -- getUnique
  --  = updateUnique id

  setUnique i
    = updateUnique (const i) >> return ()

  unique
    = updateUnique (+1)

  uniques n
    = mapM (const unique) [1..n]

  uniqueId baseName
    = do i <- unique
         return (genId baseName i)

  uniqueIds baseName n
    = do is <- uniques n
         return (map (genId baseName) is)

  uniqueName baseName
    = do i <- unique
         return (newHiddenName (baseName ++ "." ++ show i))

  uniqueNameFrom baseName
    = do i <- unique
         return (toUniqueName i baseName)

         
{--------------------------------------------------------------------------
  Helper instance for unique variables
--------------------------------------------------------------------------}
newtype Unique a = Unique (Int -> (a,Int))

-- | Run a unique monad that will generate unique identifiers
-- with respect to a given set of identifiers
runUniqueWith :: [Id] -> Unique a -> a
runUniqueWith ids uniq
  = fst (runUnique (foldr max 0 (map idNumber ids) + 1) uniq)

-- | Run a unique monad, starting with an initial unique seed
runUnique :: Int -> Unique a -> (a,Int)
runUnique i (Unique u)
  = u i
  
withUnique :: HasUnique m => (Int -> (a,Int)) -> m a
withUnique f
  = do u <- unique
       let (x,u') = f u
       setUnique u'
       return x


liftUnique :: HasUnique m => Unique a -> m a
liftUnique uniq
  = withUnique (\u -> runUnique u uniq)

instance Functor Unique where
  fmap f (Unique u) = Unique (\i -> case u i of (x,j) -> (f x,j))

instance Applicative Unique where
  pure x = Unique (\i -> (x,i))
  (<*>) = ap

instance Monad Unique where
  -- return = pure 
  (Unique u) >>= f  = Unique (\i -> case u i of
                                      (x,j) -> case f x of
                                                 Unique v -> v j)


instance HasUnique Unique where
  updateUnique f    = Unique (\i -> (i, f i))

newtype UniqueT m a = UniqueT
  { unUniqueT :: Int -> m (a, Int)
  }

runUniqueT :: Int -> UniqueT m a -> m (a, Int)
runUniqueT = flip unUniqueT

instance Monad m => HasUnique (UniqueT m) where
  updateUnique f = UniqueT $ \i -> pure (i, f i)

instance Functor m => Functor (UniqueT m) where
  fmap f (UniqueT u) = UniqueT $ \i -> fmap (first f) (u i)

instance Monad m => Applicative (UniqueT m) where
  pure x = UniqueT $ \i -> pure (x, i)
  (<*>) = ap

instance Monad m => Monad (UniqueT m) where
  return = pure
  UniqueT u >>= f = UniqueT $ \i -> do
    (x, j) <- u i
    unUniqueT (f x) j

instance MonadTrans UniqueT where
  lift m = UniqueT $ \i -> do
    a <- m
    pure (a, i)
