-----------------------------------------------------------------------------
-- Copyright 2012 Microsoft Corporation.
--
-- This is free software; you can redistribute it and/or modify it under the
-- terms of the Apache License, Version 2.0. A copy of the License can be
-- found in the file "license.txt" at the root of this distribution.
-----------------------------------------------------------------------------
{-
    Useful monad classes.
-}
-----------------------------------------------------------------------------
module Common.Unique( -- * Unique
                     HasUnique(updateUnique,setUnique,unique,uniques,uniqueId,uniqueIds,uniqueName)
                   -- ** Instances
                   , Unique, runUnique, runUniqueWith, liftUnique
                   ) where

import Common.Id   ( Id, genId, idNumber )
import Common.Name
import Control.Monad
import Control.Applicative

instance Applicative Unique where
  pure  = return
  (<*>) = ap

class (Monad m, Functor m) => HasUnique m where
  updateUnique :: (Int -> Int) -> m Int
  -- getUnique    :: m Int
  setUnique    :: Int -> m ()

  unique  :: m Int
  uniques :: Int -> m [Int]
  uniqueId :: String -> m Id
  uniqueIds :: String -> Int -> m [Id]
  uniqueName :: String -> m Name

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


liftUnique :: HasUnique m => Unique a -> m a
liftUnique uniq
  = do u <- unique
       let (x,u') = runUnique u uniq
       setUnique u'
       return x

instance Functor Unique where
  fmap f (Unique u) = Unique (\i -> case u i of (x,j) -> (f x,j))

instance Monad Unique where
  return x          = Unique (\i -> (x,i))
  (Unique u) >>= f  = Unique (\i -> case u i of
                                      (x,j) -> case f x of
                                                 Unique v -> v j)


instance HasUnique Unique where
  updateUnique f    = Unique (\i -> (i, f i))

