-----------------------------------------------------------------------------
-- Copyright 2012 Microsoft Corporation.
--
-- This is free software; you can redistribute it and/or modify it under the
-- terms of the Apache License, Version 2.0. A copy of the License can be
-- found in the file "license.txt" at the root of this distribution.
-----------------------------------------------------------------------------
{-
    Finite maps from names to ...
-}
-----------------------------------------------------------------------------
module Common.QNameMap
          ( QNameMap, Lookup(..)
          , empty
          , single
          , fromList
          , lookup, lookupQ
          , insert
          , union
          , unions
          , toAscList
          , isEmpty
          , filterNames
          ) where

import Prelude hiding (lookup)
import qualified Prelude
import qualified Data.Map as M
import Common.Name
import Common.Failure

----------------------------------------------------------------
-- Types
----------------------------------------------------------------
-- | A map from names to values
data QNameMap a = QM !(M.Map Name [(Name,a)])

instance Show a => Show (QNameMap a) where
  show qm
    = show (toAscList qm)

data Lookup a = Found Name a
              | Ambiguous [Name]
              | NotFound

empty :: QNameMap a
empty = QM M.empty

isEmpty :: QNameMap a -> Bool
isEmpty (QM m)
  = M.null m

single :: Name -> a -> QNameMap a
single name x
  = QM (M.singleton (unqualify name) [(name,x)])

fromList :: [(Name,a)] -> QNameMap a
fromList xs 
  = foldl (\qm (name,x) -> insert name x qm) empty xs

-- | Lookup a fully qualified name
lookupQ :: Name -> QNameMap a -> Maybe a
lookupQ name (QM m)
  = case M.lookup (unqualify name) m of
      Nothing -> Nothing
      Just xs -> Prelude.lookup name xs 

-- | Lookup a potentially unqualified name within a module context. 
-- (The module context is ignored if a qualified name is looked up)
lookup :: Name -> Name -> QNameMap a -> Lookup a
lookup context name (QM m)
  = case M.lookup (unqualify name) m of
      Nothing   -> NotFound
      Just [(qname,x)]  | not (isQualified name) -> Found qname x
      Just xs   -> let qname = if isQualified name then name else qualify context name 
                   in case Prelude.filter (\p -> fst p == qname) xs of
                        [(realname,x)]  -> Found realname x
                        _ -> Ambiguous (map fst xs)
                     

filterNames :: (Name -> Bool) -> QNameMap a -> QNameMap a
filterNames pred (QM m)
  = QM (M.map belongs m)
  where
    belongs xs  = [(name,x) | (name,x) <- xs, pred name]
  
insert :: Name -> a -> QNameMap a -> QNameMap a
insert name x (QM m)
  = QM (M.insertWith (safeCombine "insert")  (unqualify name) [(name,x)] m)

union :: QNameMap a -> QNameMap a -> QNameMap a
union (QM m1) (QM m2)
  = QM (M.unionWith (safeCombine "union") m1 m2)


unions :: [QNameMap a] -> QNameMap a
unions qs
  = foldl union empty qs
    
toAscList :: QNameMap a -> [(Name,a)]
toAscList (QM m)
  = concatMap snd (M.toAscList m)


safeCombine :: String -> [(Name,a)] -> [(Name,a)] -> [(Name,a)]    
safeCombine method xs ys
  = let ynames = map fst ys
        xnames = map fst xs
    in if any (`elem` ynames) xnames
        then failure ("Common.QNameMap." ++ method ++ ": overlapping names: " ++ show (xnames,ynames))
        else xs ++ ys

