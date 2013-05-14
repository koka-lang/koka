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
module Common.NameMap
          ( NameMap, module Data.Map
          , find
          ) where

import Data.Map
import Common.Name
import Common.Failure

----------------------------------------------------------------
-- Types
----------------------------------------------------------------
-- | A map from names to values
type NameMap a = Map Name a

find :: Name -> NameMap a -> a
find name nameMap
  = case Data.Map.lookup name nameMap of
      Just x -> x
      Nothing -> failure ("Common.NameMap.find: could not find: " ++ show name)