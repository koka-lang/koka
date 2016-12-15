-----------------------------------------------------------------------------
-- Copyright 2012 Microsoft Corporation.
--
-- This is free software; you can redistribute it and/or modify it under the
-- terms of the Apache License, Version 2.0. A copy of the License can be
-- found in the file "license.txt" at the root of this distribution.
-----------------------------------------------------------------------------
{-
    Maps from identifiers to values.
-}
-----------------------------------------------------------------------------
module Common.IdMap
          ( IdMap, module Data.IntMap
          ) where

import Data.IntMap

----------------------------------------------------------------
-- Types
----------------------------------------------------------------
-- | A map from identifiers to values
type IdMap a = IntMap a
