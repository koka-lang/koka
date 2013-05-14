-----------------------------------------------------------------------------
-- Copyright 2012 Microsoft Corporation.
--
-- This is free software; you can redistribute it and/or modify it under the
-- terms of the Apache License, Version 2.0. A copy of the License can be
-- found in the file "license.txt" at the root of this distribution.
-----------------------------------------------------------------------------
{-
    Sets of identifiers.
-}
-----------------------------------------------------------------------------
module Common.IdSet
          ( IdSet, module Data.IntSet
          ) where

import Data.IntSet

----------------------------------------------------------------
-- Types
----------------------------------------------------------------
-- | A set of identifiers
type IdSet = IntSet
