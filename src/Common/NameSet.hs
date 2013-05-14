-----------------------------------------------------------------------------
-- Copyright 2012 Microsoft Corporation.
--
-- This is free software; you can redistribute it and/or modify it under the
-- terms of the Apache License, Version 2.0. A copy of the License can be
-- found in the file "license.txt" at the root of this distribution.
-----------------------------------------------------------------------------
{-
    Set of names.
-}
-----------------------------------------------------------------------------
module Common.NameSet
          ( NameSet, module Data.Set
          ) where

import Data.Set
import Common.Name

----------------------------------------------------------------
-- Types
----------------------------------------------------------------
-- | A set of names
type NameSet = Set Name
