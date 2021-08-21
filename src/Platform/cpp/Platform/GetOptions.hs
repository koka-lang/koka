{-# OPTIONS -cpp #-}
------------------------------------------------------------------------------
-- Copyright 2012-2021, Microsoft Research, Daan Leijen.
--
-- This is free software; you can redistribute it and/or modify it under the
-- terms of the Apache License, Version 2.0. A copy of the License can be
-- found in the LICENSE file at the root of this distribution.
-----------------------------------------------------------------------------
{-
    Export the GetOpt module.
-}
-----------------------------------------------------------------------------
module Platform.GetOptions( 
                      -- * GetOpt
                        getOpt, usageInfo
                      , ArgOrder(..)
                      , OptDescr(..)
                      , ArgDescr(..)
                      ) where

import System.Console.GetOpt 
