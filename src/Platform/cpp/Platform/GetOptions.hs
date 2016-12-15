{-# OPTIONS -cpp #-}
------------------------------------------------------------------------------
-- Copyright 2012 Microsoft Corporation.
--
-- This is free software; you can redistribute it and/or modify it under the
-- terms of the Apache License, Version 2.0. A copy of the License can be
-- found in the file "license.txt" at the root of this distribution.
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
