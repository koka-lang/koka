------------------------------------------------------------------------------
-- Copyright 2012 Microsoft Corporation.
--
-- This is free software; you can redistribute it and/or modify it under the
-- terms of the Apache License, Version 2.0. A copy of the License can be
-- found in the file "license.txt" at the root of this distribution.
-----------------------------------------------------------------------------
{-
    Module that exports the GetOpt library in a portable way
-}
-----------------------------------------------------------------------------
module Platform.GetOptions( -- * GetOpt
                            getOpt, usageInfo
                          , ArgOrder(..)
                          , OptDescr(..)
                          , ArgDescr(..)
                          ) where

import GetOpt
