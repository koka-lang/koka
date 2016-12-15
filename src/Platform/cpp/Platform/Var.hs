{-# OPTIONS -cpp #-}
------------------------------------------------------------------------------
-- Copyright 2012 Microsoft Corporation.
--
-- This is free software; you can redistribute it and/or modify it under the
-- terms of the Apache License, Version 2.0. A copy of the License can be
-- found in the file "license.txt" at the root of this distribution.
-----------------------------------------------------------------------------
{-
    Module that exports non-standardized 'MVar's.
-}
-----------------------------------------------------------------------------
module Platform.Var( Var, newVar, takeVar, putVar
                   ) where


#if __GLASGOW_HASKELL__ > 600
import Control.Concurrent.MVar

type Var a  = MVar a

newVar :: a -> IO (Var a)
newVar  = newMVar

takeVar :: Var a -> IO a
takeVar = takeMVar

putVar :: Var a -> a -> IO ()
putVar  = putMVar

#else
import Data.IORef

type Var a = IORef a

newVar :: a -> IO (Var a)
newVar   = newIORef

putVar :: Var a -> a -> IO ()
putVar   = writeIORef

takeVar :: Var a -> IO a
takeVar  = readIORef
#endif
