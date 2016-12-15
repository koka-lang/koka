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


newtype Var a = Var a

newVar :: a -> IO (Var a)
newVar x  = return (Var x)

putVar :: Var a -> a -> IO ()
putVar v x  = return ()

takeVar :: Var a -> IO a
takeVar v = error "haddock.Platform.Var.takeVar: undefined" 
