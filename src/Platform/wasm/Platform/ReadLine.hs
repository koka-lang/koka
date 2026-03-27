------------------------------------------------------------------------------
-- Copyright 2012-2021, Microsoft Research, Daan Leijen.
--
-- This is free software; you can redistribute it and/or modify it under the
-- terms of the Apache License, Version 2.0. A copy of the License can be
-- found in the LICENSE file at the root of this distribution.
-----------------------------------------------------------------------------
{-
    Module that exports readline functionality.
    (GHC JavaScript backend variant: all functions are stubs)
-}
-----------------------------------------------------------------------------
module Platform.ReadLine( withReadLine, readLine, readLineEx, addHistory
                        ) where

import Common.ColorScheme

withReadLine :: FilePath -> IO a -> IO a
withReadLine _ io = io

readLine :: ColorScheme -> [FilePath] -> [String] -> [(String,String)] -> String -> IO (Maybe String)
readLine _ _ _ _ _ = return Nothing

readLineEx :: ColorScheme -> [FilePath] -> [String] -> [(String,String)] -> String -> IO () -> IO (Maybe String)
readLineEx _ _ _ _ _ _ = return Nothing

addHistory :: String -> IO ()
addHistory _ = return ()
