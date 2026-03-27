------------------------------------------------------------------------------
-- Copyright 2012-2021, Microsoft Research, Daan Leijen.
--
-- This is free software; you can redistribute it and/or modify it under the
-- terms of the Apache License, Version 2.0. A copy of the License can be
-- found in the LICENSE file at the root of this distribution.
-----------------------------------------------------------------------------
{-
    Module for portable control of colors in a console.
    Warning: "Lib.Printer" depends strongly on implicit interface
    assumptions.
    (GHC JavaScript backend variant: all color functions are no-ops)
-}
-----------------------------------------------------------------------------
module Platform.Console( setColor, setBackColor, setReverse, setUnderline
                       , withConsole, bracketConsole
                       , getProgramPath
                       , termWrite, termWriteLn, termFlush, withTerm
                       ) where

import System.IO ( hFlush, stdout )

setColor :: Enum c => c -> IO ()
setColor _ = return ()

setBackColor :: Enum c => c -> IO ()
setBackColor _ = return ()

setReverse :: Bool -> IO ()
setReverse _ = return ()

setUnderline :: Bool -> IO ()
setUnderline _ = return ()

-- | Initialize the console module. Passes 'True' on success.
withConsole :: (Bool -> IO a) -> IO a
withConsole f = f False

-- | Restore the console state after a computation
bracketConsole :: IO a -> IO a
bracketConsole io = io

-- | Retrieve the path to the currently executing program.
getProgramPath :: IO String
getProgramPath = return "/koka"

-- Terminal output stubs (Isocline not available on WASM)
termWrite :: String -> IO ()
termWrite s = putStr s

termWriteLn :: String -> IO ()
termWriteLn s = putStrLn s

termFlush :: IO ()
termFlush = hFlush stdout

withTerm :: IO a -> IO a
withTerm io = io
