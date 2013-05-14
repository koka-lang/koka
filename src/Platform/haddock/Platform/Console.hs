------------------------------------------------------------------------------
-- Copyright 2012 Microsoft Corporation.
--
-- This is free software; you can redistribute it and/or modify it under the
-- terms of the Apache License, Version 2.0. A copy of the License can be
-- found in the file "license.txt" at the root of this distribution.
-----------------------------------------------------------------------------
{-
    Module for portable control of colors in a console.
    Warning: "Lib.Printer" depends strongly on implicit interface
    assumptions.
-}
-----------------------------------------------------------------------------
module Platform.Console( setColor, setBackColor, setReverse, setUnderline
                       , withConsole, bracketConsole
                       ) where

setColor :: Enum c => c -> IO ()
setColor c      = return ()

setBackColor :: Enum c => c -> IO ()
setBackColor c  = return ()

setReverse :: Bool -> IO ()
setReverse r    = return ()

setUnderline :: Bool -> IO ()
setUnderline u  = return ()

-- | Initialize the console module. Passes 'True' on success.
withConsole :: (Bool -> IO a) -> IO a
withConsole f
  = f False

-- | Restore the console state after a computation
bracketConsole :: IO a -> IO a
bracketConsole io
  = io
