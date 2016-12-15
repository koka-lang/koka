------------------------------------------------------------------------------
-- Copyright 2012 Microsoft Corporation.
--
-- This is free software; you can redistribute it and/or modify it under the
-- terms of the Apache License, Version 2.0. A copy of the License can be
-- found in the file "license.txt" at the root of this distribution.
-----------------------------------------------------------------------------
{-
    Module that exports non-standard system functions.
-}
-----------------------------------------------------------------------------
module Platform.Runtime( exCatch
                       , unsafePerformIO
                       , finally
                       , copyBinaryFile
                       ) where

import IO ( ioeGetErrorString )
import IOExts( unsafePerformIO )
import System( ExitCode, system )
import Directory( copyFile )

finally :: IO a -> IO b -> IO a
finally io post
  = do x <- (catch io (\err -> do post; ioError err))
       post
       return x

exCatch :: IO a -> (String -> IO a) -> IO a
exCatch io handler
  = catch io (\err -> handler (ioeGetErrorString err))

copyBinaryFile :: FilePath -> FilePath -> IO ()
copyBinaryFile src dest
  = copyFile src dest -- may cause trouble on windows (since it uses text mode to copy)
