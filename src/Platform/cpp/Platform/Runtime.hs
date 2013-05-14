{-# OPTIONS -cpp #-}
------------------------------------------------------------------------------
-- Copyright 2012 Microsoft Corporation.
--
-- This is free software; you can redistribute it and/or modify it under the
-- terms of the Apache License, Version 2.0. A copy of the License can be
-- found in the file "license.txt" at the root of this distribution.
-----------------------------------------------------------------------------
{-
    Module that exports non-standardized functions.
-}
-----------------------------------------------------------------------------
module Platform.Runtime( exCatch
                       , unsafePerformIO
                       , finally                       
                       , copyBinaryFile
                       ) where


import System.IO.Unsafe( unsafePerformIO )
import System.IO.Error ( ioeGetErrorString )

#if __GLASGOW_HASKELL__ > 600
import Control.Exception( finally )
import qualified Control.Exception as Ex

import qualified Data.ByteString as B

exCatch :: IO a -> (String -> IO a) -> IO a
exCatch io handler
  = {- 
    Ex.catch io (\err -> handler (userError (case Ex.userErrors err of
                                               Just msg -> msg
                                               Nothing  -> show err)))
    -}
    Ex.catches io [Ex.Handler (\(Ex.ErrorCall msg)  -> handler msg)
                  ,Ex.Handler (\(err) -> handler (ioeGetErrorString (err :: IOError)))
                  ,Ex.Handler (\(err) -> handler (show (err :: Ex.SomeException)))]


copyBinaryFile :: FilePath -> FilePath -> IO ()
copyBinaryFile src dest
  = do content <- B.readFile src
       B.writeFile dest content

#else
import System.IO( withBinaryFile, hGetContents, hPutStr, IOMode(..) )

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
  = withBinaryFile src ReadMode $ \hsrc ->
    withBinaryFile dest WriteMode $ \hdest ->
    do content <- hGetContents hsrc
       hPutStr hdest content
#endif
