------------------------------------------------------------------------------
-- Copyright 2012-2021, Microsoft Research, Daan Leijen.
--
-- This is free software; you can redistribute it and/or modify it under the
-- terms of the Apache License, Version 2.0. A copy of the License can be
-- found in the LICENSE file at the root of this distribution.
-----------------------------------------------------------------------------
{-
    Debug module to insert 'trace' statements.
-}
-----------------------------------------------------------------------------
module Lib.Trace( trace, traceDoc, ctrace, Color(..), traceShowId, traceShow, traceM, traceShowM, traceId, traceEq ) where

import Lib.Printer
import Lib.PPrint
import Platform.Runtime( unsafePerformIO )
-- import System.Console.Isocline
import System.IO 

trace :: String -> a -> a
trace msg x = ctrace DarkGray msg x 

traceShowM :: (Applicative m, Show s) => s -> m ()
traceShowM msg = traceM (show msg)

traceM :: (Applicative m) => String -> m ()
traceM msg = trace msg (pure ())

traceId :: String -> String
traceId x = trace x x

traceShow :: (Show s) => s -> a -> a
traceShow s = trace $ show s

traceShowId :: (Show a) => a -> a
traceShowId s = trace (show s) s

traceDoc :: Doc -> a -> a
traceDoc msg x = trace (show msg) x

traceEq :: (Show a) => String -> a -> a
traceEq name val = trace (name ++ " = " ++ show val) val

ctrace :: Color -> String -> a -> a
ctrace clr msg x
  = seq (unsafePerformIO $ 
         -- withColorPrinter $ 
         withNoColorPrinter $
         \p -> withColor p clr (writeLn p msg)
         -- hPutStrLn stderr msg
        ) x
