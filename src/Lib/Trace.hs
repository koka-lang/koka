------------------------------------------------------------------------------
-- Copyright 2012 Microsoft Corporation.
--
-- This is free software; you can redistribute it and/or modify it under the
-- terms of the Apache License, Version 2.0. A copy of the License can be
-- found in the file "license.txt" at the root of this distribution.
-----------------------------------------------------------------------------
{-
    Debug module to insert 'trace' statements.
-}
-----------------------------------------------------------------------------
module Lib.Trace( trace, traceDoc, ctrace, Color(..), traceShowId, traceShow, traceM, traceShowM ) where

import Lib.Printer
import Lib.PPrint
import Platform.Runtime( unsafePerformIO )

trace :: String -> a -> a
trace msg x = ctrace DarkGray msg x 

traceShowM :: (Applicative m, Show s) => s -> m ()
traceShowM msg = traceM (show msg)

traceM :: (Applicative m) => String -> m ()
traceM msg = trace msg (pure ())

traceShow :: (Show s) => s -> a -> a
traceShow s = trace $ show s

traceShowId :: (Show a) => a -> a
traceShowId s = trace (show s) s

traceDoc :: Doc -> a -> a
traceDoc msg x = trace (show msg) x
  
ctrace :: Color -> String -> a -> a
ctrace clr msg x
  = seq (unsafePerformIO $ 
         -- withColorPrinter $ 
         withNoColorPrinter $
         \p -> withColor p clr (writeLn p msg)
        ) x
