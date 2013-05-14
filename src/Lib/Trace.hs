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
module Lib.Trace( trace, traceDoc, ctrace, Color(..) ) where

import Lib.Printer
import Lib.PPrint
import Platform.Runtime( unsafePerformIO )

trace :: String -> a -> a
trace msg x
  = ctrace DarkGray msg x 

traceDoc :: Doc -> a -> a
traceDoc msg x
  = ctrace DarkGray (show msg) x
  
ctrace :: Color -> String -> a -> a
ctrace clr msg x
  = seq (unsafePerformIO (withColorPrinter $ \p -> withColor p clr (writeLn p msg))) x
