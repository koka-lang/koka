/*---------------------------------------------------------------------------
  Copyright 2026, Tim Whiting, Microsoft Research, Daan Leijen.

  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the LICENSE file at the root of this distribution.
---------------------------------------------------------------------------*/
module uv/timer

import std/num/int64
import core

extern import
  c  file "inline/timer.c"
  js file "inline/timer.js"

pub extern timer-setup( evloop : any, millisecs : int64, cb : () -> io-noexn () ) : io-noexn error<() -> io-noexn ()>
  c  "kk_timer_setup"
  js "kk_timer_setup"