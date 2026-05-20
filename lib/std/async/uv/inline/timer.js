/*---------------------------------------------------------------------------
  Copyright 2026, Tim Whiting, Microsoft Research, Daan Leijen.

  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the LICENSE file at the root of this distribution.
---------------------------------------------------------------------------*/

export function kk_timer_setup( evloop, millisecs, cb ) {
  var h = setTimeout( cb, Number(millisecs) );
  return $std_core_types.Ok(() => clearTimeout(h));
}