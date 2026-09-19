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

export function kk_timer_setup_repeat( evloop, millisecs, cb ) {
  var h = setInterval( cb, Number(millisecs) );
  return $std_core_types.Ok(() => clearInterval(h));
}

export function kk_immediate_setup( evloop, cb ) {
  if (typeof setImmediate === 'function') {
    var h = setImmediate( cb );
    return $std_core_types.Ok(() => clearImmediate(h));
  }
  else {
    return kk_timer_setup(evloop,0,cb);
  }
}
