/*---------------------------------------------------------------------------
  Copyright 2026, Tim Whiting, Microsoft Research, Daan Leijen.

  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the LICENSE file at the root of this distribution.
---------------------------------------------------------------------------*/

#ifndef __EMSCRIPTEN__

// Set the thread local uv loop
void kk_set_uv_loop(uv_loop_t* loop) {
  kk_uv_loop_default = loop;
}
// Get the thread local uv loop
uv_loop_t* uvloop() {
  return kk_uv_loop_default;
}

#endif
