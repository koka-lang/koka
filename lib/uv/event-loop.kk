/*---------------------------------------------------------------------------
  Copyright 2023 Tim Whiting.

  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the LICENSE file at the root of this distribution.
---------------------------------------------------------------------------*/

module uv/event-loop

pub import std/time/duration
pub import std/time/timestamp
pub import uv/utils
pub import uv/timer
import std/num/int32

extern import
  c { conan="libuv[>=1.47.0]"; vcpkg="libuv"; library="uv" }
// TODO: wasm {}

extern import
  c file "inline/event-loop.c"
  cs file "inline/event-loop.cs"

// Sets a timeout for libuv / wasm / javscript / C# event loops
pub extern set-timeout( cb : () -> io-noexn (), ms : int32 ) : io-noexn any
  cs "_Async.SetTimeout"
  js "setTimeout"
  c "kk_set_timeout"

// Clears a timeout for libuv / wasm / javscript / C# event loops
pub extern clear-timeout( tid : any) : io-noexn ()
  cs "_Async.ClearTimeout"
  js "clearTimeout"
  c "kk_clear_timeout"

// Handles a uv loop
pub fun default-event-loop(action)
  if host() == "libc" then
    handle-loop(action)
  else // TODO: Support event loop on other platforms
    action()

// Configures uv with Koka's allocators when importing this file
val @initialize = init-uv-alloc()

// Runs a UV loop
fun handle-loop(action)
  init-loop()
  val res = action()
  run-loop()
  close-loop()
  res

// Runs a uv loop (or the emscripten loop on wasm)
extern run-loop(): io-noexn ()
  c "kk_uv_loop_run"
  wasm "kk_emscripten_loop_run"
  js inline ""
  cs inline ""

// Initilizes a uv loop
extern init-loop(): io-noexn ()
  c "kk_uv_loop_init"
  wasm inline "kk_Unit"
  js inline ""
  cs inline ""

// initializes only the allocators (not an event loop)
// needed for some file operations which alloc - kk_uv_fs_mkdtemp, kk_uv_fs_mkstemp
pub extern init-uv-alloc(): ()
  c "kk_uv_alloc_init"
  wasm inline "kk_Unit"
  cs inline ""
  js inline ""

// Closes a uv loop
extern close-loop(): io-noexn ()
  c "kk_uv_loop_close"
  wasm inline "kk_Unit"
  cs inline ""
  js inline ""

// Closes a uv handle
pub extern close(hnd: uv-handle, callback: () -> io-noexn ()): io-noexn ()
  c "kk_uv_close"
  wasm inline "kk_Unit"
  cs inline ""
  js inline ""
