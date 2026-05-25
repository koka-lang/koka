/*---------------------------------------------------------------------------
  Copyright 2026, Tim Whiting, Microsoft Research, Daan Leijen.

  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the LICENSE file at the root of this distribution.
---------------------------------------------------------------------------*/
module uv/core

extern import
  c[host=libc] { vcpkg="libuv"; library="uv" }

extern import
  c[host=libc,os=windows] { syslib="user32;iphlpapi;userenv;ws2_32;dbghelp;ole32;uuid" }

extern import
  c[host=wasmweb] header-file "inline/emscripten-uv.h" 

extern import
  c[host=wasmweb] file "inline/emscripten-uv.c" 

extern import
  c header-file "inline/core.h"

extern import
  c file "inline/core.c"

pub extern event-loop-init() : any
  c "kk_uv_loop_init"
  js inline "null"

pub extern event-loop-run(evloop : any) : ()
  c "kk_uv_loop_run"
  js inline "$std_core_types.Unit"
