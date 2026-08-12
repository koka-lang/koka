/*---------------------------------------------------------------------------
  Copyright 2026, Tim Whiting, Microsoft Research, Daan Leijen.

  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the LICENSE file at the root of this distribution.
---------------------------------------------------------------------------*/
module uv/evloop

import std/num/int64

extern import
  c[host=libc] { vcpkg="libuv"; library="uv" }

extern import
  c[host=libc,os=windows] { syslib="user32;iphlpapi;userenv;ws2_32;dbghelp;ole32;uuid" }

extern import
  c { header-file="evloop.h"; file="evloop.c" }

extern import
  c  file "evloop-timer.c"
  js file "evloop-timer.js"

pub extern event-loop-init() : any
  c "kk_uv_loop_init"
  js inline "null"

pub extern event-loop-run(evloop : any) : ()
  c "kk_uv_loop_run"
  js inline "$std_core_types.Unit"

pub extern timer-setup( evloop : any, millisecs : int64, cb : () -> ioc () ) : ioc error<() -> ioc ()>
  c  "kk_timer_setup"
  js "kk_timer_setup"

pub extern timer-setup-repeat( evloop : any, millisecs : int64, cb : () -> ioc () ) : ioc error<() -> ioc ()>
  c  "kk_timer_setup_repeat"
  js "kk_timer_setup_repeat"

pub extern immediate-setup( evloop : any, cb : () -> ioc () ) : ioc error<() -> ioc ()>
  c  "kk_immediate_setup"
  js "kk_immediate_setup"
