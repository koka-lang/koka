/*---------------------------------------------------------------------------
  Copyright 2026, Microsoft Research, Daan Leijen, Tim Whiting.

  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the LICENSE file at the root of this distribution.
---------------------------------------------------------------------------*/
module uv/file

import std/num/int32
import std/num/int64
import uv/core

extern import
  c  file "inline/file.c"
  js file "inline/file.js" 

pub extern fd-flags-from-filemode( fmode : int32 ) : int32
  c  "kk_uv_fd_flags_from_filemode"
  js "kk_fd_flags_from_filemode"
  
pub extern fd-open-setup( evloop : any, fpath : string, flags : int32, mode : int32, cb : error<ssize_t> -> ioc () ) : ioc error<() -> ioc ()>
  c  "kk_uv_fd_open_setup"
  js "kk_fd_open_setup" 

pub extern fd-close-setup( evloop : any, fd : ssize_t, cb : error<ssize_t> -> ioc () ) : ioc error<() -> ioc ()>
  c  "kk_uv_fd_close"
  js "kk_fd_close_setup"

pub extern fd-read-setup( evloop : any, fd : ssize_t, buf : bytes, offset : int64, cb : error<ssize_t> -> ioc () ) : ioc error<() -> ioc ()>
  c  "kk_uv_fd_read"  
  js "kk_fd_read_setup"  

