/*---------------------------------------------------------------------------
  Copyright 2026, Microsoft Research, Daan Leijen, Tim Whiting.

  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the LICENSE file at the root of this distribution. 
---------------------------------------------------------------------------*/

module uv/file

import std/num/int32
import uv/core

extern import
  c  file "inline/file.c"

pub val fnctl_o_rdonly  : int32 = 0x000.int32
pub val fnctl_o_wronly  : int32 = 0x001.int32
pub val fnctl_o_rdwr    : int32 = 0x002.int32
pub val fnctl_o_append  : int32 = 0x008.int32
pub val fnctl_o_sync    : int32 = 0x080.int32
pub val fnctl_o_nofollow: int32 = 0x100.int32
pub val fnctl_o_creat   : int32 = 0x200.int32
pub val fnctl_o_trunc   : int32 = 0x400.int32
pub val fnctl_o_excl    : int32 = 0x800.int32
  
pub extern file-open-setup( evloop : any, fpath : string, flags : int32, mode : int32, cb : error<ssize_t> -> ioc () ) : ioc error<() -> ioc ()>
  c "kk_uv_file_open_setup" 

pub extern file-close( evloop : any, fhandle : ssize_t, cb : error<ssize_t> -> ioc () ) : ioc error<() -> ioc ()>
  c "kk_uv_file_close"

