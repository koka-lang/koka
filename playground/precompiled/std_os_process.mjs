// Koka generated module: std/os/process, koka version: 3.2.4
"use strict";
 
// imports
import * as $std_core_types from './std_core_types.mjs';
import * as $std_core_hnd from './std_core_hnd.mjs';
import * as $std_core_exn from './std_core_exn.mjs';
import * as $std_core_bool from './std_core_bool.mjs';
import * as $std_core_order from './std_core_order.mjs';
import * as $std_core_char from './std_core_char.mjs';
import * as $std_core_int from './std_core_int.mjs';
import * as $std_core_vector from './std_core_vector.mjs';
import * as $std_core_string from './std_core_string.mjs';
import * as $std_core_sslice from './std_core_sslice.mjs';
import * as $std_core_list from './std_core_list.mjs';
import * as $std_core_maybe from './std_core_maybe.mjs';
import * as $std_core_maybe2 from './std_core_maybe2.mjs';
import * as $std_core_either from './std_core_either.mjs';
import * as $std_core_result from './std_core_result.mjs';
import * as $std_core_tuple from './std_core_tuple.mjs';
import * as $std_core_lazy from './std_core_lazy.mjs';
import * as $std_core_show from './std_core_show.mjs';
import * as $std_core_debug from './std_core_debug.mjs';
import * as $std_core_delayed from './std_core_delayed.mjs';
import * as $std_core_console from './std_core_console.mjs';
import * as $std_core from './std_core.mjs';
import * as $std_os_path from './std_os_path.mjs';
 
// externals
 
// type declarations
 
// declarations
 
 
// Run a command in the shell and return its output as a string.
export function run_system_read(cmd) /* (cmd : string) -> io error<string> */  {
  return ((function() {
    try {
      return $std_core._unsupported_external("std/os/process/@extern-run-system-read");
    }
    catch(_err){ return $std_core_exn._throw_exception(_err); }
  })());
}
 
 
// Run a command in the shell
export function run_system(cmd) /* (cmd : string) -> io int */  {
  return ((function() {
    try {
      return $std_core._unsupported_external("std/os/process/@extern-run-system");
    }
    catch(_err){ return $std_core_exn._throw_exception(_err); }
  })());
}