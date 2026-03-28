// Koka generated module: std/os/readline, koka version: 3.2.4
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
 
// externals
/*---------------------------------------------------------------------------
  Copyright 2020-2021, Microsoft Research, Daan Leijen.
  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the LICENSE file at the root of this distribution.
---------------------------------------------------------------------------*/
var sync_readline_err = function(){ 
  return $std_core_exn._error_from_exception(new Error("readline is not supported")); 
}
if ($std_core.host()=="node")
{
  var fs = await import("fs");
  sync_readline_err = function(){
    try {
      const buffer = Buffer.alloc(1024);
      const n = fs.readSync(0, buffer);
      var   input = buffer.toString("utf8", 0, n); 
      const len = input.length;
      if (input.endsWith("\r\n")) {
        input = input.substr(0,len-2);
      }
      else if (input.endsWith("\n")) {
        input = input.substr(0,len-1);
      }
      // console.log("read: " + input);
      return $std_core_exn.Ok( input );
    }
    catch(exn) {
      return $std_core_exn._error_from_exception(exn);
    }
  }  
}
 
// type declarations
 
// declarations
 
export function readline_err() /* () -> console/console error<string> */  {
  return sync_readline_err();
}
 
 
// Read a line of input synchronously from stdin (using UTF8 encoding).
// Read characters until either a newline is encountered (not included in the result),
// or 1023 characters have been read.
export function readline() /* () -> <console/console,exn> string */  {
  var _x0 = $std_core_hnd._open_none0(readline_err);
  if (_x0._tag === 1) {
    return $std_core_exn.untry($std_core_types.$Error($std_core_hnd._open_none2(function(exn_0 /* exception */ , pre /* string */ ) {
        var _x1 = exn_0.message;
        var _x2 = exn_0.info;
        return $std_core_exn.Exception($std_core_types._lp__plus__plus__rp_(pre, $std_core_types._lp__plus__plus__rp_(": ", _x1)), _x2);
      }, _x0.error, "unable to read from stdin")));
  }
  else {
    return _x0.value;
  }
}