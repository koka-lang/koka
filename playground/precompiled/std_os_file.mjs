// Koka generated module: std/os/file, koka version: 3.2.4
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
import * as $std_os_dir from './std_os_dir.mjs';
 
// externals
/*---------------------------------------------------------------------------
  Copyright 2012-2021, Microsoft Research, Daan Leijen.
  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the LICENSE file at the root of this distribution.
---------------------------------------------------------------------------*/
var _read_text_file_error;
var _write_text_file_error;
if ($std_core.host()=="node")
{
  // Node
  var fs = await import("fs");
  _read_text_file_error = function( path ) {
    try {
      return $std_core_exn.Ok( fs.readFileSync(path,{encoding: 'utf8'}) );
    }
    catch(exn) {
      return $std_core_exn._error_from_exception(exn);
    }
  };
  _write_text_file_error = function( path, content ) {
    try {
      fs.writeFileSync(path,content,{encoding: 'utf8'});
      return $std_core_exn.Ok( $std_core_types._Unit_ );
    }
    catch(exn) {
      return $std_core_exn._error_from_exception(exn);
    }
  };
}
else {
  // TODO: write to local storage on the browser?
  _read_text_file_error = function( path ) {
    return $std_core.Ok( "" );
  };
  _write_text_file_error = function( path, content ) {
    return $std_core.Ok( $std_core_types._Unit_ );
  }
}
 
// type declarations
 
// declarations
 
export function read_text_file_err(path) /* (path : string) -> fsys error<string> */  {
  return _read_text_file_error(path);
}
 
 
// Read a text file synchronously (using UTF8 encoding)
export function read_text_file(path) /* (path : std/os/path/path) -> <exn,fsys> string */  {
   
  var _x_x1_10020 = $std_core_hnd._open_none1($std_os_path.string, path);
  var _x0 = $std_core_hnd._open_none1(read_text_file_err, _x_x1_10020);
  if (_x0._tag === 1) {
     
    var _x_x2_10023 = $std_core_types._lp__plus__plus__rp_("unable to read text file ", $std_core_hnd._open_none1(function(p /* std/os/path/path */ ) {
          return $std_core_show.string_fs_show($std_os_path.string(p));
        }, path));
     
    var exn_0_10000 = $std_core_hnd._open_none2(function(exn_1 /* exception */ , pre /* string */ ) {
        var _x1 = exn_1.message;
        var _x2 = exn_1.info;
        return $std_core_exn.Exception($std_core_types._lp__plus__plus__rp_(pre, $std_core_types._lp__plus__plus__rp_(": ", _x1)), _x2);
      }, _x0.error, _x_x2_10023);
     
    var ev_10034 = $std_core_hnd._evv_at(0);
    var _x1 = $std_core_exn.throw_exn_fs__select(ev_10034.hnd);
    return _x1(ev_10034.marker, ev_10034, exn_0_10000);
  }
  else {
    return _x0.value;
  }
}
 
export function write_text_file_err(path, content) /* (path : string, content : string) -> fsys error<()> */  {
  return _write_text_file_error(path,content);
}
 
 
// monadic lift
export function _mlift_write_text_file_10033(content, path, _c_x10007) /* (content : string, path : std/os/path/path, ()) -> () */  {
   
  var _x_x1_1_10027 = $std_core_hnd._open_none1($std_os_path.string, path);
  var _x2 = $std_core_hnd._open_none2(write_text_file_err, _x_x1_1_10027, content);
  if (_x2._tag === 1) {
     
    var _x_x2_0_10031 = $std_core_types._lp__plus__plus__rp_("unable to write text file ", $std_core_hnd._open_none1(function(p_1 /* std/os/path/path */ ) {
          return $std_core_show.string_fs_show($std_os_path.string(p_1));
        }, path));
     
    var exn_0_10001 = $std_core_hnd._open_none2(function(exn_1 /* exception */ , pre /* string */ ) {
        var _x3 = exn_1.message;
        var _x4 = exn_1.info;
        return $std_core_exn.Exception($std_core_types._lp__plus__plus__rp_(pre, $std_core_types._lp__plus__plus__rp_(": ", _x3)), _x4);
      }, _x2.error, _x_x2_0_10031);
     
    var ev_10037 = $std_core_hnd._evv_at(0);
    var _x3 = $std_core_exn.throw_exn_fs__select(ev_10037.hnd);
    return _x3(ev_10037.marker, ev_10037, exn_0_10001);
  }
  else {
    return $std_core_types.Unit;
  }
}
 
 
// Write a text file synchronously (using UTF8 encoding)
export function write_text_file(path, content, create_dir) /* (path : std/os/path/path, content : string, create-dir : ? bool) -> <exn,fsys> () */  {
   
  if (create_dir !== undefined) {
    if (create_dir) {
      var x_10040 = $std_os_dir.ensure_dir($std_core_hnd._open_none1(function(p /* std/os/path/path */ ) {
          var _x5 = undefined;
          if (_x5 !== undefined) {
            var _x4 = _x5;
          }
          else {
            var _x4 = p.root;
          }
          var _x6 = (p.parts !== null) ? p.parts.tail : $std_core_types.Nil;
          return $std_os_path.Path(_x4, _x6);
        }, path));
    }
    else {
      var x_10040 = $std_core_types.Unit;
    }
  }
  else {
    var x_10040 = $std_os_dir.ensure_dir($std_core_hnd._open_none1(function(p_0 /* std/os/path/path */ ) {
        var _x8 = undefined;
        if (_x8 !== undefined) {
          var _x7 = _x8;
        }
        else {
          var _x7 = p_0.root;
        }
        var _x9 = (p_0.parts !== null) ? p_0.parts.tail : $std_core_types.Nil;
        return $std_os_path.Path(_x7, _x9);
      }, path));
  }
  if ($std_core_hnd._yielding()) {
    return $std_core_hnd.yield_extend(function(_c_x10007 /* () */ ) {
      return _mlift_write_text_file_10033(content, path, _c_x10007);
    });
  }
  else {
     
    var _x_x1_1_10027 = $std_core_hnd._open_none1($std_os_path.string, path);
    var _x4 = $std_core_hnd._open_none2(write_text_file_err, _x_x1_1_10027, content);
    if (_x4._tag === 1) {
       
      var _x_x2_0_10031 = $std_core_types._lp__plus__plus__rp_("unable to write text file ", $std_core_hnd._open_none1(function(p_1 /* std/os/path/path */ ) {
            return $std_core_show.string_fs_show($std_os_path.string(p_1));
          }, path));
       
      var exn_0_10001 = $std_core_hnd._open_none2(function(exn_1 /* exception */ , pre /* string */ ) {
          var _x5 = exn_1.message;
          var _x6 = exn_1.info;
          return $std_core_exn.Exception($std_core_types._lp__plus__plus__rp_(pre, $std_core_types._lp__plus__plus__rp_(": ", _x5)), _x6);
        }, _x4.error, _x_x2_0_10031);
       
      var ev_10043 = $std_core_hnd._evv_at(0);
      var _x5 = $std_core_exn.throw_exn_fs__select(ev_10043.hnd);
      return _x5(ev_10043.marker, ev_10043, exn_0_10001);
    }
    else {
      return $std_core_types.Unit;
    }
  }
}