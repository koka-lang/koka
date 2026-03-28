// Koka generated module: handlers/named/ask, koka version: 3.2.4
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
 
// type declarations
 
 
// runtime tag for the effect `:reader`
export var reader_fs__tag;
var reader_fs__tag = "reader@ask";
// type reader
export function _Hnd_reader(_cfc, _fun_ask) /* forall<e,a> (int, hnd/clause0<string,reader,e,a>) -> reader<e,a> */  {
  return { _cfc: _cfc, _fun_ask: _fun_ask };
}
 
// declarations
 
 
// Automatically generated. Retrieves the `@cfc` constructor field of the `:reader` type.
export function reader_fs__cfc(reader_0) /* forall<e,a> (reader : reader<e,a>) -> int */  {
  return reader_0._cfc;
}
 
 
// handler for the effect `:reader`
export function reader_fs__handle(hnd, ret, action) /* forall<a,e,b> (hnd : reader<e,b>, ret : (res : a) -> e b, action : (hname : hnd/ev<reader>) -> e a) -> e b */  {
  return $std_core_hnd._named_handle(reader_fs__tag, hnd, ret, action);
}
 
 
// Automatically generated. Retrieves the `@fun-ask` constructor field of the `:reader` type.
export function reader_fs__fun_ask(reader_0) /* forall<e,a> (reader : reader<e,a>) -> hnd/clause0<string,reader,e,a> */  {
  return reader_0._fun_ask;
}
 
 
// select `ask` operation out of effect `:reader`
export function ask_fs__select(hnd) /* forall<e,a> (hnd : reader<e,a>) -> hnd/clause0<string,reader,e,a> */  {
  return hnd._fun_ask;
}
 
 
// named handler for files
// Call the `fun ask` operation of the effect `:reader`
export function ask(_hname) /* (hnd/ev<reader>) -> exn string */  {
  return _hname.hnd._fun_ask(_hname.marker, _hname);
}
 
 
// A reader handler
export function reader(msg, action) /* forall<a,e> (msg : string, action : (hnd/ev<reader>) -> e a) -> e a */  {
  return reader_fs__handle(_Hnd_reader(1, $std_core_hnd.clause_tail0(function() {
        return msg;
      })), function(_res /* 286 */ ) {
      return _res;
    }, action);
}
 
 
// monadic lift
export function _mlift_read2files_10014(_y_x10009, _y_x10010) /* (string, string) -> exn () */  {
  return $std_core_console.printsln($std_core_types._lp__plus__plus__rp_(_y_x10009, $std_core_types._lp__plus__plus__rp_(" ", _y_x10010)));
}
 
 
// monadic lift
export function _mlift_read2files_10015(r2, _y_x10009) /* (r2 : hnd/ev<reader>, string) -> exn () */  {
   
  var x_10019 = r2.hnd._fun_ask(r2.marker, r2);
   
  function next_10020(_y_x10010) /* (string) -> exn () */  {
    return $std_core_console.printsln($std_core_types._lp__plus__plus__rp_(_y_x10009, $std_core_types._lp__plus__plus__rp_(" ", _y_x10010)));
  }
  if ($std_core_hnd._yielding()) {
    return $std_core_hnd.yield_extend(next_10020);
  }
  else {
    return next_10020(x_10019);
  }
}
 
 
// Using two readers.
// This particular example is of course silly (one can just pass regular parameters)
// but it can be useful if the named effect performs operations itself
// in its own context (like reading from the current http connection)
export function read2files(r1, r2) /* (r1 : hnd/ev<reader>, r2 : hnd/ev<reader>) -> <console/console,exn> () */  {
   
  var x_10025 = r1.hnd._fun_ask(r1.marker, r1);
  if ($std_core_hnd._yielding()) {
    return $std_core_hnd.yield_extend(function(_y_x10009 /* string */ ) {
      return _mlift_read2files_10015(r2, _y_x10009);
    });
  }
  else {
     
    var x_0_10030 = r2.hnd._fun_ask(r2.marker, r2);
    if ($std_core_hnd._yielding()) {
      return $std_core_hnd.yield_extend(function(_y_x10010 /* string */ ) {
        return $std_core_console.printsln($std_core_types._lp__plus__plus__rp_(x_10025, $std_core_types._lp__plus__plus__rp_(" ", _y_x10010)));
      });
    }
    else {
      return $std_core_console.printsln($std_core_types._lp__plus__plus__rp_(x_10025, $std_core_types._lp__plus__plus__rp_(" ", x_0_10030)));
    }
  }
}
 
export function main() /* () -> <console/console,exn> () */  {
  return reader_fs__handle(_Hnd_reader(1, $std_core_hnd.clause_tail0(function() {
        return "hello";
      })), function(_res /* () */ ) {
      return _res;
    }, function(r1 /* hnd/ev<reader> */ ) {
      return reader_fs__handle(_Hnd_reader(1, $std_core_hnd.clause_tail0(function() {
            return "world";
          })), function(_res_0 /* () */ ) {
          return _res_0;
        }, function(r2 /* hnd/ev<reader> */ ) {
          return read2files(r1, r2);
        });
    });
}