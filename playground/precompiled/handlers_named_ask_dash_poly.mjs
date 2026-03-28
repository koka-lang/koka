// Koka generated module: handlers/named/ask-poly, koka version: 3.2.4
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
var reader_fs__tag = "reader@ask-poly";
// type reader
export function _Hnd_reader(_cfc, _fun_ask) /* forall<a,e,b> (int, hnd/clause0<a,reader<a>,e,b>) -> reader<a,e,b> */  {
  return { _cfc: _cfc, _fun_ask: _fun_ask };
}
 
// declarations
 
 
// Automatically generated. Retrieves the `@cfc` constructor field of the `:reader` type.
export function reader_fs__cfc(reader_0) /* forall<a,e,b> (reader : reader<a,e,b>) -> int */  {
  return reader_0._cfc;
}
 
 
// handler for the effect `:reader`
export function reader_fs__handle(hnd, ret, action) /* forall<a,b,e,c> (hnd : reader<a,e,c>, ret : (res : b) -> e c, action : (hname : hnd/ev<reader<a>>) -> e b) -> e c */  {
  return $std_core_hnd._named_handle(reader_fs__tag, hnd, ret, action);
}
 
 
// Automatically generated. Retrieves the `@fun-ask` constructor field of the `:reader` type.
export function reader_fs__fun_ask(reader_0) /* forall<a,e,b> (reader : reader<a,e,b>) -> hnd/clause0<a,reader<a>,e,b> */  {
  return reader_0._fun_ask;
}
 
 
// select `ask` operation out of effect `:reader`
export function ask_fs__select(hnd) /* forall<a,e,b> (hnd : reader<a,e,b>) -> hnd/clause0<a,reader<a>,e,b> */  {
  return hnd._fun_ask;
}
 
 
// named handler for files
// Call the `fun ask` operation of the effect `:reader`
export function ask(_hname) /* forall<a> (hnd/ev<reader<a>>) -> <div,exn> a */  {
  return _hname.hnd._fun_ask(_hname.marker, _hname);
}
 
 
// A reader handler
export function reader(x, action) /* forall<a,b,e> (x : a, action : (hnd/ev<reader<a>>) -> e b) -> e b */  {
  return reader_fs__handle(_Hnd_reader(1, $std_core_hnd.clause_tail0(function() {
        return x;
      })), function(_res /* 394 */ ) {
      return _res;
    }, action);
}
 
 
// monadic lift
export function _mlift_read2files_10016(_y_x10009, _implicit_fs_show, _y_x10010) /* forall<a> (string, ?show : (a) -> string, a) -> <div,exn> () */  {
  return $std_core_console.printsln($std_core_types._lp__plus__plus__rp_(_y_x10009, $std_core_types._lp__plus__plus__rp_(" ", $std_core_hnd._open_none1(_implicit_fs_show, _y_x10010))));
}
 
 
// monadic lift
export function _mlift_read2files_10017(r2, _implicit_fs_show, _y_x10009) /* forall<a> (r2 : hnd/ev<reader<a>>, ?show : (a) -> string, string) -> <div,exn> () */  {
   
  var x_10021 = r2.hnd._fun_ask(r2.marker, r2);
   
  function next_10022(_y_x10010) /* (573) -> <div,exn> () */  {
    return $std_core_console.printsln($std_core_types._lp__plus__plus__rp_(_y_x10009, $std_core_types._lp__plus__plus__rp_(" ", $std_core_hnd._open_none1(_implicit_fs_show, _y_x10010))));
  }
  if ($std_core_hnd._yielding()) {
    return $std_core_hnd.yield_extend(next_10022);
  }
  else {
    return next_10022(x_10021);
  }
}
 
 
// Using two readers.
// This particular example is of course silly (one can just pass regular parameters)
// but it can be useful if the named effect performs operations itself
// in its own context (like reading from the current http connection)
export function read2files(r1, r2, _implicit_fs_show) /* forall<a> (r1 : hnd/ev<reader<string>>, r2 : hnd/ev<reader<a>>, ?show : (a) -> string) -> <pure,console/console> () */  {
   
  var x_10028 = r1.hnd._fun_ask(r1.marker, r1);
  if ($std_core_hnd._yielding()) {
    return $std_core_hnd.yield_extend(function(_y_x10009 /* string */ ) {
      return _mlift_read2files_10017(r2, _implicit_fs_show, _y_x10009);
    });
  }
  else {
     
    var x_0_10033 = r2.hnd._fun_ask(r2.marker, r2);
    if ($std_core_hnd._yielding()) {
      return $std_core_hnd.yield_extend(function(_y_x10010 /* 573 */ ) {
        return $std_core_console.printsln($std_core_types._lp__plus__plus__rp_(x_10028, $std_core_types._lp__plus__plus__rp_(" ", $std_core_hnd._open_none1(_implicit_fs_show, _y_x10010))));
      });
    }
    else {
      return $std_core_console.printsln($std_core_types._lp__plus__plus__rp_(x_10028, $std_core_types._lp__plus__plus__rp_(" ", $std_core_hnd._open_none1(_implicit_fs_show, x_0_10033))));
    }
  }
}
 
export function main() /* () -> <pure,console/console> () */  {
  return reader_fs__handle(_Hnd_reader(1, $std_core_hnd.clause_tail0(function() {
        return "hello";
      })), function(_res /* () */ ) {
      return _res;
    }, function(r1 /* hnd/ev<reader<string>> */ ) {
      return reader_fs__handle(_Hnd_reader(1, $std_core_hnd.clause_tail0(function() {
            return 42;
          })), function(_res_0 /* () */ ) {
          return _res_0;
        }, function(r2 /* hnd/ev<reader<int>> */ ) {
          return read2files(r1, r2, $std_core_int.show);
        });
    });
}