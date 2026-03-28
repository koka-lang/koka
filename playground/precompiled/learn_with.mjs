// Koka generated module: learn/with, koka version: 3.2.4
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
 
 
// runtime tag for the effect `:ask`
export var ask_fs__tag;
var ask_fs__tag = "ask@with";
// type ask
export function _Hnd_ask(_cfc, _val_ask) /* forall<e,a> (int, hnd/clause0<int,ask,e,a>) -> ask<e,a> */  {
  return { _cfc: _cfc, _val_ask: _val_ask };
}
 
// declarations
 
 
// monadic lift
export function _mlift_twice_10030(f, wild__) /* forall<a,e> (f : () -> e a, wild_ : a) -> e a */  {
  return f();
}
 
export function twice(f) /* forall<a,e> (f : () -> e a) -> e a */  {
   
  var x_10031 = f();
  if ($std_core_hnd._yielding()) {
    return $std_core_hnd.yield_extend(function(wild__ /* 55 */ ) {
      return f();
    });
  }
  else {
    return f();
  }
}
 
export function example1() /* () -> console/console () */  {
   
  $std_core_console.printsln("hi");
   
  $std_core_console.printsln("hi");
   
  $std_core_console.printsln("hi");
  return $std_core_console.printsln("hi");
}
 
 
// The above is equivalent to:
export function example1_expanded() /* () -> console/console () */  {
   
  $std_core_console.printsln("hi");
   
  $std_core_console.printsln("hi");
   
  $std_core_console.printsln("hi");
  return $std_core_console.printsln("hi");
}
 
 
// Without using layout, the fully expanded version is:
export function example1_fully_expanded() /* () -> console/console () */  {
   
  $std_core_console.printsln("hi");
   
  $std_core_console.printsln("hi");
   
  $std_core_console.printsln("hi");
  return $std_core_console.printsln("hi");
}
 
export function example2() /* () -> console/console list<()> */  {
  return $std_core_list.map($std_core_list.range_fs_list(1, 10), function(x /* int */ ) {
      return $std_core_console.printsln($std_core_int.show(x));
    });
}
 
 
// which is equivalent to:
export function example2_expanded() /* () -> console/console list<()> */  {
  return $std_core_list.map($std_core_list.range_fs_list(1, 10), function(x /* int */ ) {
      return $std_core_console.printsln($std_core_int.show(x));
    });
}
 
 
// select `@val-ask` operation out of effect `:ask`
export function _val_ask_fs__select(hnd) /* forall<e,a> (hnd : ask<e,a>) -> hnd/clause0<int,ask,e,a> */  {
  return hnd._val_ask;
}
 
 
// Call the `val ask` operation of the effect `:ask`
export function _val_ask() /* () -> ask int */  {
   
  var ev_10036 = $std_core_hnd._evv_at(0);
  return ev_10036.hnd._val_ask(ev_10036.marker, ev_10036);
}
 
 
// Automatically generated. Retrieves the `@val-ask` constructor field of the `:ask` type.
export function ask_fs__val_ask(ask_0) /* forall<e,a> (ask : ask<e,a>) -> hnd/clause0<int,ask,e,a> */  {
  return ask_0._val_ask;
}
 
 
// Automatically generated. Retrieves the `@cfc` constructor field of the `:ask` type.
export function ask_fs__cfc(ask_0) /* forall<e,a> (ask : ask<e,a>) -> int */  {
  return ask_0._cfc;
}
 
 
// handler for the effect `:ask`
export function ask_fs__handle(hnd, ret, action) /* forall<a,e,b> (hnd : ask<e,b>, ret : (res : a) -> e b, action : () -> <ask|e> a) -> e b */  {
  return $std_core_hnd._hhandle(ask_fs__tag, hnd, ret, action);
}
 
 
// Call the `val ask` operation of the effect `:ask`
export var ask;
var ask = $std_core_types._Valueop;
 
export function ask_twice() /* () -> ask int */  {
   
  var ev_10039 = $std_core_hnd._evv_at(0);
   
  var x_10028 = ev_10039.hnd._val_ask(ev_10039.marker, ev_10039);
   
  var ev_0_10041 = $std_core_hnd._evv_at(0);
   
  var y_10029 = ev_0_10041.hnd._val_ask(ev_0_10041.marker, ev_0_10041);
  return $std_core_types._int_add(x_10028,y_10029);
}
 
export function example3() /* () -> console/console () */  {
  return ask_fs__handle(_Hnd_ask(1, function(___wildcard_x695__14 /* hnd/marker<console/console,()> */ , ___wildcard_x695__17 /* hnd/ev<ask> */ ) {
        return 21;
      }), function(_res /* () */ ) {
      return _res;
    }, function() {
       
      var x_10013 = ask_twice();
      return $std_core_console.printsln($std_core_int.show(x_10013));
    });
}
 
 
// Which is the same as:
export function example3_expanded() /* () -> console/console () */  {
  return ask_fs__handle(_Hnd_ask(1, function(___wildcard_x695__14 /* hnd/marker<console/console,()> */ , ___wildcard_x695__17 /* hnd/ev<ask> */ ) {
        return 21;
      }), function(_res /* () */ ) {
      return _res;
    }, function() {
       
      var x_10015 = ask_twice();
      return $std_core_console.printsln($std_core_int.show(x_10015));
    });
}
 
export function example4() /* () -> console/console () */  {
  return ask_fs__handle(_Hnd_ask(1, function(___wildcard_x695__14 /* hnd/marker<console/console,()> */ , ___wildcard_x695__17 /* hnd/ev<ask> */ ) {
        return 21;
      }), function(_res /* () */ ) {
      return _res;
    }, function() {
       
      var x_10017 = ask_twice();
      return $std_core_console.printsln($std_core_int.show(x_10017));
    });
}
 
export function example4_expanded() /* () -> console/console () */  {
  return ask_fs__handle(_Hnd_ask(1, function(___wildcard_x695__14 /* hnd/marker<console/console,()> */ , ___wildcard_x695__17 /* hnd/ev<ask> */ ) {
        return 21;
      }), function(_res /* () */ ) {
      return _res;
    }, function() {
       
      var x_10019 = ask_twice();
      return $std_core_console.printsln($std_core_int.show(x_10019));
    });
}
 
export function main() /* () -> console/console () */  {
   
  $std_core_console.printsln("hi");
   
  $std_core_console.printsln("hi");
   
  $std_core_console.printsln("hi");
   
  $std_core_console.printsln("hi");
   
  $std_core_list.map($std_core_list.range_fs_list(1, 10), function(x /* int */ ) {
      return $std_core_console.printsln($std_core_int.show(x));
    });
   
  ask_fs__handle(_Hnd_ask(1, function(___wildcard_x695__14 /* hnd/marker<console/console,()> */ , ___wildcard_x695__17 /* hnd/ev<ask> */ ) {
        return 21;
      }), function(_res /* () */ ) {
      return _res;
    }, function() {
       
      var x_10013 = ask_twice();
      return $std_core_console.printsln($std_core_int.show(x_10013));
    });
  return ask_fs__handle(_Hnd_ask(1, function(___wildcard_x695__14_0 /* hnd/marker<console/console,()> */ , ___wildcard_x695__17_0 /* hnd/ev<ask> */ ) {
        return 21;
      }), function(_res_0 /* () */ ) {
      return _res_0;
    }, function() {
       
      var x_10017 = ask_twice();
      return $std_core_console.printsln($std_core_int.show(x_10017));
    });
}