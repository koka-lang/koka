// Koka generated module: learn/handler, koka version: 3.2.4
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
 
 
// runtime tag for the effect `:example`
export var example_fs__tag;
var example_fs__tag = "example@handler";
 
 
// runtime tag for the effect `:ask`
export var ask_fs__tag;
var ask_fs__tag = "ask@handler";
// type ask
export function _Hnd_ask(_cfc, _val_ask) /* forall<e,a> (int, hnd/clause0<int,ask,e,a>) -> ask<e,a> */  {
  return { _cfc: _cfc, _val_ask: _val_ask };
}
// type example
export function _Hnd_example(_cfc, _ctl_raise) /* forall<e,a> (int, forall<b> hnd/clause1<string,b,example,e,a>) -> example<e,a> */  {
  return { _cfc: _cfc, _ctl_raise: _ctl_raise };
}
 
// declarations
 
 
// Automatically generated. Retrieves the `@cfc` constructor field of the `:example` type.
export function example_fs__cfc(example) /* forall<e,a> (example : example<e,a>) -> int */  {
  return example._cfc;
}
 
 
// handler for the effect `:example`
export function example_fs__handle(hnd, ret, action) /* forall<a,e,b> (hnd : example<e,b>, ret : (res : a) -> e b, action : () -> <example|e> a) -> e b */  {
  return $std_core_hnd._hhandle(example_fs__tag, hnd, ret, action);
}
 
 
// Automatically generated. Retrieves the `@ctl-raise` constructor field of the `:example` type.
export function example_fs__ctl_raise(example) /* forall<e,a,b> (example : example<e,a>) -> hnd/clause1<string,b,example,e,a> */  {
  return example._ctl_raise;
}
 
 
// select `raise` operation out of effect `:example`
export function raise_fs__select(hnd) /* forall<a,e,b> (hnd : example<e,b>) -> hnd/clause1<string,a,example,e,b> */  {
  return hnd._ctl_raise;
}
 
 
// raise with a message
// Call the `ctl raise` operation of the effect `:example`
export function raise(msg) /* forall<a> (msg : string) -> example a */  {
   
  var ev_10023 = $std_core_hnd._evv_at(0);
  var _x0 = ev_10023.hnd._ctl_raise;
  return _x0(ev_10023.marker, ev_10023, msg);
}
 
 
// Functions can use abstract operations like `raise`,
// but it is reflected in the effect type (`:example`).
// Such function requires the effect to be handled at some point.
export function foo(pred) /* (pred : bool) -> example int */  {
  if (pred) {
     
    var ev_10026 = $std_core_hnd._evv_at(0);
    var _x1 = ev_10026.hnd._ctl_raise;
    return _x1(ev_10026.marker, ev_10026, "error");
  }
  else {
    return 1;
  }
}
 
 
// The basic `handler` expression takes a block
// of operation clauses and returns a handler function
// that function takes a unit-function `action`
// that is run under the handler.
export function example1() /* () -> console/console int */  {
  return example_fs__handle(_Hnd_example(3, function(m /* hnd/marker<console/console,int> */ , ___wildcard_x639__16 /* hnd/ev<example> */ , x /* string */ ) {
        return $std_core_hnd.yield_to(m, function(k /* (hnd/resume-result<_340,int>) -> console/console int */ ) {
            return $std_core_hnd.protect(x, function(msg /* string */ , resume /* (440) -> console/console int */ ) {
                 
                $std_core_console.printsln(msg);
                return 42;
              }, k);
          });
      }), function(_res /* int */ ) {
      return _res;
    }, function() {
       
      var ev_10030 = $std_core_hnd._evv_at(0);
      var _x2 = ev_10030.hnd._ctl_raise;
      return _x2(ev_10030.marker, ev_10030, "error");
    });
}
 
 
// The `with` statement (see `learn/with`) can help to
// specify the previous handler more concisely as:
export function example2() /* () -> console/console int */  {
  return example_fs__handle(_Hnd_example(3, function(m /* hnd/marker<console/console,int> */ , ___wildcard_x639__16 /* hnd/ev<example> */ , x /* string */ ) {
        return $std_core_hnd.yield_to(m, function(k /* (hnd/resume-result<_464,int>) -> console/console int */ ) {
            return $std_core_hnd.protect(x, function(msg /* string */ , resume /* (564) -> console/console int */ ) {
                 
                $std_core_console.printsln(msg);
                return 42;
              }, k);
          });
      }), function(_res /* int */ ) {
      return _res;
    }, function() {
       
      var ev_10034 = $std_core_hnd._evv_at(0);
      var _x3 = ev_10034.hnd._ctl_raise;
      return _x3(ev_10034.marker, ev_10034, "error");
    });
}
 
 
// If there is only one operation, we can shorten this further as:
export function example3() /* () -> console/console int */  {
  return example_fs__handle(_Hnd_example(3, function(m /* hnd/marker<console/console,int> */ , ___wildcard_x639__16 /* hnd/ev<example> */ , x /* string */ ) {
        return $std_core_hnd.yield_to(m, function(k /* (hnd/resume-result<_587,int>) -> console/console int */ ) {
            return $std_core_hnd.protect(x, function(msg /* string */ , resume /* (687) -> console/console int */ ) {
                 
                $std_core_console.printsln(msg);
                return 42;
              }, k);
          });
      }), function(_res /* int */ ) {
      return _res;
    }, function() {
       
      var ev_10038 = $std_core_hnd._evv_at(0);
      var _x4 = ev_10038.hnd._ctl_raise;
      return _x4(ev_10038.marker, ev_10038, "error");
    });
}
 
 
// Mark `:raise` as `final` since it never resumes to the call-site
export function example4() /* () -> console/console int */  {
  return example_fs__handle(_Hnd_example(0, function(m /* hnd/marker<console/console,int> */ , ___wildcard_x654__16 /* hnd/ev<example> */ , x /* string */ ) {
        return $std_core_hnd.yield_to_final(m, function(___wildcard_x654__45 /* (hnd/resume-result<813,int>) -> console/console int */ ) {
             
            $std_core_console.printsln(x);
            return 42;
          });
      }), function(_res /* int */ ) {
      return _res;
    }, function() {
       
      var ev_10042 = $std_core_hnd._evv_at(0);
      var _x5 = ev_10042.hnd._ctl_raise;
      return _x5(ev_10042.marker, ev_10042, "error");
    });
}
 
 
// select `@val-ask` operation out of effect `:ask`
export function _val_ask_fs__select(hnd) /* forall<e,a> (hnd : ask<e,a>) -> hnd/clause0<int,ask,e,a> */  {
  return hnd._val_ask;
}
 
 
// Call the `val ask` operation of the effect `:ask`
export function _val_ask() /* () -> ask int */  {
   
  var ev_10045 = $std_core_hnd._evv_at(0);
  return ev_10045.hnd._val_ask(ev_10045.marker, ev_10045);
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
 
 
// Dynamic binding with static typing!
export function print_ask() /* () -> <ask,console/console> () */  {
   
  var ev_10048 = $std_core_hnd._evv_at(0);
   
  var x_10011 = ev_10048.hnd._val_ask(ev_10048.marker, ev_10048);
  return $std_core_console.printsln($std_core_int.show(x_10011));
}
 
 
// Using different handlers for each invocation
export function example5() /* () -> console/console () */  {
   
  ask_fs__handle(_Hnd_ask(1, function(___wildcard_x695__14 /* hnd/marker<console/console,()> */ , ___wildcard_x695__17 /* hnd/ev<ask> */ ) {
        return 42;
      }), function(_res /* () */ ) {
      return _res;
    }, function() {
       
      var ev_10048 = $std_core_hnd._evv_at(0);
       
      var x_10011 = ev_10048.hnd._val_ask(ev_10048.marker, ev_10048);
      return $std_core_console.printsln($std_core_int.show(x_10011));
    });
  return ask_fs__handle(_Hnd_ask(1, function(___wildcard_x695__14_0 /* hnd/marker<console/console,()> */ , ___wildcard_x695__17_0 /* hnd/ev<ask> */ ) {
        return 43;
      }), function(_res_0 /* () */ ) {
      return _res_0;
    }, function() {
       
      var ev_10048_0 = $std_core_hnd._evv_at(0);
       
      var x_10011_0 = ev_10048_0.hnd._val_ask(ev_10048_0.marker, ev_10048_0);
      return $std_core_console.printsln($std_core_int.show(x_10011_0));
    });
}
 
export function main() /* () -> console/console () */  {
   
  ask_fs__handle(_Hnd_ask(1, function(___wildcard_x695__14 /* hnd/marker<console/console,()> */ , ___wildcard_x695__17 /* hnd/ev<ask> */ ) {
        return 42;
      }), function(_res /* () */ ) {
      return _res;
    }, function() {
       
      var ev_10048 = $std_core_hnd._evv_at(0);
       
      var x_10011 = ev_10048.hnd._val_ask(ev_10048.marker, ev_10048);
      return $std_core_console.printsln($std_core_int.show(x_10011));
    });
  return ask_fs__handle(_Hnd_ask(1, function(___wildcard_x695__14_0 /* hnd/marker<console/console,()> */ , ___wildcard_x695__17_0 /* hnd/ev<ask> */ ) {
        return 43;
      }), function(_res_0 /* () */ ) {
      return _res_0;
    }, function() {
       
      var ev_10048_0 = $std_core_hnd._evv_at(0);
       
      var x_10011_0 = ev_10048_0.hnd._val_ask(ev_10048_0.marker, ev_10048_0);
      return $std_core_console.printsln($std_core_int.show(x_10011_0));
    });
}