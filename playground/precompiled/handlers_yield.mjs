// Koka generated module: handlers/yield, koka version: 3.2.4
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
 
 
// runtime tag for the effect `:yield`
export var yield_fs__tag;
var yield_fs__tag = "yield@yield";
// type yield
export function _Hnd_yield(_cfc, _fun_yield) /* forall<a,e,b> (int, hnd/clause1<a,(),yield<a>,e,b>) -> yield<a,e,b> */  {
  return { _cfc: _cfc, _fun_yield: _fun_yield };
}
 
// declarations
 
 
// Automatically generated. Retrieves the `@cfc` constructor field of the `:yield` type.
export function yield_fs__cfc(yield_0) /* forall<a,e,b> (yield : yield<a,e,b>) -> int */  {
  return yield_0._cfc;
}
 
 
// handler for the effect `:yield`
export function yield_fs__handle(hnd, ret, action) /* forall<a,b,e,c> (hnd : yield<a,e,c>, ret : (res : b) -> e c, action : () -> <yield<a>|e> b) -> e c */  {
  return $std_core_hnd._hhandle(yield_fs__tag, hnd, ret, action);
}
 
 
// Automatically generated. Retrieves the `@fun-yield` constructor field of the `:yield` type.
export function yield_fs__fun_yield(yield_0) /* forall<a,e,b> (yield : yield<a,e,b>) -> hnd/clause1<a,(),yield<a>,e,b> */  {
  return yield_0._fun_yield;
}
 
 
// select `yield` operation out of effect `:yield`
export function yield_fs__select(hnd) /* forall<a,e,b> (hnd : yield<a,e,b>) -> hnd/clause1<a,(),yield<a>,e,b> */  {
  return hnd._fun_yield;
}
 
 
// Call the `fun yield` operation of the effect `:yield`
export function $yield(x) /* forall<a> (x : a) -> (yield<a>) () */  {
   
  var ev_10011 = $std_core_hnd._evv_at(0);
  return ev_10011.hnd._fun_yield(ev_10011.marker, ev_10011, x);
}
 
 
// monadic lift
export function _mlift_traverse_10008(xx, wild__) /* forall<a> (xx : list<a>, wild_ : ()) -> (yield<a>) () */  {
  return traverse(xx);
}
 
 
// traverse a list and yield the elements
export function traverse(xs) /* forall<a> (xs : list<a>) -> (yield<a>) () */  { tailcall: while(1)
{
  if (xs !== null) {
     
    var ev_10017 = $std_core_hnd._evv_at(0);
     
    var x_0_10014 = ev_10017.hnd._fun_yield(ev_10017.marker, ev_10017, xs.head);
    if ($std_core_hnd._yielding()) {
      return $std_core_hnd.yield_extend(function(wild___0 /* () */ ) {
        return _mlift_traverse_10008(xs.tail, wild___0);
      });
    }
    else {
      {
        // tail call
        xs = xs.tail;
        continue tailcall;
      }
    }
  }
  else {
    return $std_core_types.Unit;
  }
}}
 
 
// monadic lift
export function _mlift_main_10009(wild__) /* (wild_ : ()) -> (yield<int>) () */  {
  return $std_core_console.printsln("done");
}
 
 
// bind the yield operation dynamically
export function main() /* () -> console/console () */  {
  return yield_fs__handle(_Hnd_yield(1, function(___wildcard_x649__14 /* hnd/marker<console/console,()> */ , ___wildcard_x649__17 /* hnd/ev<yield<int>> */ , x /* int */ ) {
         
        var s_10001 = $std_core_types._lp__plus__plus__rp_("yielded ", $std_core_int.show(x));
        return $std_core_console.printsln(s_10001);
      }), function(_res /* () */ ) {
      return _res;
    }, function() {
       
      var x_0_10021 = traverse($std_core_types.Cons(1, $std_core_types.Cons(2, $std_core_types.Cons(3, $std_core_types.Nil))));
      if ($std_core_hnd._yielding()) {
        return $std_core_hnd.yield_extend(_mlift_main_10009);
      }
      else {
        return $std_core_console.printsln("done");
      }
    });
}