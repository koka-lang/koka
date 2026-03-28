// Koka generated module: basic/fibonacci, koka version: 3.2.4
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
 
// declarations
 
export function fib(n, x1, x2) /* (n : int, x1 : int, x2 : int) -> div int */  { tailcall: while(1)
{
  if ($std_core_types._int_le(n,0)) {
    return x1;
  }
  else {
    {
      // tail call
      var _x0 = $std_core_types._int_sub(n,1);
      var _x1 = $std_core_types._int_add(x1,x2);
      n = _x0;
      x1 = x2;
      x2 = _x1;
      continue tailcall;
    }
  }
}}
 
 
// Calculate the `n`'th fibonacci number.
export function fibonacci(n) /* (n : int) -> div int */  {
  return fib(n, 0, 1);
}
 
export function main() /* () -> <console/console,div> () */  {
   
  var s_10004 = $std_core_types._lp__plus__plus__rp_("The ", $std_core_types._lp__plus__plus__rp_($std_core_int.show(10000), $std_core_types._lp__plus__plus__rp_("th fibonacci number is ", $std_core_int.show(fib(10000, 0, 1)))));
  return $std_core_console.printsln(s_10004);
}
 
export function examplex() /* () -> <console/console,div> () */  {
   
  var x_10006 = fib(100, 0, 1);
  return $std_core_console.printsln($std_core_int.show(x_10006));
}
 
export function example2() /* () -> <console/console,div> () */  {
   
  var x_10009 = fib(1000, 0, 1);
  return $std_core_console.printsln($std_core_int.show(x_10009));
}