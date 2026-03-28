// Koka generated module: learn/basic, koka version: 3.2.4
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
 
 
// Named function declarations use `fun`
// while anonymous function expressions use `fn`
export function example1() /* () -> console/console list<()> */  {
  return $std_core_list.map($std_core_list.range_fs_list(1, 5), function(i /* int */ ) {
      return $std_core_console.printsln($std_core_int.show(i));
    });
}
 
 
// Indented blocks get automatic curly braces (if not braced explicitly)
// We can write the previous example with just indentation:
export function example1a() /* () -> console/console list<()> */  {
  return $std_core_list.map($std_core_list.range_fs_list(1, 5), function(i /* int */ ) {
      return $std_core_console.printsln($std_core_int.show(i));
    });
}
 
 
// If the body of an anonymous function is an expression, we do not need curly braces:
export function example1b() /* () -> console/console list<()> */  {
  return $std_core_list.map($std_core_list.range_fs_list(1, 5), function(i /* int */ ) {
      return $std_core_console.printsln($std_core_int.show(i));
    });
}
 
 
// Use the dot-syntax to chain functions calls,
// where the left-expression is passed as the first argument to the
// right-expression:
// ```
// e.f(e1,...,eN)  ~>  f(e,e1,...,eN)
// ```
// This is also called _dot chaining_.
export function example2() /* () -> console/console list<()> */  {
  return $std_core_list.map($std_core_list.range_fs_list(1, 5), function(i /* int */ ) {
      return $std_core_console.printsln($std_core_int.show(i));
    });
}
 
 
// Trailing `fn` anonymous function arguments can be put outside the parenthesis following the call:
// `f(e1,...,eN) fn(x){ ... }  ~>  f(e1,...,eN, fn(x){ ... })`
// This is also called _trailing lambdas_
export function example3() /* () -> console/console list<()> */  {
  return $std_core_list.map($std_core_list.range_fs_list(1, 5), function(i /* int */ ) {
      return $std_core_console.printsln($std_core_int.show(i));
    });
}
 
 
// Of course, the indentation rule allows us to elide the braces here as well:
export function example3a() /* () -> console/console list<()> */  {
  return $std_core_list.map($std_core_list.range_fs_list(1, 5), function(i /* int */ ) {
      return $std_core_console.printsln($std_core_int.show(i));
    });
}
 
 
// monadic lift
export function _mlift_twice_10038(f, wild__) /* forall<a,e> (f : () -> e a, wild_ : a) -> e a */  {
  return f();
}
 
 
// An anonymous function without arguments can be written
// just with a pair of curly braces:
// `{ ... }   ~>  fn(){ ... }`
// This is also called _suspenders_ and very useful to define "control-flow" functions.
// (The term "suspenders" was coined by Conor McBride in the Frank language).
export function twice(f) /* forall<a,e> (f : () -> e a) -> e a */  {
   
  var x_10039 = f();
  if ($std_core_hnd._yielding()) {
    return $std_core_hnd.yield_extend(function(wild__ /* 1144 */ ) {
      return f();
    });
  }
  else {
    return f();
  }
}
 
export function example4() /* () -> console/console () */  {
   
  $std_core_console.printsln("hi");
  return $std_core_console.printsln("hi");
}
 
 
// Or with indentation:
export function example4a() /* () -> console/console () */  {
   
  $std_core_console.printsln("hi");
  return $std_core_console.printsln("hi");
}
 
 
// This syntax enable many control-flow operations to be defined as functions,
// like `while`:
export function example5() /* () -> <console/console,div> () */  {
  return function() {
     
    var loc = { value: 0 };
     
    var res = $std_core.$while(function() {
        return $std_core_types._int_lt((((loc).value)),10);
      }, function() {
         
        var x_10017 = ((loc).value);
         
        ((loc).value = ($std_core_types._int_add(x_10017,1)));
         
        var x_0_10019 = ((loc).value);
        return $std_core_console.printsln($std_core_int.show(x_0_10019));
      });
    return $std_core_hnd.prompt_local_var(loc, res);
  }();
}
 
 
// In Koka, expressions between curly braces (_suspenders_)
// are not directly evaluated -- and may be never evaluated, or
// evaluated multiple times (as in `while`).
//
// Expressions between parenthesis are always evaluated to a value
// (including evaluating their effects) before a call.
//
// This is one of the reasons why there is no ML-style currying (like `f e1 e2`)
// as we need  to distinguish `f(e1)(e2)` versus `f(e1,e2)`: in the
// first form there may be a side effect induced by `f` before
// `e2` is evaluated and passed to the result of `f(e1)`
// while in the second form both `e1` and `e2` are evaluated
// before calling `f`.
export function example6() /* () -> console/console () */  {
   
  var x_10021 = $std_core_list.foldl($std_core_types.Cons(21, $std_core_types.Cons(21, $std_core_types.Nil)), 0, $std_core_int._lp__plus__rp_);
  return $std_core_console.printsln($std_core_int.show(x_10021));
}
 
 
// The body of a function `fun` or `fn` does not have to braced
// but can directly be an expression.
export function increment(xs) /* (xs : list<int>) -> list<int> */  {
  return $std_core_list.filter($std_core_list.map(xs, function(x /* int */ ) {
        return $std_core_types._int_add(x,1);
      }), function(x_0_0 /* int */ ) {
      return $std_core_types._int_gt(x_0_0,2);
    });
}
 
 
// which is equivalent to:
export function increment2(xs) /* (xs : list<int>) -> list<int> */  {
  return $std_core_list.filter($std_core_list.map(xs, function(x /* int */ ) {
        return $std_core_types._int_add(x,1);
      }), function(x_0_0 /* int */ ) {
      return $std_core_types._int_gt(x_0_0,2);
    });
}
 
 
// Use trace for debug output
export function example_trace() /* () -> () */  {
   
  $std_core_debug.trace_info("example trace", $std_core_types._lp__plus__plus__rp_("learn/basic.kk", $std_core_types._lp__plus__plus__rp_("(", $std_core_types._lp__plus__plus__rp_($std_core_int.show(104), ")"))));
  return $std_core_debug.trace($std_core_types._lp__plus__plus__rp_("module: ", "learn/basic"));
}
 
export function main() /* () -> <console/console,div> () */  {
   
  $std_core_list.map($std_core_list.range_fs_list(1, 5), function(i /* int */ ) {
      return $std_core_console.printsln($std_core_int.show(i));
    });
   
  $std_core_list.map($std_core_list.range_fs_list(1, 5), function(i_0 /* int */ ) {
      return $std_core_console.printsln($std_core_int.show(i_0));
    });
   
  $std_core_list.map($std_core_list.range_fs_list(1, 5), function(i_1 /* int */ ) {
      return $std_core_console.printsln($std_core_int.show(i_1));
    });
   
  $std_core_console.printsln("hi");
   
  $std_core_console.printsln("hi");
  return example5();
}