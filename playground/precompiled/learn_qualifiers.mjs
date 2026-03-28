// Koka generated module: learn/qualifiers, koka version: 3.2.4
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
 
 
// We can always use fully qualified to disambiguate plain names.
// In VS Code, press `ctrl+alt` (or `ctrl+option` on the mac) to see
// disambiguation with inlay-hints.
export function example1() /* () -> list<string> */  {
  return $std_core_list.map($std_core_types.Cons(1, $std_core_types.Cons(2, $std_core_types.Nil)), $std_core_int.show);
}
 
export function example1_explicit() /* () -> list<string> */  {
  return $std_core_list.map($std_core_types.Cons(1, $std_core_types.Cons(2, $std_core_types.Nil)), $std_core_int.show);
}
 
 
// Qualified names can be shortened as well:
export function example1_semi_explicit() /* () -> list<string> */  {
  return $std_core_list.map($std_core_types.Cons(1, $std_core_types.Cons(2, $std_core_types.Nil)), $std_core_int.show);
}
 
 
// We can define functions (or values) of the same name
// in the same module by using _local qualification_
export function int_fs_foo(i) /* (i : int) -> int */  {
  return $std_core_int.count_digits(i);
}
 
export function string_fs_foo(s) /* (s : string) -> int */  {
   
  var _x1 = undefined;
  var _x0 = (_x1 !== undefined) ? _x1 : false;
  var m_10001 = $std_core_int.xparse(s, _x0);
  var _x0 = (m_10001 === null) ? 42 : m_10001.value;
  return $std_core_int.count_digits(_x0);
}
 
 
// We can now use qualified names to disambiguate
// between the two top-level `foo` definitions:
export function example2() /* () -> int */  {
   
  var x_10005 = $std_core_int.count_digits(10);
   
  var _x2 = undefined;
  var _x1 = (_x2 !== undefined) ? _x2 : false;
  var m_10010 = $std_core_int.xparse("hi", _x1);
   
  var _x3 = (m_10010 === null) ? 42 : m_10010.value;
  var y_10006 = $std_core_int.count_digits(_x3);
  return $std_core_types._int_add(x_10005,y_10006);
}
 
 
// The Koka compiler uses type information
// to disambiguate names based on the argument types.
// This is more convenient than always writing full qualified names.
// (press `ctrl+alt` to see the inferred qualified names as inlay hints)
export function example3() /* () -> int */  {
   
  var x_10014 = $std_core_int.count_digits(11);
   
  var _x2 = undefined;
  var _x1 = (_x2 !== undefined) ? _x2 : false;
  var m_10019 = $std_core_int.xparse("hi there", _x1);
   
  var _x3 = (m_10019 === null) ? 42 : m_10019.value;
  var y_10015 = $std_core_int.count_digits(_x3);
  return $std_core_types._int_add(x_10014,y_10015);
}
 
 
// The disambiguation does not always succeed, and in such
// case you can either add more type information:
export function example4a(x) /* (x : int) -> int */  {
   
  var x_0_10023 = $std_core_int.count_digits(x);
   
  var _x2 = undefined;
  var _x1 = (_x2 !== undefined) ? _x2 : false;
  var m_10028 = $std_core_int.xparse("hi there", _x1);
   
  var _x3 = (m_10028 === null) ? 42 : m_10028.value;
  var y_10024 = $std_core_int.count_digits(_x3);
  return $std_core_types._int_add(x_0_10023,y_10024);
}
 
 
// Or disambiguate manually by writing (enough of) the qualifier.
// This always works in Koka as the disambiguation is purely convenience.
// In Koka, a program with fully qualified names has an _untyped dynamic semantics_,
// i.e. we can execute a program without knowing any types (including effect handlers).
// This is in contrast to a system with type classes (like Haskell or Lean) where the
// types are essential (as one cannot disambiguate instances otherwise, e.g. consider `show []`
// where the behavior depends on the type of sub-expression `[]`, like `[Char]` (=`""`) or `[Int]` (=`[]`)).
export function example4b(x) /* (x : int) -> int */  {
   
  var x_0_10032 = $std_core_int.count_digits(x);
   
  var _x2 = undefined;
  var _x1 = (_x2 !== undefined) ? _x2 : false;
  var m_10037 = $std_core_int.xparse("hi there", _x1);
   
  var _x3 = (m_10037 === null) ? 42 : m_10037.value;
  var y_10033 = $std_core_int.count_digits(_x3);
  return $std_core_types._int_add(x_0_10032,y_10033);
}
 
 
// Continue reading `implicits.kk` on how to combine overloading with implicit parameters.
export function main() /* () -> int */  {
   
  var x_0_10032 = $std_core_int.count_digits(42);
   
  var _x2 = undefined;
  var _x1 = (_x2 !== undefined) ? _x2 : false;
  var m_10037 = $std_core_int.xparse("hi there", _x1);
   
  var _x3 = (m_10037 === null) ? 42 : m_10037.value;
  var y_10033 = $std_core_int.count_digits(_x3);
  return $std_core_types._int_add(x_0_10032,y_10033);
}