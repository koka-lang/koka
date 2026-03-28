// Koka generated module: learn/implicits, koka version: 3.2.4
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
import * as $std_num_float64 from './std_num_float64.mjs';
 
// externals
 
// type declarations
 
// declarations
 
export function int_fs_eq(x, y) /* (x : int, y : int) -> bool */  {
  return $std_core_types._int_eq(x,y);
}
 
export function float64_fs_eq(x, y) /* (x : float64, y : float64) -> bool */  {
  return (x === y);
}
 
export function char_fs_eq(x, y) /* (x : char, y : char) -> bool */  {
  return (x === y);
}
 
 
// And we have an equality function on lists as well:
export function list_eq_explicit(xs, ys, eq) /* forall<a> (xs : list<a>, ys : list<a>, eq : (a, a) -> bool) -> bool */  { tailcall: while(1)
{
  if (xs !== null) {
    if (ys !== null) {
      if (eq(xs.head, ys.head)){
        {
          // tail call
          xs = xs.tail;
          ys = ys.tail;
          continue tailcall;
        }
      }
    }
    return false;
  }
  else {
    return (ys === null);
  }
}}
 
export function string_fs_compare(x, y) /* (x : string, y : string) -> order */  {
  return $std_core_string.cmp(x, y);
}
 
 
// These conversions can lead to further ambiguities. Consider adding an `:int` comparison.
export function int_fs_compare(x, y) /* (x : int, y : int) -> order */  {
  if ($std_core_types._int_eq(x,y)) {
    return $std_core_types.Eq;
  }
  else {
    return ($std_core_types._int_gt(x,y)) ? $std_core_types.Gt : $std_core_types.Lt;
  }
}
 
 
// When resolving an implicit parameter, the compiler also considers
// matching functions that take further implicit parameters themselves.
// These are called _conversions_. For example, given a `compare` function,
// we could provide an `eq` function:
export function default_fs_compare_fs_eq(x, y, _implicit_fs_compare) /* forall<a> (x : a, y : a, ?compare : (a, a) -> order) -> bool */  {
  return $std_core_order._lp__eq__eq__rp_(_implicit_fs_compare(x, y), $std_core_types.Eq);
}
 
 
// The `?eq` notation is actually a shorthand for a locally qualified name `@implicit/eq`.
// When resolving an implicit parameter `?eq`, the compiler looks for the plain name `eq` though.
// This means that we can still refer to `eq` definitions at the top-level scope.
// For example, we can write list equality also as:
export function list_fs_eq(xs, ys, _implicit_fs_eq) /* forall<a> (xs : list<a>, ys : list<a>, ?eq : (a, a) -> bool) -> bool */  { tailcall: while(1)
{
  if (xs !== null) {
    if (ys !== null) {
      if (_implicit_fs_eq(xs.head, ys.head)){
        {
          // tail call
          xs = xs.tail;
          ys = ys.tail;
          continue tailcall;
        }
      }
    }
    return false;
  }
  else {
    return (ys === null);
  }
}}
 
 
// Sometimes we need multiple implicit parameters resolving to the same name.
// In that case we can further qualify the local names. This way each local parameter
// has a unique qualified name, but each one is resolved using the plain `eq` name at the call-site.
// (again, these are the normal disambiguation rules for qualified names, not specific for implicits)
export function tuple_fs_eq(_pat_x161__15, _pat_x161__32, _implicit_fs_a_fs_eq, _implicit_fs_b_fs_eq) /* forall<a,b> ((a, b), (a, b), ?a/eq : (a, a) -> bool, ?b/eq : (b, b) -> bool) -> bool */  {
  var _x0 = _implicit_fs_a_fs_eq(_pat_x161__15.fst, _pat_x161__32.fst);
  if (_x0) {
    return _implicit_fs_b_fs_eq(_pat_x161__15.snd, _pat_x161__32.snd);
  }
  else {
    return false;
  }
}
 
export function example1() /* () -> bool */  {
  return list_eq_explicit($std_core_types.Cons(1, $std_core_types.Cons(2, $std_core_types.Nil)), $std_core_types.Cons(1, $std_core_types.Cons(2, $std_core_types.Nil)), int_fs_eq);
}
 
 
// It can be cumbersome to always pass in the explicit `eq` function.
// We can instead make it an _implicit_ parameter, written as `?eq`.
// Such parameter is automatically resolved by the compiler
// to _any `eq` function at the call site_.
// (where the argument types may need to be instantiated enough to disambiguate)
//
// In other words, any call
//  `list-eq(_,_)`
// is essentially expanded at the call site to
//  `list-eq(_,_,eq)`
//
// So, in the recursive call below, `list-eq(xx,yy)`, the implicit
// parameter is resolved to the local `eq` parameter itself.
// (in VS Code, press `ctrl+alt` to see the implicits as inlay hints)
export function list_eq(xs, ys, _implicit_fs_eq) /* forall<a> (xs : list<a>, ys : list<a>, ?eq : (a, a) -> bool) -> bool */  { tailcall: while(1)
{
  if (xs !== null) {
    if (ys !== null) {
      if (_implicit_fs_eq(xs.head, ys.head)){
        {
          // tail call
          xs = xs.tail;
          ys = ys.tail;
          continue tailcall;
        }
      }
    }
    return false;
  }
  else {
    return (ys === null);
  }
}}
 
 
// In this example, the implicit `?eq` parameter is resolved to `int/eq`.
export function example2() /* () -> bool */  {
  return list_eq($std_core_types.Cons(1, $std_core_types.Cons(2, $std_core_types.Nil)), $std_core_types.Cons(1, $std_core_types.Cons(2, $std_core_types.Nil)), int_fs_eq);
}
 
 
// Sometimes, the implicit parameter can still not be disambiguated.
// In that case we can either use a local definition or an implicit parameter again.
// (ironically, implicit parameters themselves must be declared explicitly :-))
export function eq_refl(xx, _implicit_fs_eq) /* forall<a> (xx : list<a>, ?eq : (a, a) -> bool) -> bool */  {
  return list_eq(xx, xx, _implicit_fs_eq);
}
 
 
// Or we can provide the implicit argument explicitly.
// (this would be like a named instance with type classes)
export function example3() /* () -> bool */  {
  return list_eq($std_core_types.Nil, $std_core_types.Nil, int_fs_eq);
}
 
 
// or unnamed
export function example3a() /* () -> bool */  {
  return list_eq($std_core_types.Nil, $std_core_types.Nil, int_fs_eq);
}
 
 
// or pass any expression
export function example3b() /* () -> bool */  {
  return list_eq($std_core_types.Cons(1, $std_core_types.Cons(2, $std_core_types.Nil)), $std_core_types.Cons(1, $std_core_types.Cons(2, $std_core_types.Nil)), function(_eta_x83__34 /* int */ , _eta_x83__36 /* int */ ) {
       
      var b_10005 = $std_core_types._int_eq(_eta_x83__34,_eta_x83__36);
      return (b_10005) ? false : true;
    });
}
 
 
// Above, the first `eq(x,y` resolves to `?eq(x,y)` (== `@implicit/eq(x,y)`),
// the second `eq(xx,yy)` resolves to `list/eq(xx,yy)`, bypassing
// the local `?eq` parameter (as its fully qualified name is `?eq` (==`@implicit/eq`)).
// The implicit parameter to `list/eq` is resolved to the local parameter `?eq` again.
// (these are just the regular rules for disambiguating qualified names based on types -- nothing specific to implicits)
export function example4() /* () -> bool */  {
  return list_fs_eq($std_core_types.Cons(1, $std_core_types.Cons(2, $std_core_types.Nil)), $std_core_types.Cons(1, $std_core_types.Cons(2, $std_core_types.Nil)), int_fs_eq);
}
 
 
// Implicit parameters are resolved recursively.
// Below, the implicit `?eq` is resolved first to `list/eq`, and
// then the implicit `?eq` of `list/eq` is resolved further to `int/eq`.
// (again, in VS Code press `ctrl+alt` to see the inlay hints)
export function example5() /* () -> bool */  {
  return list_fs_eq($std_core_types.Cons($std_core_types.Cons(1, $std_core_types.Cons(2, $std_core_types.Nil)), $std_core_types.Cons($std_core_types.Nil, $std_core_types.Nil)), $std_core_types.Cons($std_core_types.Cons(1, $std_core_types.Cons(2, $std_core_types.Nil)), $std_core_types.Cons($std_core_types.Nil, $std_core_types.Nil)), function(_arg_x1 /* list<int> */ , _arg_x2 /* list<int> */ ) {
      return list_fs_eq(_arg_x1, _arg_x2, int_fs_eq);
    });
}
 
 
// Normally the compiler does not allow ambigious overloads.
// However, if one of the overloads is in an inner scope it will be preferred.
// In the next example, the `eq` is resolved to `?eq` (==`@implicit/eq`) and not to `int/eq` since it is in an inner scope.
export function example5a(_implicit_fs_eq) /* (?eq : (int, int) -> bool) -> bool */  {
  return _implicit_fs_eq(1, 2);
}
 
 
// In the following example, the implicit `?eq` parameter is first
// resolved to `default/compare/eq`, and then recursively `?compare` to `string/compare`.
export function example6() /* () -> bool */  {
  return list_fs_eq($std_core_types.Cons("hi", $std_core_types.Nil), $std_core_types.Cons("there", $std_core_types.Nil), function(_arg_x1 /* string */ , _arg_x2 /* string */ ) {
      return $std_core_order._lp__eq__eq__rp_($std_core_string.cmp(_arg_x1, _arg_x2), $std_core_types.Eq);
    });
}
 
 
// We can now choose to resolve `?eq` either as `int/eq`, or `default/compare/eq(_,_,?compare=?int/compare)`
// Any ambiguous choice like this is normally rejected by the compiler (and we need to pass the implicit explicitly).
// However, by convention, the locally qualified `default` namespace is assumed to be in an outer
// scope of the global scope -- and due to the "inner scope" rule, the compiler disambiguates to the non-default definition.
// In our case `int/eq` will be preferred over `default/cmp/eq`.
// (note: the `default` scope is still considered experimental)
export function example7() /* () -> bool */  {
  return list_fs_eq($std_core_types.Cons(1, $std_core_types.Cons(2, $std_core_types.Nil)), $std_core_types.Cons(1, $std_core_types.Cons(2, $std_core_types.Nil)), int_fs_eq);
}
 
 
// When taking the scope into consideration, the first choice point determines which one is used.
export function example7a(_implicit_fs_compare) /* forall<a> (?compare : a) -> bool */  {
  return list_fs_eq($std_core_types.Cons(1, $std_core_types.Cons(2, $std_core_types.Nil)), $std_core_types.Cons(1, $std_core_types.Cons(2, $std_core_types.Nil)), int_fs_eq);
}
 
 
// Or without types and explicit qualification
export function tuple_fs_eq2(_pat_x165__16, _pat_x165__25, _implicit_fs_a_fs_eq, _implicit_fs_b_fs_eq) /* forall<a,b,c,d> ((a, b), (c, d), ?a/eq : (a, c) -> bool, ?b/eq : (b, d) -> bool) -> bool */  {
  var _x1 = _implicit_fs_a_fs_eq(_pat_x165__16.fst, _pat_x165__25.fst);
  if (_x1) {
    return _implicit_fs_b_fs_eq(_pat_x165__16.snd, _pat_x165__25.snd);
  }
  else {
    return false;
  }
}
 
 
// As we can see, implicit parameters can really help reduce boilerplate!
export function example8() /* () -> bool */  {
  var _x2 = list_fs_eq($std_core_types.Cons(1, $std_core_types.Nil), $std_core_types.Cons(1, $std_core_types.Nil), int_fs_eq);
  if (_x2) {
    return $std_core_order._lp__eq__eq__rp_($std_core_string.cmp("a", "a"), $std_core_types.Eq);
  }
  else {
    return false;
  }
}
 
export function main() /* () -> console/console () */  {
   
  var _x3 = list_fs_eq($std_core_types.Cons(1, $std_core_types.Nil), $std_core_types.Cons(1, $std_core_types.Nil), int_fs_eq);
  if (_x3) {
    var x_10018 = $std_core_order._lp__eq__eq__rp_($std_core_string.cmp("a", "a"), $std_core_types.Eq);
  }
  else {
    var x_10018 = false;
  }
  var _x3 = (x_10018) ? "True" : "False";
  return $std_core_console.printsln(_x3);
}