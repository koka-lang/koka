// Koka generated module: learn/fip, koka version: 3.2.4
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
import * as $std_core_undiv from './std_core_undiv.mjs';
 
// externals
 
// type declarations
// type tree
export function Bin(left, value, right) /* forall<a> (left : tree<a>, value : a, right : tree<a>) -> tree<a> */  {
  return { left: left, value: value, right: right };
}
export const Tip = null; // forall<a> tree<a>
// type zipper
export function BinL(up, x, right) /* forall<a,b> (up : zipper<a,b>, x : a, right : tree<a>) -> zipper<a,b> */  {
  return { _tag: 1, up: up, x: x, right: right };
}
export function BinR(left, x, up) /* forall<a,b> (left : tree<b>, x : b, up : zipper<a,b>) -> zipper<a,b> */  {
  return { _tag: 2, left: left, x: x, up: up };
}
export const Done = { _tag: 3 }; // forall<a,b> zipper<a,b>
 
// declarations
 
 
// We can use the `fip` keyword to ensure that,
// _if the (non-borrowed) parameters are unique at runtime_,
// the function does not allocate or deallocate memory, and uses constant stack space.
// For example:
export function rev_acc(xs, acc) /* forall<a> (xs : list<a>, acc : list<a>) -> list<a> */  { tailcall: while(1)
{
  if (xs !== null) {
    {
      // tail call
      var _x0 = $std_core_types.Cons(xs.head, acc);
      xs = xs.tail;
      acc = _x0;
      continue tailcall;
    }
  }
  else {
    return acc;
  }
}}
 
export function rev(xs) /* forall<a> (xs : list<a>) -> list<a> */  {
  return rev_acc(xs, $std_core_types.Nil);
}
 
export function example_rev() /* () -> console/console () */  {
   
  var x_10000 = rev_acc($std_core_types.Cons(1, $std_core_types.Cons(2, $std_core_types.Cons(3, $std_core_types.Nil))), $std_core_types.Nil);
  return $std_core_console.printsln($std_core_list.show(x_10000, $std_core_int.show));
}
 
 
// We can still use `rev` persistently as well and have a full
// functional semantics where the argument list is copied when needed.
// The best of both worlds: we can write a purely functional version
// but get in-place update when possible without having to write multiple
// versions of the same functionality, e.g. an in-place updating set and a persistent set
export function example_persistent() /* () -> list<int> */  {
   
  var xs = $std_core_list.range_fs_list(1, 5);
   
  var ys_10004 = rev_acc(xs, $std_core_types.Nil);
  return $std_core_list.append(xs, ys_10004);
}
 
 
// Sometimes, this copying can be unnecessary .. we are working on providing
// better warnings for these situations
export function example_bad() /* () -> console/console () */  {
   
  var xs = $std_core_list.range_fs_list(1, 5);
   
  var ys = rev_acc(xs, $std_core_types.Nil);
   
  var n = $std_core_list._lift_length_6003(xs, 0);
  return $std_core_console.printsln($std_core_tuple.tuple2_fs_show($std_core_types.Tuple2(ys, n), function(_arg_x1_0 /* list<int> */ ) {
      return $std_core_list.show(_arg_x1_0, $std_core_int.show);
    }, $std_core_int.show));
}
 
 
// Similarly, we can write the weaker `fbip` keyword for a function that does not allocate any memory,
// but is allowed to deallocate and use arbitrary stack space.
export function _trmc_filter_odd(xs, _acc) /* (xs : list<int>, ctx<list<int>>) -> list<int> */  { tailcall: while(1)
{
  if (xs !== null) {
    if ($std_core_types._int_isodd((xs.head))) {
       
      var _trmc_x10051 = undefined;
       
      var _trmc_x10052 = $std_core_types.Cons(xs.head, _trmc_x10051);
      {
        // tail call
        var _x1 = $std_core_types._cctx_extend(_acc,_trmc_x10052,({obj: _trmc_x10052, field_name: "tail"}));
        xs = xs.tail;
        _acc = _x1;
        continue tailcall;
      }
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
    return $std_core_types._cctx_apply(_acc,($std_core_types.Nil));
  }
}}
 
 
// Similarly, we can write the weaker `fbip` keyword for a function that does not allocate any memory,
// but is allowed to deallocate and use arbitrary stack space.
export function filter_odd(xs_0) /* (xs : list<int>) -> list<int> */  {
  return _trmc_filter_odd(xs_0, $std_core_types._cctx_empty());
}
 
 
// Automatically generated. Tests for the `Bin` constructor of the `:tree` type.
export function is_bin(tree_0) /* forall<a> (tree : tree<a>) -> bool */  {
  return (tree_0 !== null);
}
 
 
// Automatically generated. Tests for the `Tip` constructor of the `:tree` type.
export function is_tip(tree_0) /* forall<a> (tree : tree<a>) -> bool */  {
  return (tree_0 === null);
}
 
export function _trmc_insert(t, k, _acc) /* (t : tree<int>, k : int, ctx<tree<int>>) -> tree<int> */  { tailcall: while(1)
{
  if (t !== null) {
    if ($std_core_types._int_eq((t.value),k)) {
      return $std_core_types._cctx_apply(_acc,(Bin(t.left, k, t.right)));
    }
    else {
      if ($std_core_types._int_lt((t.value),k)) {
         
        var _trmc_x10053 = undefined;
         
        var _trmc_x10054 = Bin(t.left, t.value, _trmc_x10053);
        {
          // tail call
          var _x2 = $std_core_types._cctx_extend(_acc,_trmc_x10054,({obj: _trmc_x10054, field_name: "right"}));
          t = t.right;
          _acc = _x2;
          continue tailcall;
        }
      }
      else {
         
        var _trmc_x10055 = undefined;
         
        var _trmc_x10056 = Bin(_trmc_x10055, t.value, t.right);
        {
          // tail call
          var _x3 = $std_core_types._cctx_extend(_acc,_trmc_x10056,({obj: _trmc_x10056, field_name: "left"}));
          t = t.left;
          _acc = _x3;
          continue tailcall;
        }
      }
    }
  }
  else {
    return $std_core_types._cctx_apply(_acc,(Bin(Tip, k, Tip)));
  }
}}
 
export function insert(t_0, k_0) /* (t : tree<int>, k : int) -> tree<int> */  {
  return _trmc_insert(t_0, k_0, $std_core_types._cctx_empty());
}
 
 
// Unfortunately, we cannot quite check a recursive polymorphic fip version of `insert` yet
// since we cannot (yet) express second-rank borrow information where the compare
// function does not only need to be borrowed, but borrow its arguments as well
// (e.g. we need `^?cmp : (^a,^a) -> order`).
//
// The `insert-poly` actually _does_ execute in-place at runtime like a `fip(1)`
// function, we just can't check it statically (at this time).
export function _trmc_insert_poly(t, k, _implicit_fs_cmp, _acc) /* forall<a> (t : tree<a>, k : a, ?cmp : (a, a) -> order, ctx<tree<a>>) -> tree<a> */  { tailcall: while(1)
{
  if (t !== null) {
    var _x4 = _implicit_fs_cmp(t.value, k);
    if (_x4 === 2) {
      return $std_core_types._cctx_apply(_acc,(Bin(t.left, k, t.right)));
    }
    else {
      var _x5 = $std_core_order._lp__eq__eq__rp_(_implicit_fs_cmp(t.value, k), $std_core_types.Lt);
      if (_x5) {
         
        var _trmc_x10057 = undefined;
         
        var _trmc_x10058 = Bin(t.left, t.value, _trmc_x10057);
        {
          // tail call
          var _x6 = $std_core_types._cctx_extend(_acc,_trmc_x10058,({obj: _trmc_x10058, field_name: "right"}));
          t = t.right;
          _acc = _x6;
          continue tailcall;
        }
      }
      else {
         
        var _trmc_x10059 = undefined;
         
        var _trmc_x10060 = Bin(_trmc_x10059, t.value, t.right);
        {
          // tail call
          var _x7 = $std_core_types._cctx_extend(_acc,_trmc_x10060,({obj: _trmc_x10060, field_name: "left"}));
          t = t.left;
          _acc = _x7;
          continue tailcall;
        }
      }
    }
  }
  else {
    return $std_core_types._cctx_apply(_acc,(Bin(Tip, k, Tip)));
  }
}}
 
 
// Unfortunately, we cannot quite check a recursive polymorphic fip version of `insert` yet
// since we cannot (yet) express second-rank borrow information where the compare
// function does not only need to be borrowed, but borrow its arguments as well
// (e.g. we need `^?cmp : (^a,^a) -> order`).
//
// The `insert-poly` actually _does_ execute in-place at runtime like a `fip(1)`
// function, we just can't check it statically (at this time).
export function insert_poly(t_0, k_0, _implicit_fs_cmp_0) /* forall<a> (t : tree<a>, k : a, ?cmp : (a, a) -> order) -> tree<a> */  {
  return _trmc_insert_poly(t_0, k_0, _implicit_fs_cmp_0, $std_core_types._cctx_empty());
}
 
 
// We can get around the previous restriction by using the `:order2<a>`
// (value) type which contains not only the `:order`, but also the elements
// themselves in ascending order.
// Instead of a bare comparison, we use an `order2` function that returns
// such an ordered pair -- this way we can keep using the elements `k` and `x` in
// a linear way while being polymorphic over the elements.
// A subtle point here is that `order2` is passed borrowed and as such can itself
// allocate/deallocate; the `fip` annotation only checks if a function is intrinsically
// fully in-place -- modulo the borrowed function parameters. (Consider the `map` function for example).
export function _trmc_insert_order(t, k, _implicit_fs_order2, _acc) /* forall<a> (t : tree<a>, k : a, ?order2 : (a, a) -> order2<a>, ctx<tree<a>>) -> tree<a> */  { tailcall: while(1)
{
  if (t !== null) {
    var _x8 = _implicit_fs_order2(t.value, k);
    if (_x8._tag === 2) {
      return $std_core_types._cctx_apply(_acc,(Bin(t.left, _x8.eq, t.right)));
    }
    else if (_x8._tag === 1) {
       
      var _trmc_x10061 = undefined;
       
      var _trmc_x10062 = Bin(t.left, _x8.lt, _trmc_x10061);
      {
        // tail call
        var _x9 = $std_core_types._cctx_extend(_acc,_trmc_x10062,({obj: _trmc_x10062, field_name: "right"}));
        t = t.right;
        k = _x8.gt;
        _acc = _x9;
        continue tailcall;
      }
    }
    else {
       
      var _trmc_x10063 = undefined;
       
      var _trmc_x10064 = Bin(_trmc_x10063, _x8.gt, t.right);
      {
        // tail call
        var _x10 = $std_core_types._cctx_extend(_acc,_trmc_x10064,({obj: _trmc_x10064, field_name: "left"}));
        t = t.left;
        k = _x8.lt;
        _acc = _x10;
        continue tailcall;
      }
    }
  }
  else {
    return $std_core_types._cctx_apply(_acc,(Bin(Tip, k, Tip)));
  }
}}
 
 
// We can get around the previous restriction by using the `:order2<a>`
// (value) type which contains not only the `:order`, but also the elements
// themselves in ascending order.
// Instead of a bare comparison, we use an `order2` function that returns
// such an ordered pair -- this way we can keep using the elements `k` and `x` in
// a linear way while being polymorphic over the elements.
// A subtle point here is that `order2` is passed borrowed and as such can itself
// allocate/deallocate; the `fip` annotation only checks if a function is intrinsically
// fully in-place -- modulo the borrowed function parameters. (Consider the `map` function for example).
export function insert_order(t_0, k_0, _implicit_fs_order2_0) /* forall<a> (t : tree<a>, k : a, ?order2 : (a, a) -> order2<a>) -> tree<a> */  {
  return _trmc_insert_order(t_0, k_0, _implicit_fs_order2_0, $std_core_types._cctx_empty());
}
 
 
// Return the height of a tree.
export function height(t) /* forall<a> (t : tree<a>) -> int */  {
  if (t === null) {
    return 0;
  }
  else {
     
    var i_10018 = height(t.left);
     
    var j_10019 = height(t.right);
     
    var y_10017 = ($std_core_types._int_ge(i_10018,j_10019)) ? i_10018 : j_10019;
    return $std_core_types._int_add(1,y_10017);
  }
}
 
 
// Return the sum of a tree
export function sum(t) /* (t : tree<int>) -> int */  {
  if (t === null) {
    return 0;
  }
  else {
     
    var y_0_10023 = sum(t.left);
     
    var x_0_10020 = $std_core_types._int_add((t.value),y_0_10023);
     
    var y_10021 = sum(t.right);
    return $std_core_types._int_add(x_0_10020,y_10021);
  }
}
 
 
// Convert a list to a tree given an ordering on the elements
export function tree(elems, _implicit_fs_order2) /* forall<a> (elems : list<a>, ?order2 : (a, a) -> order2<a>) -> tree<a> */  {
  return $std_core_list.foldl(elems, Tip, function(t /* tree<1701> */ , i /* 1701 */ ) {
      return _trmc_insert_order(t, i, _implicit_fs_order2, $std_core_types._cctx_empty());
    });
}
 
export function example_order() /* () -> console/console () */  {
   
  var x_10024 = $std_core_types.Cons(height($std_core_list.foldl($std_core_types.Cons(2, $std_core_types.Cons(3, $std_core_types.Cons(1, $std_core_types.Nil))), Tip, function(t /* tree<int> */ , i /* int */ ) {
        return _trmc_insert_order(t, i, $std_core_int.order2, $std_core_types._cctx_empty());
      })), $std_core_types.Cons(height($std_core_list.foldl($std_core_types.Cons(0x0061, $std_core_types.Cons(0x0062, $std_core_types.Cons(0x0063, $std_core_types.Nil))), Tip, function(t_0_0 /* tree<char> */ , i_0 /* char */ ) {
          return _trmc_insert_order(t_0_0, i_0, $std_core_char.order2, $std_core_types._cctx_empty());
        })), $std_core_types.Cons(height($std_core_list.foldl($std_core_types.Cons(2, $std_core_types.Cons(3, $std_core_types.Cons(1, $std_core_types.Nil))), Tip, function(t_1_0 /* tree<int> */ , i_1 /* int */ ) {
            return _trmc_insert_order(t_1_0, i_1, function(x_0 /* int */ , y /* int */ ) {
                return $std_core_types.Eq2(x_0);
              }, $std_core_types._cctx_empty());
          })), $std_core_types.Nil)));
  return $std_core_console.printsln($std_core_list.show(x_10024, $std_core_int.show));
}
 
 
// Automatically generated. Tests for the `BinL` constructor of the `:zipper` type.
export function is_binL(zipper) /* forall<a,b> (zipper : zipper<a,b>) -> bool */  {
  return (zipper._tag === 1);
}
 
 
// Automatically generated. Tests for the `BinR` constructor of the `:zipper` type.
export function is_binR(zipper) /* forall<a,b> (zipper : zipper<a,b>) -> bool */  {
  return (zipper._tag === 2);
}
 
 
// Automatically generated. Tests for the `Done` constructor of the `:zipper` type.
export function is_done(zipper) /* forall<a,b> (zipper : zipper<a,b>) -> bool */  {
  return (zipper._tag === 3);
}
 
 
// monadic lift
export function _mlift_tmap_up_10073(f_0, r_0, t_0, up, _y_x10068) /* forall<a,b,e> (f@0 : (b) -> <div|e> a, r@0 : tree<b>, t@0 : tree<a>, up : zipper<b,a>, a) -> <div|e> tree<a> */  {
  return tmap_down(r_0, f_0, BinR(t_0, _y_x10068, up));
}
 
 
// traverse down the left spine accumulating a left zipper
export function tmap_down(t, f, acc) /* forall<a,b,e> (t : tree<a>, f : (a) -> <div|e> b, acc : zipper<a,b>) -> <div|e> tree<b> */  { tailcall: while(1)
{
  if (t !== null) {
    {
      // tail call
      var _x11 = BinL(acc, t.value, t.right);
      t = t.left;
      acc = _x11;
      continue tailcall;
    }
  }
  else {
    return tmap_up(Tip, f, acc);
  }
}}
 
 
// and traverse back up visiting all the right nodes recursively
export function tmap_up(t_0_0, f_0_0, acc_0) /* forall<a,b,e> (t : tree<a>, f : (b) -> <div|e> a, acc : zipper<b,a>) -> <div|e> tree<a> */  { tailcall: while(1)
{
  if (acc_0._tag === 1) {
     
    var x_1_10086 = f_0_0(acc_0.x);
    if ($std_core_hnd._yielding()) {
      return $std_core_hnd.yield_extend(function(_y_x10068_0 /* 2364 */ ) {
        return _mlift_tmap_up_10073(f_0_0, acc_0.right, t_0_0, acc_0.up, _y_x10068_0);
      });
    }
    else {
      return tmap_down(acc_0.right, f_0_0, BinR(t_0_0, x_1_10086, acc_0.up));
    }
  }
  else if (acc_0._tag === 2) {
    {
      // tail call
      var _x12 = Bin(acc_0.left, acc_0.x, t_0_0);
      t_0_0 = _x12;
      acc_0 = acc_0.up;
      continue tailcall;
    }
  }
  else {
    return t_0_0;
  }
}}
 
 
// Map a function `f` over all element of a tree.
export function tmap(t, f) /* forall<a,b,e> (t : tree<a>, f : (a) -> <div|e> b) -> <div|e> tree<b> */  {
  return tmap_down(t, f, Done);
}
 
export function example_tmap() /* () -> div int */  {
   
  var elems_10034 = $std_core_list.range_fs_list(1, 10);
   
  var t_10032 = $std_core_list.foldl(elems_10034, Tip, function(t_0 /* tree<int> */ , i /* int */ ) {
      return _trmc_insert_order(t_0, i, $std_core_int.order2, $std_core_types._cctx_empty());
    });
  return sum(tmap_down(t_10032, $std_core_int.inc, Done));
}
 
export function main() /* () -> <console/console,div> () */  {
   
  var xs = $std_core_list.range_fs_list(1, 5);
   
  var ys_10039 = rev_acc(xs, $std_core_types.Nil);
   
  var x_10036 = $std_core_list.append(xs, ys_10039);
   
  $std_core_console.printsln($std_core_list.show(x_10036, $std_core_int.show));
   
  var elems_10045 = $std_core_list.range_fs_list(1, 10);
   
  var t_10043 = $std_core_list.foldl(elems_10045, Tip, function(t_0 /* tree<int> */ , i /* int */ ) {
      return _trmc_insert_order(t_0, i, $std_core_int.order2, $std_core_types._cctx_empty());
    });
   
  var x_0_10041 = sum(tmap_down(t_10043, $std_core_int.inc, Done));
  return $std_core_console.printsln($std_core_int.show(x_0_10041));
}