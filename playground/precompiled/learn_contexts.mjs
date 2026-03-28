// Koka generated module: learn/contexts, koka version: 3.2.4
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
 
 
// monadic lift
export function _mlift_trmc_map_std_10069(_acc, f, xx, _trmc_x10022) /* forall<a,b,e> (ctx<list<b>>, f : (a) -> e b, xx : list<a>, b) -> e list<b> */  {
   
  var _trmc_x10023 = undefined;
   
  var _trmc_x10024 = $std_core_types.Cons(_trmc_x10022, _trmc_x10023);
  return _trmc_map_std(xx, f, $std_core_types._cctx_extend(_acc,_trmc_x10024,({obj: _trmc_x10024, field_name: "tail"})));
}
 
 
// monadic lift
export function _mlift_trmcm_map_std_10070(_accm, f_0, xx_0, _trmc_x10027) /* forall<a,b,e> ((list<b>) -> list<b>, f : (a) -> e b, xx : list<a>, b) -> e list<b> */  {
  return _trmcm_map_std(xx_0, f_0, function(_trmc_x10026 /* list<167> */ ) {
      return _accm($std_core_types.Cons(_trmc_x10027, _trmc_x10026));
    });
}
 
 
// Consider the well-known `map` function that maps a function over a list
export function _trmc_map_std(xs, f_1, _acc_0) /* forall<a,b,e> (xs : list<a>, f : (a) -> e b, ctx<list<b>>) -> e list<b> */  { tailcall: while(1)
{
  if (xs !== null) {
     
    var x_0_10076 = f_1(xs.head);
    if ($std_core_hnd._yielding()) {
      return $std_core_hnd.yield_extend(function(_trmc_x10022_0 /* 167 */ ) {
        return _mlift_trmc_map_std_10069(_acc_0, f_1, xs.tail, _trmc_x10022_0);
      });
    }
    else {
       
      var _trmc_x10023_0 = undefined;
       
      var _trmc_x10024_0 = $std_core_types.Cons(x_0_10076, _trmc_x10023_0);
      {
        // tail call
        var _x0 = $std_core_types._cctx_extend(_acc_0,_trmc_x10024_0,({obj: _trmc_x10024_0, field_name: "tail"}));
        xs = xs.tail;
        _acc_0 = _x0;
        continue tailcall;
      }
    }
  }
  else {
    return $std_core_types._cctx_apply(_acc_0,($std_core_types.Nil));
  }
}}
 
 
// Consider the well-known `map` function that maps a function over a list
export function _trmcm_map_std(xs_0, f_2, _accm_0) /* forall<a,b,e> (xs : list<a>, f : (a) -> e b, (list<b>) -> list<b>) -> e list<b> */  { tailcall: while(1)
{
  if (xs_0 !== null) {
     
    var x_2_10079 = f_2(xs_0.head);
    if ($std_core_hnd._yielding()) {
      return $std_core_hnd.yield_extend(function(_trmc_x10027_0 /* 167 */ ) {
        return _mlift_trmcm_map_std_10070(_accm_0, f_2, xs_0.tail, _trmc_x10027_0);
      });
    }
    else {
      {
        // tail call
        var _x3 = function(__at_accm_01 /* (list<167>) -> list<167> */ , _x_2_100792 /* 167 */ ) {
          return function(_trmc_x10026_0 /* list<167> */ ) {
            return __at_accm_01($std_core_types.Cons(_x_2_100792, _trmc_x10026_0));
          };
        }(_accm_0, x_2_10079);
        xs_0 = xs_0.tail;
        _accm_0 = _x3;
        continue tailcall;
      }
    }
  }
  else {
    return _accm_0($std_core_types.Nil);
  }
}}
 
 
// Consider the well-known `map` function that maps a function over a list
export function map_std(xs_1, f_3) /* forall<a,b,e> (xs : list<a>, f : (a) -> e b) -> e list<b> */  {
  var _x4 = $std_core_hnd._evv_is_affine();
  if (_x4) {
    return _trmc_map_std(xs_1, f_3, $std_core_types._cctx_empty());
  }
  else {
    return _trmcm_map_std(xs_1, f_3, function(_trmc_x10025 /* list<167> */ ) {
        return _trmc_x10025;
      });
  }
}
 
 
// monadic lift
export function _mlift_trmc_map_tail_10071(_acc, f, xx, _trmc_x10028) /* forall<a,b,e> (ctx<list<b>>, f : (a) -> e b, xx : list<a>, b) -> e list<b> */  {
   
  var _trmc_x10029 = undefined;
   
  var _trmc_x10030 = $std_core_types.Cons(_trmc_x10028, _trmc_x10029);
  return _trmc_map_tail(xx, f, $std_core_types._cctx_extend(_acc,_trmc_x10030,({obj: _trmc_x10030, field_name: "tail"})));
}
 
 
// monadic lift
export function _mlift_trmcm_map_tail_10072(_accm, f_0, xx_0, _trmc_x10033) /* forall<a,b,e> ((list<b>) -> list<b>, f : (a) -> e b, xx : list<a>, b) -> e list<b> */  {
  return _trmcm_map_tail(xx_0, f_0, function(_trmc_x10032 /* list<231> */ ) {
      return _accm($std_core_types.Cons(_trmc_x10033, _trmc_x10032));
    });
}
 
 
// Naively, the `map-std` function would use stack space linear in the size
// of the first list as it is not directly tail recursive. It turns out
// that Koka can actually optimize tail recursive calls under a series
// of constructors and `map-std` will be compiled in an efficient way.
// See the paper:
//
// >  "Tail Recursion Modulo Context: An Equational Approach",
// >  Daan Leijen and Anton Lorenzen, POPL'23,
// >  <https://www.microsoft.com/en-us/research/publication/tail-recursion-modulo-context-an-equational-approach-2/>
//
// To be sure this optimization happens, we can write `tail fun` to get
// a warning if our function is not quite TRMC.
export function _trmc_map_tail(xs, f_1, _acc_0) /* forall<a,b,e> (xs : list<a>, f : (a) -> e b, ctx<list<b>>) -> e list<b> */  { tailcall: while(1)
{
  if (xs !== null) {
     
    var x_0_10082 = f_1(xs.head);
    if ($std_core_hnd._yielding()) {
      return $std_core_hnd.yield_extend(function(_trmc_x10028_0 /* 231 */ ) {
        return _mlift_trmc_map_tail_10071(_acc_0, f_1, xs.tail, _trmc_x10028_0);
      });
    }
    else {
       
      var _trmc_x10029_0 = undefined;
       
      var _trmc_x10030_0 = $std_core_types.Cons(x_0_10082, _trmc_x10029_0);
      {
        // tail call
        var _x5 = $std_core_types._cctx_extend(_acc_0,_trmc_x10030_0,({obj: _trmc_x10030_0, field_name: "tail"}));
        xs = xs.tail;
        _acc_0 = _x5;
        continue tailcall;
      }
    }
  }
  else {
    return $std_core_types._cctx_apply(_acc_0,($std_core_types.Nil));
  }
}}
 
 
// Naively, the `map-std` function would use stack space linear in the size
// of the first list as it is not directly tail recursive. It turns out
// that Koka can actually optimize tail recursive calls under a series
// of constructors and `map-std` will be compiled in an efficient way.
// See the paper:
//
// >  "Tail Recursion Modulo Context: An Equational Approach",
// >  Daan Leijen and Anton Lorenzen, POPL'23,
// >  <https://www.microsoft.com/en-us/research/publication/tail-recursion-modulo-context-an-equational-approach-2/>
//
// To be sure this optimization happens, we can write `tail fun` to get
// a warning if our function is not quite TRMC.
export function _trmcm_map_tail(xs_0, f_2, _accm_0) /* forall<a,b,e> (xs : list<a>, f : (a) -> e b, (list<b>) -> list<b>) -> e list<b> */  { tailcall: while(1)
{
  if (xs_0 !== null) {
     
    var x_2_10085 = f_2(xs_0.head);
    if ($std_core_hnd._yielding()) {
      return $std_core_hnd.yield_extend(function(_trmc_x10033_0 /* 231 */ ) {
        return _mlift_trmcm_map_tail_10072(_accm_0, f_2, xs_0.tail, _trmc_x10033_0);
      });
    }
    else {
      {
        // tail call
        var _x8 = function(__at_accm_06 /* (list<231>) -> list<231> */ , _x_2_100857 /* 231 */ ) {
          return function(_trmc_x10032_0 /* list<231> */ ) {
            return __at_accm_06($std_core_types.Cons(_x_2_100857, _trmc_x10032_0));
          };
        }(_accm_0, x_2_10085);
        xs_0 = xs_0.tail;
        _accm_0 = _x8;
        continue tailcall;
      }
    }
  }
  else {
    return _accm_0($std_core_types.Nil);
  }
}}
 
 
// Naively, the `map-std` function would use stack space linear in the size
// of the first list as it is not directly tail recursive. It turns out
// that Koka can actually optimize tail recursive calls under a series
// of constructors and `map-std` will be compiled in an efficient way.
// See the paper:
//
// >  "Tail Recursion Modulo Context: An Equational Approach",
// >  Daan Leijen and Anton Lorenzen, POPL'23,
// >  <https://www.microsoft.com/en-us/research/publication/tail-recursion-modulo-context-an-equational-approach-2/>
//
// To be sure this optimization happens, we can write `tail fun` to get
// a warning if our function is not quite TRMC.
export function map_tail(xs_1, f_3) /* forall<a,b,e> (xs : list<a>, f : (a) -> e b) -> e list<b> */  {
  var _x9 = $std_core_hnd._evv_is_affine();
  if (_x9) {
    return _trmc_map_tail(xs_1, f_3, $std_core_types._cctx_empty());
  }
  else {
    return _trmcm_map_tail(xs_1, f_3, function(_trmc_x10031 /* list<231> */ ) {
        return _trmc_x10031;
      });
  }
}
 
export function test_tail() /* () -> console/console () */  {
   
  var x_10000 = $std_core_list.map($std_core_types.Cons(1, $std_core_types.Cons(2, $std_core_types.Cons(3, $std_core_types.Nil))), $std_core_int.inc);
  return $std_core_console.printsln($std_core_list.show(x_10000, $std_core_int.show));
}
 
 
// monadic lift
export function _mlift_map_acc_10073(acc, f, xx, _y_x10054) /* forall<a,b,e> (acc : list<b>, f : (a) -> e b, xx : list<a>, b) -> e list<b> */  {
  return map_acc(xx, f, $std_core_types.Cons(_y_x10054, acc));
}
 
 
// However, the TRMC optimization cannot always apply. How would we
// then write an efficient `map` function? Commonly, functional programmers
// use an accumulator to write a tail-recursive version of `map`.
export function map_acc(xs, f_0, acc_0) /* forall<a,b,e> (xs : list<a>, f : (a) -> e b, acc : list<b>) -> e list<b> */  { tailcall: while(1)
{
  if (xs !== null) {
     
    var x_0_10088 = f_0(xs.head);
    if ($std_core_hnd._yielding()) {
      return $std_core_hnd.yield_extend(function(_y_x10054_0 /* 533 */ ) {
        return _mlift_map_acc_10073(acc_0, f_0, xs.tail, _y_x10054_0);
      });
    }
    else {
      {
        // tail call
        var _x10 = $std_core_types.Cons(x_0_10088, acc_0);
        xs = xs.tail;
        acc_0 = _x10;
        continue tailcall;
      }
    }
  }
  else {
    return $std_core_list.reverse_acc($std_core_types.Nil, acc_0);
  }
}}
 
export function map_acc_entry(xs, f) /* forall<a,b,e> (xs : list<a>, f : (a) -> e b) -> e list<b> */  {
  return map_acc(xs, f, $std_core_types.Nil);
}
 
export function test_acc() /* () -> console/console () */  {
   
  var x_10002 = map_acc($std_core_types.Cons(1, $std_core_types.Cons(2, $std_core_types.Cons(3, $std_core_types.Nil))), $std_core_int.inc, $std_core_types.Nil);
  return $std_core_console.printsln($std_core_list.show(x_10002, $std_core_int.show));
}
 
 
// monadic lift
export function _mlift_map_td_10074(acc, f, xx, _y_x10058) /* forall<a,b,e> (acc : ctx<list<b>>, f : (a) -> e b, xx : list<a>, b) -> e list<b> */  {
   
  var _cctx_x796 = $std_core_types.Cons(_y_x10058, undefined);
   
  var _cctx_x797 = {obj: _cctx_x796, field_name: "tail"};
  return map_td(xx, f, $std_core_types._cctx_compose(acc,($std_core_types._cctx_create(_cctx_x796,_cctx_x797))));
}
 
 
// We can create a context using the `ctx` keyword. A context must
// consist of constructors and have a single _hole_ (written as `hole`).
// For example, `ctx Cons(1,Cons(2,hole))` or `ctx Bin(Bin(Leaf 1,hole),Leaf 3)`
// There are two operations on context, append and apply:
//
// `(++)  : (cctx<a,b>, cctx<b,c>) -> cctx<a,c>`  // append
// `(++.) : (cctx<a,b>, b) -> a`                  // apply
//
// Append takes a context of type `:a` with hole `:b` and writes a
// a context `cctx<b,c>` into the hole to get a new context of type
// `:a` with hole `:c`. Apply `(.++)` plugs the hole `:b` in a
// context `cctx<a,b>` and returns _the whole structure_ `:a`.
// Often the hole and the structure are of the same type, and
// we have the `alias ctx<a> = cctx<a,a>`.
//
// Now we can write a top-down `map` ourselves: (note: we don't need `tail` here as `fip` implies `tail` already)
export function map_td(xs, f_0, acc_0) /* forall<a,b,e> (xs : list<a>, f : (a) -> e b, acc : ctx<list<b>>) -> e list<b> */  { tailcall: while(1)
{
  if (xs !== null) {
     
    var x_0_10091 = f_0(xs.head);
    if ($std_core_hnd._yielding()) {
      return $std_core_hnd.yield_extend(function(_y_x10058_0 /* 831 */ ) {
        return _mlift_map_td_10074(acc_0, f_0, xs.tail, _y_x10058_0);
      });
    }
    else {
       
      var _cctx_x796_0 = $std_core_types.Cons(x_0_10091, undefined);
       
      var _cctx_x797_0 = {obj: _cctx_x796_0, field_name: "tail"};
      {
        // tail call
        var _x11 = $std_core_types._cctx_compose(acc_0,($std_core_types._cctx_create(_cctx_x796_0,_cctx_x797_0)));
        xs = xs.tail;
        acc_0 = _x11;
        continue tailcall;
      }
    }
  }
  else {
    return $std_core_types._cctx_apply(acc_0,($std_core_types.Nil));
  }
}}
 
export function map_td_entry(xs, f) /* forall<a,b,e> (xs : list<a>, f : (a) -> e b) -> e list<b> */  {
  return map_td(xs, f, $std_core_types._cctx_empty());
}
 
export function test_td() /* () -> console/console () */  {
   
  var x_10006 = map_td($std_core_types.Cons(1, $std_core_types.Cons(2, $std_core_types.Cons(3, $std_core_types.Nil))), $std_core_int.inc, $std_core_types._cctx_empty());
  return $std_core_console.printsln($std_core_list.show(x_10006, $std_core_int.show));
}
 
export function _trmc_concat(xs, ys, _acc) /* forall<a> (xs : list<a>, ys : list<a>, ctx<list<a>>) -> list<a> */  { tailcall: while(1)
{
  if (xs !== null) {
     
    var _trmc_x10034 = undefined;
     
    var _trmc_x10035 = $std_core_types.Cons(xs.head, _trmc_x10034);
    {
      // tail call
      var _x12 = $std_core_types._cctx_extend(_acc,_trmc_x10035,({obj: _trmc_x10035, field_name: "tail"}));
      xs = xs.tail;
      _acc = _x12;
      continue tailcall;
    }
  }
  else {
    return $std_core_types._cctx_apply(_acc,ys);
  }
}}
 
export function concat(xs_0, ys_0) /* forall<a> (xs : list<a>, ys : list<a>) -> list<a> */  {
  return _trmc_concat(xs_0, ys_0, $std_core_types._cctx_empty());
}
 
export function flatten(xss) /* forall<a> (xss : list<list<a>>) -> list<a> */  {
  if (xss !== null) {
     
    var ys_10095 = flatten(xss.tail);
    return _trmc_concat(xss.head, ys_10095, $std_core_types._cctx_empty());
  }
  else {
    return $std_core_types.Nil;
  }
}
 
 
// Here, `concat` is TRMC, but `flatten` is not (since the recursive call is inside a function call)
// With constructor contexts though we can represent the flattened list as a context
// with a hole in the tail element:
export function concat_td(xs, acc) /* forall<a> (xs : list<a>, acc : ctx<list<a>>) -> ctx<list<a>> */  { tailcall: while(1)
{
  if (xs !== null) {
     
    var _cctx_x1188 = $std_core_types.Cons(xs.head, undefined);
     
    var _cctx_x1189 = {obj: _cctx_x1188, field_name: "tail"};
    {
      // tail call
      var _x13 = $std_core_types._cctx_compose(acc,($std_core_types._cctx_create(_cctx_x1188,_cctx_x1189)));
      xs = xs.tail;
      acc = _x13;
      continue tailcall;
    }
  }
  else {
    return acc;
  }
}}
 
export function flatten_td(xss, acc) /* forall<a> (xss : list<list<a>>, acc : ctx<list<a>>) -> ctx<list<a>> */  { tailcall: while(1)
{
  if (xss !== null) {
    {
      // tail call
      var _x14 = concat_td(xss.head, acc);
      xss = xss.tail;
      acc = _x14;
      continue tailcall;
    }
  }
  else {
    return acc;
  }
}}
 
export function flatten_entry(xss) /* forall<a> (xss : list<list<a>>) -> list<a> */  {
  return $std_core_types._cctx_apply((flatten_td(xss, $std_core_types._cctx_empty())),($std_core_types.Nil));
}
 
export function example_flatten() /* () -> console/console () */  {
   
  var x_10010 = $std_core_types._cctx_apply((flatten_td($std_core_types.Cons($std_core_types.Cons(1, $std_core_types.Cons(2, $std_core_types.Nil)), $std_core_types.Cons($std_core_types.Cons(3, $std_core_types.Cons(4, $std_core_types.Nil)), $std_core_types.Cons($std_core_types.Nil, $std_core_types.Nil))), $std_core_types._cctx_empty())),($std_core_types.Nil));
  return $std_core_console.printsln($std_core_list.show(x_10010, $std_core_int.show));
}
 
 
// monadic lift
export function _mlift_partition_td_10075(acc1, acc2, pred, x, xx, _y_x10062) /* forall<a,e> (acc1 : ctx<list<a>>, acc2 : ctx<list<a>>, pred : (a) -> e bool, x : a, xx : list<a>, bool) -> e (list<a>, list<a>) */  {
  if (_y_x10062) {
     
    var _cctx_x1579 = $std_core_types.Cons(x, undefined);
     
    var _cctx_x1580 = {obj: _cctx_x1579, field_name: "tail"};
    return partition_td(xx, pred, $std_core_types._cctx_compose(acc1,($std_core_types._cctx_create(_cctx_x1579,_cctx_x1580))), acc2);
  }
  else {
     
    var _cctx_x1626 = $std_core_types.Cons(x, undefined);
     
    var _cctx_x1627 = {obj: _cctx_x1626, field_name: "tail"};
    return partition_td(xx, pred, acc1, $std_core_types._cctx_compose(acc2,($std_core_types._cctx_create(_cctx_x1626,_cctx_x1627))));
  }
}
 
 
// Another example is the `partition` function where we
// need two contexts to build the two result lists in one
// traversal.
export function partition_td(xs, pred_0, acc1_0, acc2_0) /* forall<a,e> (xs : list<a>, pred : (a) -> e bool, acc1 : ctx<list<a>>, acc2 : ctx<list<a>>) -> e (list<a>, list<a>) */  { tailcall: while(1)
{
  if (xs === null) {
    return $std_core_types.Tuple2($std_core_types._cctx_apply(acc1_0,($std_core_types.Nil)), $std_core_types._cctx_apply(acc2_0,($std_core_types.Nil)));
  }
  else {
     
    var x_1_10096 = pred_0(xs.head);
    if ($std_core_hnd._yielding()) {
      return $std_core_hnd.yield_extend(function(_y_x10062_0 /* bool */ ) {
        return _mlift_partition_td_10075(acc1_0, acc2_0, pred_0, xs.head, xs.tail, _y_x10062_0);
      });
    }
    else {
      if (x_1_10096) {
         
        var _cctx_x1579_0 = $std_core_types.Cons(xs.head, undefined);
         
        var _cctx_x1580_0 = {obj: _cctx_x1579_0, field_name: "tail"};
        {
          // tail call
          var _x15 = $std_core_types._cctx_compose(acc1_0,($std_core_types._cctx_create(_cctx_x1579_0,_cctx_x1580_0)));
          xs = xs.tail;
          acc1_0 = _x15;
          continue tailcall;
        }
      }
      else {
         
        var _cctx_x1626_0 = $std_core_types.Cons(xs.head, undefined);
         
        var _cctx_x1627_0 = {obj: _cctx_x1626_0, field_name: "tail"};
        {
          // tail call
          var _x16 = $std_core_types._cctx_compose(acc2_0,($std_core_types._cctx_create(_cctx_x1626_0,_cctx_x1627_0)));
          xs = xs.tail;
          acc2_0 = _x16;
          continue tailcall;
        }
      }
    }
  }
}}
 
export function partition_entry(xs, pred) /* forall<a,e> (xs : list<a>, pred : (a) -> e bool) -> e (list<a>, list<a>) */  {
  return partition_td(xs, pred, $std_core_types._cctx_empty(), $std_core_types._cctx_empty());
}
 
export function example_partition() /* () -> (list<int>, list<int>) */  {
  return partition_td($std_core_types.Cons(1, $std_core_types.Cons(2, $std_core_types.Cons(3, $std_core_types.Cons(4, $std_core_types.Cons(5, $std_core_types.Nil))))), (function(_x17) {
      return $std_core_types._int_isodd(_x17);
    }), $std_core_types._cctx_empty(), $std_core_types._cctx_empty());
}
 
export function main() /* () -> console/console () */  {
   
  var x_10015 = partition_td($std_core_types.Cons(1, $std_core_types.Cons(2, $std_core_types.Cons(3, $std_core_types.Cons(4, $std_core_types.Cons(5, $std_core_types.Nil))))), (function(_x18) {
      return $std_core_types._int_isodd(_x18);
    }), $std_core_types._cctx_empty(), $std_core_types._cctx_empty());
  return $std_core_console.printsln($std_core_tuple.tuple2_fs_show(x_10015, function(_arg_x1_0 /* list<int> */ ) {
      return $std_core_list.show(_arg_x1_0, $std_core_int.show);
    }, function(_arg_x1_1 /* list<int> */ ) {
      return $std_core_list.show(_arg_x1_1, $std_core_int.show);
    }));
}