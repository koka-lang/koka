// Koka generated module: std/core/list, koka version: 3.2.4
"use strict";
 
// imports
import * as $std_core_types from './std_core_types.mjs';
import * as $std_core_undiv from './std_core_undiv.mjs';
import * as $std_core_hnd from './std_core_hnd.mjs';
import * as $std_core_exn from './std_core_exn.mjs';
import * as $std_core_char from './std_core_char.mjs';
import * as $std_core_string from './std_core_string.mjs';
import * as $std_core_int from './std_core_int.mjs';
import * as $std_core_vector from './std_core_vector.mjs';
 
// externals
 
// type declarations
 
// declarations
 
 
// Return the head of list if the list is not empty.
export function maybe_fs_head(xs) /* forall<a> (xs : list<a>) -> maybe<a> */  {
  if (xs !== null) {
    return $std_core_types.Just(xs.head);
  }
  else {
    return $std_core_types.Nothing;
  }
}
 
 
// Return the head of list with a default value in case the list is empty.
export function head(xs, $default) /* forall<a> (xs : list<a>, default : a) -> a */  {
  return (xs !== null) ? xs.head : $default;
}
 
 
// Return the tail of list. Returns the empty list if `xs` is empty.
export function tail(xs) /* forall<a> (xs : list<a>) -> list<a> */  {
  return (xs !== null) ? xs.tail : $std_core_types.Nil;
}
 
 
// Is the list empty?
export function is_empty(xs) /* forall<a> (xs : list<a>) -> bool */  {
  return (xs === null);
}
 
 
// Returns a singleton list.
export function single(x) /* forall<a> (x : a) -> list<a> */  {
  return $std_core_types.Cons(x, $std_core_types.Nil);
}
 
 
// lifted local: length, len
export function _lift_length_6003(ys, acc) /* forall<a> (ys : list<a>, acc : int) -> int */  { tailcall: while(1)
{
  if (ys !== null) {
    {
      // tail call
      var _x0 = $std_core_types._int_add(acc,1);
      ys = ys.tail;
      acc = _x0;
      continue tailcall;
    }
  }
  else {
    return acc;
  }
}}
 
 
// Returns the length of a list.
export function length(xs) /* forall<a> (xs : list<a>) -> int */  {
  return _lift_length_6003(xs, 0);
}
 
 
// monadic lift
export function _lp__at_mlift_x_10410_eq__eq__rp_(_implicit_fs__lp__eq__eq__rp_, xx, yy, _y_x10140) /* forall<a,e> (?(==) : (a, a) -> e bool, xx : list<a>, yy : list<a>, bool) -> e bool */  {
  if (_y_x10140) {
    return _lp__eq__eq__rp_(xx, yy, _implicit_fs__lp__eq__eq__rp_);
  }
  else {
    return false;
  }
}
 
 
// Element-wise list equality
export function _lp__eq__eq__rp_(xs, ys, _implicit_fs__lp__at_x_0_eq__eq__rp_) /* forall<a,e> (xs : list<a>, ys : list<a>, ?(==) : (a, a) -> e bool) -> e bool */  { tailcall: while(1)
{
  if (xs !== null) {
    if (ys === null) {
      return false;
    }
    else {
       
      var x_0_10458 = _implicit_fs__lp__at_x_0_eq__eq__rp_(xs.head, ys.head);
      if ($std_core_hnd._yielding()) {
        return $std_core_hnd.yield_extend(function(_y_x10140_0 /* bool */ ) {
          return _lp__at_mlift_x_10410_eq__eq__rp_(_implicit_fs__lp__at_x_0_eq__eq__rp_, xs.tail, ys.tail, _y_x10140_0);
        });
      }
      else {
        if (x_0_10458) {
          {
            // tail call
            xs = xs.tail;
            ys = ys.tail;
            continue tailcall;
          }
        }
        else {
          return false;
        }
      }
    }
  }
  else {
    return (ys === null);
  }
}}
 
 
// monadic lift
export function _mlift_cmp_10411(_implicit_fs_cmp, xx, yy, _y_x10145) /* forall<a,e> (?cmp : (a, a) -> e order, xx : list<a>, yy : list<a>, order) -> e order */  {
  if (_y_x10145 === 2) {
    return cmp(xx, yy, _implicit_fs_cmp);
  }
  else {
    return _y_x10145;
  }
}
 
 
// Order on lists
export function cmp(xs, ys, _implicit_fs_cmp_0) /* forall<a,e> (xs : list<a>, ys : list<a>, ?cmp : (a, a) -> e order) -> e order */  { tailcall: while(1)
{
  if (xs !== null) {
    if (ys === null) {
      return $std_core_types.Gt;
    }
    else {
       
      var x_0_10461 = _implicit_fs_cmp_0(xs.head, ys.head);
      if ($std_core_hnd._yielding()) {
        return $std_core_hnd.yield_extend(function(_y_x10145_0 /* order */ ) {
          return _mlift_cmp_10411(_implicit_fs_cmp_0, xs.tail, ys.tail, _y_x10145_0);
        });
      }
      else {
        if (x_0_10461 === 2) {
          {
            // tail call
            xs = xs.tail;
            ys = ys.tail;
            continue tailcall;
          }
        }
        else {
          return x_0_10461;
        }
      }
    }
  }
  else {
    return (ys === null) ? $std_core_types.Eq : $std_core_types.Lt;
  }
}}
 
 
// Take the first `n` elements of a list (or fewer if the list is shorter than `n`)
export function _trmc_take(xs, n, _acc) /* forall<a> (xs : list<a>, n : int, ctx<list<a>>) -> list<a> */  { tailcall: while(1)
{
  if (xs !== null) {
    if ($std_core_types._int_gt(n,0)){
       
      var _trmc_x10048 = undefined;
       
      var _trmc_x10049 = $std_core_types.Cons(xs.head, _trmc_x10048);
      {
        // tail call
        var _x1 = $std_core_types._int_sub(n,1);
        var _x2 = $std_core_types._cctx_extend(_acc,_trmc_x10049,({obj: _trmc_x10049, field_name: "tail"}));
        xs = xs.tail;
        n = _x1;
        _acc = _x2;
        continue tailcall;
      }
    }
  }
  return $std_core_types._cctx_apply(_acc,($std_core_types.Nil));
}}
 
 
// Take the first `n` elements of a list (or fewer if the list is shorter than `n`)
export function take(xs_0, n_0) /* forall<a> (xs : list<a>, n : int) -> list<a> */  {
  return _trmc_take(xs_0, n_0, $std_core_types._cctx_empty());
}
 
 
// Drop the first `n` elements of a list (or fewer if the list is shorter than `n`)
export function drop(xs, n) /* forall<a> (xs : list<a>, n : int) -> list<a> */  { tailcall: while(1)
{
  if (xs !== null) {
    if ($std_core_types._int_gt(n,0)){
      {
        // tail call
        var _x3 = $std_core_types._int_sub(n,1);
        xs = xs.tail;
        n = _x3;
        continue tailcall;
      }
    }
  }
  return xs;
}}
 
export function reverse_acc(acc, ys) /* forall<a> (acc : list<a>, ys : list<a>) -> list<a> */  { tailcall: while(1)
{
  if (ys !== null) {
    {
      // tail call
      var _x4 = $std_core_types.Cons(ys.head, acc);
      acc = _x4;
      ys = ys.tail;
      continue tailcall;
    }
  }
  else {
    return acc;
  }
}}
 
 
// Efficiently reverse a list `xs` and append it to `tl`:
// `reverse-append(xs,tl) == reverse(xs) ++ tl
export function reverse_append(xs, tl) /* forall<a> (xs : list<a>, tl : list<a>) -> list<a> */  {
  return reverse_acc(tl, xs);
}
 
 
// Reverse a list.
export function reverse(xs) /* forall<a> (xs : list<a>) -> list<a> */  {
  return reverse_acc($std_core_types.Nil, xs);
}
 
 
// Append two lists.
export function _trmc_append(xs, ys, _acc) /* forall<a> (xs : list<a>, ys : list<a>, ctx<list<a>>) -> list<a> */  { tailcall: while(1)
{
  if (xs !== null) {
     
    var _trmc_x10050 = undefined;
     
    var _trmc_x10051 = $std_core_types.Cons(xs.head, _trmc_x10050);
    {
      // tail call
      var _x5 = $std_core_types._cctx_extend(_acc,_trmc_x10051,({obj: _trmc_x10051, field_name: "tail"}));
      xs = xs.tail;
      _acc = _x5;
      continue tailcall;
    }
  }
  else {
    return $std_core_types._cctx_apply(_acc,ys);
  }
}}
 
 
// Append two lists.
export function append(xs_0, ys_0) /* forall<a> (xs : list<a>, ys : list<a>) -> list<a> */  {
  return _trmc_append(xs_0, ys_0, $std_core_types._cctx_empty());
}
 
 
// Append two lists.
export function _lp__plus__plus__rp_(xs, ys) /* forall<a> (xs : list<a>, ys : list<a>) -> list<a> */  {
  return _trmc_append(xs, ys, $std_core_types._cctx_empty());
}
 
 
// monadic lift
export function _mlift_foldl_10412(f, xx, _y_x10150) /* forall<a,b,e> (f : (b, a) -> e b, xx : list<a>, b) -> e b */  {
  return foldl(xx, _y_x10150, f);
}
 
 
// Fold a list from the left, i.e. `foldl([1,2],0,(+)) == (0+1)+2`
// Since `foldl` is tail recursive, it is preferred over `foldr` when using an associative function `f`
export function foldl(xs, z, f_0) /* forall<a,b,e> (xs : list<a>, z : b, f : (b, a) -> e b) -> e b */  { tailcall: while(1)
{
  if (xs !== null) {
     
    var x_0_10466 = f_0(z, xs.head);
    if ($std_core_hnd._yielding()) {
      return $std_core_hnd.yield_extend(function(_y_x10150_0 /* 1203 */ ) {
        return _mlift_foldl_10412(f_0, xs.tail, _y_x10150_0);
      });
    }
    else {
      {
        // tail call
        xs = xs.tail;
        z = x_0_10466;
        continue tailcall;
      }
    }
  }
  else {
    return z;
  }
}}
 
 
// Fold a list from the right, i.e. `foldr([1,2],0,(+)) == 1+(2+0)`
// Note, `foldr` is less efficient than `foldl` as it reverses the list first.
export function foldr(xs, z, f) /* forall<a,b,e> (xs : list<a>, z : b, f : (a, b) -> e b) -> e b */  {
  return foldl(reverse_acc($std_core_types.Nil, xs), z, function(x /* 1256 */ , y /* 1255 */ ) {
      return f(y, x);
    });
}
 
export function foldl1(xs, f) /* forall<a,e> (xs : list<a>, f : (a, a) -> <exn|e> a) -> <exn|e> a */  {
  if (xs !== null) {
    return foldl(xs.tail, xs.head, f);
  }
  else {
    return $std_core_hnd._open_at2($std_core_hnd._evv_index($std_core_exn.exn_fs__tag), $std_core_exn.$throw, "unexpected Nil in std/core/foldl1");
  }
}
 
export function foldr1(xs, f) /* forall<a,e> (xs : list<a>, f : (a, a) -> <exn|e> a) -> <exn|e> a */  {
   
  var xs_0_10010 = reverse_acc($std_core_types.Nil, xs);
  if (xs_0_10010 !== null) {
    return foldl(xs_0_10010.tail, xs_0_10010.head, f);
  }
  else {
    return $std_core_hnd._open_at2($std_core_hnd._evv_index($std_core_exn.exn_fs__tag), $std_core_exn.$throw, "unexpected Nil in std/core/foldl1");
  }
}
 
 
// split a list at position `n`
export function split(xs, n) /* forall<a> (xs : list<a>, n : int) -> (list<a>, list<a>) */  {
  return $std_core_types.Tuple2(_trmc_take(xs, n, $std_core_types._cctx_empty()), drop(xs, n));
}
 
 
// monadic lift
export function _mlift_drop_while_10413(predicate, xs, xx, _y_x10161) /* forall<a,e> (predicate : (a) -> e bool, xs : list<a>, xx : list<a>, bool) -> e list<a> */  {
  if (_y_x10161) {
    return drop_while(xx, predicate);
  }
  else {
    return xs;
  }
}
 
 
// Drop all initial elements that satisfy `predicate`
export function drop_while(xs_0, predicate_0) /* forall<a,e> (xs : list<a>, predicate : (a) -> e bool) -> e list<a> */  { tailcall: while(1)
{
  if (xs_0 !== null) {
     
    var x_0_10471 = predicate_0(xs_0.head);
    if ($std_core_hnd._yielding()) {
      return $std_core_hnd.yield_extend(function(_y_x10161_0 /* bool */ ) {
        return _mlift_drop_while_10413(predicate_0, xs_0, xs_0.tail, _y_x10161_0);
      });
    }
    else {
      if (x_0_10471) {
        {
          // tail call
          xs_0 = xs_0.tail;
          continue tailcall;
        }
      }
      else {
        return xs_0;
      }
    }
  }
  else {
    return $std_core_types.Nil;
  }
}}
 
 
// monadic lift
export function _mlift_trmc_filter_10414(_acc, pred, x, xx, _y_x10165) /* forall<a,e> (ctx<list<a>>, pred : (a) -> e bool, x : a, xx : list<a>, bool) -> e list<a> */  {
  if (_y_x10165) {
     
    var _trmc_x10052 = undefined;
     
    var _trmc_x10053 = $std_core_types.Cons(x, _trmc_x10052);
    return _trmc_filter(xx, pred, $std_core_types._cctx_extend(_acc,_trmc_x10053,({obj: _trmc_x10053, field_name: "tail"})));
  }
  else {
    return _trmc_filter(xx, pred, _acc);
  }
}
 
 
// monadic lift
export function _mlift_trmcm_filter_10415(_accm, pred_0, x_0, xx_0, _y_x10170) /* forall<a,e> ((list<a>) -> list<a>, pred : (a) -> e bool, x : a, xx : list<a>, bool) -> e list<a> */  {
  if (_y_x10170) {
    return _trmcm_filter(xx_0, pred_0, function(_trmc_x10055 /* list<1504> */ ) {
        return _accm($std_core_types.Cons(x_0, _trmc_x10055));
      });
  }
  else {
    return _trmcm_filter(xx_0, pred_0, _accm);
  }
}
 
 
// Retain only those elements of a list that satisfy the given predicate `pred`.
// For example: `filter([1,2,3],odd?) == [1,3]`
export function _trmc_filter(xs, pred_1, _acc_0) /* forall<a,e> (xs : list<a>, pred : (a) -> e bool, ctx<list<a>>) -> e list<a> */  { tailcall: while(1)
{
  if (xs !== null) {
     
    var x_2_10474 = pred_1(xs.head);
    if ($std_core_hnd._yielding()) {
      return $std_core_hnd.yield_extend(function(_y_x10165_0 /* bool */ ) {
        return _mlift_trmc_filter_10414(_acc_0, pred_1, xs.head, xs.tail, _y_x10165_0);
      });
    }
    else {
      if (x_2_10474) {
         
        var _trmc_x10052_0 = undefined;
         
        var _trmc_x10053_0 = $std_core_types.Cons(xs.head, _trmc_x10052_0);
        {
          // tail call
          var _x6 = $std_core_types._cctx_extend(_acc_0,_trmc_x10053_0,({obj: _trmc_x10053_0, field_name: "tail"}));
          xs = xs.tail;
          _acc_0 = _x6;
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
  }
  else {
    return $std_core_types._cctx_apply(_acc_0,($std_core_types.Nil));
  }
}}
 
 
// Retain only those elements of a list that satisfy the given predicate `pred`.
// For example: `filter([1,2,3],odd?) == [1,3]`
export function _trmcm_filter(xs_0, pred_2, _accm_0) /* forall<a,e> (xs : list<a>, pred : (a) -> e bool, (list<a>) -> list<a>) -> e list<a> */  { tailcall: while(1)
{
  if (xs_0 !== null) {
     
    var x_4_10477 = pred_2(xs_0.head);
    if ($std_core_hnd._yielding()) {
      return $std_core_hnd.yield_extend(function(_y_x10170_0 /* bool */ ) {
        return _mlift_trmcm_filter_10415(_accm_0, pred_2, xs_0.head, xs_0.tail, _y_x10170_0);
      });
    }
    else {
      if (x_4_10477) {
        {
          // tail call
          var _x9 = function(__at_accm_07 /* (list<1504>) -> list<1504> */ , _x_38 /* 1504 */ ) {
            return function(_trmc_x10055_0 /* list<1504> */ ) {
              return __at_accm_07($std_core_types.Cons(_x_38, _trmc_x10055_0));
            };
          }(_accm_0, xs_0.head);
          xs_0 = xs_0.tail;
          _accm_0 = _x9;
          continue tailcall;
        }
      }
      else {
        {
          // tail call
          xs_0 = xs_0.tail;
          continue tailcall;
        }
      }
    }
  }
  else {
    return _accm_0($std_core_types.Nil);
  }
}}
 
 
// Retain only those elements of a list that satisfy the given predicate `pred`.
// For example: `filter([1,2,3],odd?) == [1,3]`
export function filter(xs_1, pred_3) /* forall<a,e> (xs : list<a>, pred : (a) -> e bool) -> e list<a> */  {
  var _x10 = $std_core_hnd._evv_is_affine();
  if (_x10) {
    return _trmc_filter(xs_1, pred_3, $std_core_types._cctx_empty());
  }
  else {
    return _trmcm_filter(xs_1, pred_3, function(_trmc_x10054 /* list<1504> */ ) {
        return _trmc_x10054;
      });
  }
}
 
 
// monadic lift
export function _mlift_trmc_filter_map_10416(_acc, pred, xx, _y_x10178) /* forall<a,b,e> (ctx<list<b>>, pred : (a) -> e maybe<b>, xx : list<a>, maybe<b>) -> e list<b> */  {
  if (_y_x10178 === null) {
    return _trmc_filter_map(xx, pred, _acc);
  }
  else {
     
    var _trmc_x10056 = undefined;
     
    var _trmc_x10057 = $std_core_types.Cons(_y_x10178.value, _trmc_x10056);
    return _trmc_filter_map(xx, pred, $std_core_types._cctx_extend(_acc,_trmc_x10057,({obj: _trmc_x10057, field_name: "tail"})));
  }
}
 
 
// monadic lift
export function _mlift_trmcm_filter_map_10417(_accm, pred_0, xx_0, _y_x10183) /* forall<a,b,e> ((list<b>) -> list<b>, pred : (a) -> e maybe<b>, xx : list<a>, maybe<b>) -> e list<b> */  {
  if (_y_x10183 === null) {
    return _trmcm_filter_map(xx_0, pred_0, _accm);
  }
  else {
    return _trmcm_filter_map(xx_0, pred_0, function(_trmc_x10059 /* list<1588> */ ) {
        return _accm($std_core_types.Cons(_y_x10183.value, _trmc_x10059));
      });
  }
}
 
 
// Retain only those elements of a list that satisfy the given predicate `pred`.
// For example: `filterMap([1,2,3],fn(i) { if i.odd? then Nothing else Just(i*i) }) == [4]`
export function _trmc_filter_map(xs, pred_1, _acc_0) /* forall<a,b,e> (xs : list<a>, pred : (a) -> e maybe<b>, ctx<list<b>>) -> e list<b> */  { tailcall: while(1)
{
  if (xs === null) {
    return $std_core_types._cctx_apply(_acc_0,($std_core_types.Nil));
  }
  else {
     
    var x_0_10480 = pred_1(xs.head);
    if ($std_core_hnd._yielding()) {
      return $std_core_hnd.yield_extend(function(_y_x10178_0 /* maybe<1588> */ ) {
        return _mlift_trmc_filter_map_10416(_acc_0, pred_1, xs.tail, _y_x10178_0);
      });
    }
    else {
      if (x_0_10480 === null) {
        {
          // tail call
          xs = xs.tail;
          continue tailcall;
        }
      }
      else {
         
        var _trmc_x10056_0 = undefined;
         
        var _trmc_x10057_0 = $std_core_types.Cons(x_0_10480.value, _trmc_x10056_0);
        {
          // tail call
          var _x11 = $std_core_types._cctx_extend(_acc_0,_trmc_x10057_0,({obj: _trmc_x10057_0, field_name: "tail"}));
          xs = xs.tail;
          _acc_0 = _x11;
          continue tailcall;
        }
      }
    }
  }
}}
 
 
// Retain only those elements of a list that satisfy the given predicate `pred`.
// For example: `filterMap([1,2,3],fn(i) { if i.odd? then Nothing else Just(i*i) }) == [4]`
export function _trmcm_filter_map(xs_0, pred_2, _accm_0) /* forall<a,b,e> (xs : list<a>, pred : (a) -> e maybe<b>, (list<b>) -> list<b>) -> e list<b> */  { tailcall: while(1)
{
  if (xs_0 === null) {
    return _accm_0($std_core_types.Nil);
  }
  else {
     
    var x_2_10483 = pred_2(xs_0.head);
    if ($std_core_hnd._yielding()) {
      return $std_core_hnd.yield_extend(function(_y_x10183_0 /* maybe<1588> */ ) {
        return _mlift_trmcm_filter_map_10417(_accm_0, pred_2, xs_0.tail, _y_x10183_0);
      });
    }
    else {
      if (x_2_10483 === null) {
        {
          // tail call
          xs_0 = xs_0.tail;
          continue tailcall;
        }
      }
      else {
        {
          // tail call
          var _x14 = function(__at_accm_012 /* (list<1588>) -> list<1588> */ , _y_213 /* 1588 */ ) {
            return function(_trmc_x10059_0 /* list<1588> */ ) {
              return __at_accm_012($std_core_types.Cons(_y_213, _trmc_x10059_0));
            };
          }(_accm_0, x_2_10483.value);
          xs_0 = xs_0.tail;
          _accm_0 = _x14;
          continue tailcall;
        }
      }
    }
  }
}}
 
 
// Retain only those elements of a list that satisfy the given predicate `pred`.
// For example: `filterMap([1,2,3],fn(i) { if i.odd? then Nothing else Just(i*i) }) == [4]`
export function filter_map(xs_1, pred_3) /* forall<a,b,e> (xs : list<a>, pred : (a) -> e maybe<b>) -> e list<b> */  {
  var _x15 = $std_core_hnd._evv_is_affine();
  if (_x15) {
    return _trmc_filter_map(xs_1, pred_3, $std_core_types._cctx_empty());
  }
  else {
    return _trmcm_filter_map(xs_1, pred_3, function(_trmc_x10058 /* list<1588> */ ) {
        return _trmc_x10058;
      });
  }
}
 
 
// monadic lift
export function _mlift_foreach_while_10418(action, xx, _y_x10191) /* forall<a,b,e> (action : (a) -> e maybe<b>, xx : list<a>, maybe<b>) -> e maybe<b> */  {
  if (_y_x10191 === null) {
    return foreach_while(xx, action);
  }
  else {
    return _y_x10191;
  }
}
 
 
// Invoke `action` for each element of a list while `action` return `Nothing`
export function foreach_while(xs, action_0) /* forall<a,b,e> (xs : list<a>, action : (a) -> e maybe<b>) -> e maybe<b> */  { tailcall: while(1)
{
  if (xs === null) {
    return $std_core_types.Nothing;
  }
  else {
     
    var x_0_10486 = action_0(xs.head);
    if ($std_core_hnd._yielding()) {
      return $std_core_hnd.yield_extend(function(_y_x10191_0 /* maybe<1662> */ ) {
        return _mlift_foreach_while_10418(action_0, xs.tail, _y_x10191_0);
      });
    }
    else {
      if (x_0_10486 === null) {
        {
          // tail call
          xs = xs.tail;
          continue tailcall;
        }
      }
      else {
        return x_0_10486;
      }
    }
  }
}}
 
 
// monadic lift
export function _mlift_find_10419(x, _y_x10195) /* forall<a,e> (x : a, bool) -> e maybe<a> */  {
  if (_y_x10195) {
    return $std_core_types.Just(x);
  }
  else {
    return $std_core_types.Nothing;
  }
}
 
 
// Find the first element satisfying some predicate
export function find(xs, pred) /* forall<a,e> (xs : list<a>, pred : (a) -> e bool) -> e maybe<a> */  {
  return foreach_while(xs, function(x /* 1730 */ ) {
       
      var x_0_10489 = pred(x);
       
      function next_10490(_y_x10195) /* (bool) -> 1731 maybe<1730> */  {
        if (_y_x10195) {
          return $std_core_types.Just(x);
        }
        else {
          return $std_core_types.Nothing;
        }
      }
      if ($std_core_hnd._yielding()) {
        return $std_core_hnd.yield_extend(next_10490);
      }
      else {
        return next_10490(x_0_10489);
      }
    });
}
 
 
// Find the first element satisfying some predicate and return it.
export function find_maybe(xs, pred) /* forall<a,b,e> (xs : list<a>, pred : (a) -> e maybe<b>) -> e maybe<b> */  {
  return foreach_while(xs, pred);
}
 
 
// Convert a `:maybe` type to a list type.
export function maybe_fs_list(m) /* forall<a> (m : maybe<a>) -> list<a> */  {
  if (m === null) {
    return $std_core_types.Nil;
  }
  else {
    return $std_core_types.Cons(m.value, $std_core_types.Nil);
  }
}
 
 
// Returns an integer list of increasing elements from `lo`  to `hi`
// (including both `lo`  and `hi` ).
// If `lo > hi`  the function returns the empty list.
export function range_fs__trmc_list(lo, hi, _acc) /* (lo : int, hi : int, ctx<list<int>>) -> list<int> */  { tailcall: while(1)
{
  if ($std_core_types._int_le(lo,hi)) {
     
    var _trmc_x10060 = undefined;
     
    var _trmc_x10061 = $std_core_types.Cons(lo, _trmc_x10060);
    {
      // tail call
      var _x16 = $std_core_types._int_add(lo,1);
      var _x17 = $std_core_types._cctx_extend(_acc,_trmc_x10061,({obj: _trmc_x10061, field_name: "tail"}));
      lo = _x16;
      _acc = _x17;
      continue tailcall;
    }
  }
  else {
    return $std_core_types._cctx_apply(_acc,($std_core_types.Nil));
  }
}}
 
 
// Returns an integer list of increasing elements from `lo`  to `hi`
// (including both `lo`  and `hi` ).
// If `lo > hi`  the function returns the empty list.
export function range_fs_list(lo_0, hi_0) /* (lo : int, hi : int) -> list<int> */  {
  return range_fs__trmc_list(lo_0, hi_0, $std_core_types._cctx_empty());
}
 
 
// monadic lift
export function _mlift_trmc_map_10420(_acc, f, xx, _trmc_x10062) /* forall<a,b,e> (ctx<list<b>>, f : (a) -> e b, xx : list<a>, b) -> e list<b> */  {
   
  var _trmc_x10063 = undefined;
   
  var _trmc_x10064 = $std_core_types.Cons(_trmc_x10062, _trmc_x10063);
  return _trmc_map(xx, f, $std_core_types._cctx_extend(_acc,_trmc_x10064,({obj: _trmc_x10064, field_name: "tail"})));
}
 
 
// monadic lift
export function _mlift_trmcm_map_10421(_accm, f_0, xx_0, _trmc_x10067) /* forall<a,b,e> ((list<b>) -> list<b>, f : (a) -> e b, xx : list<a>, b) -> e list<b> */  {
  return _trmcm_map(xx_0, f_0, function(_trmc_x10066 /* list<2229> */ ) {
      return _accm($std_core_types.Cons(_trmc_x10067, _trmc_x10066));
    });
}
 
 
// Apply a function `f` to each element of the input list in sequence.
export function _trmc_map(xs, f_1, _acc_0) /* forall<a,b,e> (xs : list<a>, f : (a) -> e b, ctx<list<b>>) -> e list<b> */  { tailcall: while(1)
{
  if (xs !== null) {
     
    var x_0_10493 = f_1(xs.head);
    if ($std_core_hnd._yielding()) {
      return $std_core_hnd.yield_extend(function(_trmc_x10062_0 /* 2229 */ ) {
        return _mlift_trmc_map_10420(_acc_0, f_1, xs.tail, _trmc_x10062_0);
      });
    }
    else {
       
      var _trmc_x10063_0 = undefined;
       
      var _trmc_x10064_0 = $std_core_types.Cons(x_0_10493, _trmc_x10063_0);
      {
        // tail call
        var _x18 = $std_core_types._cctx_extend(_acc_0,_trmc_x10064_0,({obj: _trmc_x10064_0, field_name: "tail"}));
        xs = xs.tail;
        _acc_0 = _x18;
        continue tailcall;
      }
    }
  }
  else {
    return $std_core_types._cctx_apply(_acc_0,($std_core_types.Nil));
  }
}}
 
 
// Apply a function `f` to each element of the input list in sequence.
export function _trmcm_map(xs_0, f_2, _accm_0) /* forall<a,b,e> (xs : list<a>, f : (a) -> e b, (list<b>) -> list<b>) -> e list<b> */  { tailcall: while(1)
{
  if (xs_0 !== null) {
     
    var x_2_10496 = f_2(xs_0.head);
    if ($std_core_hnd._yielding()) {
      return $std_core_hnd.yield_extend(function(_trmc_x10067_0 /* 2229 */ ) {
        return _mlift_trmcm_map_10421(_accm_0, f_2, xs_0.tail, _trmc_x10067_0);
      });
    }
    else {
      {
        // tail call
        var _x21 = function(__at_accm_019 /* (list<2229>) -> list<2229> */ , _x_2_1049620 /* 2229 */ ) {
          return function(_trmc_x10066_0 /* list<2229> */ ) {
            return __at_accm_019($std_core_types.Cons(_x_2_1049620, _trmc_x10066_0));
          };
        }(_accm_0, x_2_10496);
        xs_0 = xs_0.tail;
        _accm_0 = _x21;
        continue tailcall;
      }
    }
  }
  else {
    return _accm_0($std_core_types.Nil);
  }
}}
 
 
// Apply a function `f` to each element of the input list in sequence.
export function map(xs_1, f_3) /* forall<a,b,e> (xs : list<a>, f : (a) -> e b) -> e list<b> */  {
  var _x22 = $std_core_hnd._evv_is_affine();
  if (_x22) {
    return _trmc_map(xs_1, f_3, $std_core_types._cctx_empty());
  }
  else {
    return _trmcm_map(xs_1, f_3, function(_trmc_x10065 /* list<2229> */ ) {
        return _trmc_x10065;
      });
  }
}
 
 
// Create a list of characters from `lo`  to `hi`  (including `hi`).
export function char_fs_list(lo, hi) /* (lo : char, hi : char) -> list<char> */  {
   
  var lo_0_10501 = lo;
   
  var hi_0_10502 = hi;
   
  var xs_10499 = range_fs__trmc_list(lo_0_10501, hi_0_10502, $std_core_types._cctx_empty());
  var _x23 = $std_core_hnd._evv_is_affine();
  if (_x23) {
    return _trmc_map(xs_10499, (function(_x24) {
        return (_x24);
      }), $std_core_types._cctx_empty());
  }
  else {
    return _trmcm_map(xs_10499, (function(_x25) {
        return (_x25);
      }), function(_trmc_x10065 /* list<char> */ ) {
        return _trmc_x10065;
      });
  }
}
 
 
// monadic lift
export function function_fs__mlift_trmc_list_10422(_acc, f, hi, lo, _trmc_x10068) /* forall<a,e> (ctx<list<a>>, f : (int) -> e a, hi : int, lo : int, a) -> e list<a> */  {
   
  var _trmc_x10069 = undefined;
   
  var _trmc_x10070 = $std_core_types.Cons(_trmc_x10068, _trmc_x10069);
  return function_fs__trmc_list($std_core_types._int_add(lo,1), hi, f, $std_core_types._cctx_extend(_acc,_trmc_x10070,({obj: _trmc_x10070, field_name: "tail"})));
}
 
 
// monadic lift
export function function_fs__mlift_trmcm_list_10423(_accm, f_0, hi_0, lo_0, _trmc_x10073) /* forall<a,e> ((list<a>) -> list<a>, f : (int) -> e a, hi : int, lo : int, a) -> e list<a> */  {
  return function_fs__trmcm_list($std_core_types._int_add(lo_0,1), hi_0, f_0, function(_trmc_x10072 /* list<1938> */ ) {
      return _accm($std_core_types.Cons(_trmc_x10073, _trmc_x10072));
    });
}
 
 
// Applies a function `f` to list of increasing elements from `lo`  to `hi`
// (including both `lo`  and `hi` ).
// If `lo > hi`  the function returns the empty list.
export function function_fs__trmc_list(lo_1, hi_1, f_1, _acc_0) /* forall<a,e> (lo : int, hi : int, f : (int) -> e a, ctx<list<a>>) -> e list<a> */  { tailcall: while(1)
{
  if ($std_core_types._int_le(lo_1,hi_1)) {
     
    var x_10503 = f_1(lo_1);
    if ($std_core_hnd._yielding()) {
      return $std_core_hnd.yield_extend(function(_trmc_x10068_0 /* 1938 */ ) {
        return function_fs__mlift_trmc_list_10422(_acc_0, f_1, hi_1, lo_1, _trmc_x10068_0);
      });
    }
    else {
       
      var _trmc_x10069_0 = undefined;
       
      var _trmc_x10070_0 = $std_core_types.Cons(x_10503, _trmc_x10069_0);
      {
        // tail call
        var _x26 = $std_core_types._int_add(lo_1,1);
        var _x27 = $std_core_types._cctx_extend(_acc_0,_trmc_x10070_0,({obj: _trmc_x10070_0, field_name: "tail"}));
        lo_1 = _x26;
        _acc_0 = _x27;
        continue tailcall;
      }
    }
  }
  else {
    return $std_core_types._cctx_apply(_acc_0,($std_core_types.Nil));
  }
}}
 
 
// Applies a function `f` to list of increasing elements from `lo`  to `hi`
// (including both `lo`  and `hi` ).
// If `lo > hi`  the function returns the empty list.
export function function_fs__trmcm_list(lo_2, hi_2, f_2, _accm_0) /* forall<a,e> (lo : int, hi : int, f : (int) -> e a, (list<a>) -> list<a>) -> e list<a> */  { tailcall: while(1)
{
  if ($std_core_types._int_le(lo_2,hi_2)) {
     
    var x_0_10506 = f_2(lo_2);
    if ($std_core_hnd._yielding()) {
      return $std_core_hnd.yield_extend(function(_trmc_x10073_0 /* 1938 */ ) {
        return function_fs__mlift_trmcm_list_10423(_accm_0, f_2, hi_2, lo_2, _trmc_x10073_0);
      });
    }
    else {
      {
        // tail call
        var _x30 = $std_core_types._int_add(lo_2,1);
        var _x31 = function(__at_accm_028 /* (list<1938>) -> list<1938> */ , _x_0_1050629 /* 1938 */ ) {
          return function(_trmc_x10072_0 /* list<1938> */ ) {
            return __at_accm_028($std_core_types.Cons(_x_0_1050629, _trmc_x10072_0));
          };
        }(_accm_0, x_0_10506);
        lo_2 = _x30;
        _accm_0 = _x31;
        continue tailcall;
      }
    }
  }
  else {
    return _accm_0($std_core_types.Nil);
  }
}}
 
 
// Applies a function `f` to list of increasing elements from `lo`  to `hi`
// (including both `lo`  and `hi` ).
// If `lo > hi`  the function returns the empty list.
export function function_fs_list(lo_3, hi_3, f_3) /* forall<a,e> (lo : int, hi : int, f : (int) -> e a) -> e list<a> */  {
  var _x32 = $std_core_hnd._evv_is_affine();
  if (_x32) {
    return function_fs__trmc_list(lo_3, hi_3, f_3, $std_core_types._cctx_empty());
  }
  else {
    return function_fs__trmcm_list(lo_3, hi_3, f_3, function(_trmc_x10071 /* list<1938> */ ) {
        return _trmc_x10071;
      });
  }
}
 
 
// Returns an integer list of increasing elements from `lo`  to `hi` with stride `stride`.
// If `lo > hi`  the function returns the empty list.
export function stride_fs__trmc_list(lo, hi, stride, _acc) /* (lo : int, hi : int, stride : int, ctx<list<int>>) -> list<int> */  { tailcall: while(1)
{
  if ($std_core_types._int_le(lo,hi)) {
     
    var _trmc_x10074 = undefined;
     
    var _trmc_x10075 = $std_core_types.Cons(lo, _trmc_x10074);
    {
      // tail call
      var _x33 = $std_core_types._int_add(lo,stride);
      var _x34 = $std_core_types._cctx_extend(_acc,_trmc_x10075,({obj: _trmc_x10075, field_name: "tail"}));
      lo = _x33;
      _acc = _x34;
      continue tailcall;
    }
  }
  else {
    return $std_core_types._cctx_apply(_acc,($std_core_types.Nil));
  }
}}
 
 
// Returns an integer list of increasing elements from `lo`  to `hi` with stride `stride`.
// If `lo > hi`  the function returns the empty list.
export function stride_fs_list(lo_0, hi_0, stride_0) /* (lo : int, hi : int, stride : int) -> list<int> */  {
  return stride_fs__trmc_list(lo_0, hi_0, stride_0, $std_core_types._cctx_empty());
}
 
 
// monadic lift
export function stridefunction_fs__mlift_trmc_list_10424(_acc, f, hi, lo, stride, _trmc_x10076) /* forall<a,e> (ctx<list<a>>, f : (int) -> e a, hi : int, lo : int, stride : int, a) -> e list<a> */  {
   
  var _trmc_x10077 = undefined;
   
  var _trmc_x10078 = $std_core_types.Cons(_trmc_x10076, _trmc_x10077);
  return stridefunction_fs__trmc_list($std_core_types._int_add(lo,stride), hi, stride, f, $std_core_types._cctx_extend(_acc,_trmc_x10078,({obj: _trmc_x10078, field_name: "tail"})));
}
 
 
// monadic lift
export function stridefunction_fs__mlift_trmcm_list_10425(_accm, f_0, hi_0, lo_0, stride_0, _trmc_x10081) /* forall<a,e> ((list<a>) -> list<a>, f : (int) -> e a, hi : int, lo : int, stride : int, a) -> e list<a> */  {
  return stridefunction_fs__trmcm_list($std_core_types._int_add(lo_0,stride_0), hi_0, stride_0, f_0, function(_trmc_x10080 /* list<2118> */ ) {
      return _accm($std_core_types.Cons(_trmc_x10081, _trmc_x10080));
    });
}
 
 
// Returns an integer list of increasing elements from `lo`  to `hi` with stride `stride`.
// If `lo > hi`  the function returns the empty list.
export function stridefunction_fs__trmc_list(lo_1, hi_1, stride_1, f_1, _acc_0) /* forall<a,e> (lo : int, hi : int, stride : int, f : (int) -> e a, ctx<list<a>>) -> e list<a> */  { tailcall: while(1)
{
  if ($std_core_types._int_le(lo_1,hi_1)) {
     
    var x_10509 = f_1(lo_1);
    if ($std_core_hnd._yielding()) {
      return $std_core_hnd.yield_extend(function(_trmc_x10076_0 /* 2118 */ ) {
        return stridefunction_fs__mlift_trmc_list_10424(_acc_0, f_1, hi_1, lo_1, stride_1, _trmc_x10076_0);
      });
    }
    else {
       
      var _trmc_x10077_0 = undefined;
       
      var _trmc_x10078_0 = $std_core_types.Cons(x_10509, _trmc_x10077_0);
      {
        // tail call
        var _x35 = $std_core_types._int_add(lo_1,stride_1);
        var _x36 = $std_core_types._cctx_extend(_acc_0,_trmc_x10078_0,({obj: _trmc_x10078_0, field_name: "tail"}));
        lo_1 = _x35;
        _acc_0 = _x36;
        continue tailcall;
      }
    }
  }
  else {
    return $std_core_types._cctx_apply(_acc_0,($std_core_types.Nil));
  }
}}
 
 
// Returns an integer list of increasing elements from `lo`  to `hi` with stride `stride`.
// If `lo > hi`  the function returns the empty list.
export function stridefunction_fs__trmcm_list(lo_2, hi_2, stride_2, f_2, _accm_0) /* forall<a,e> (lo : int, hi : int, stride : int, f : (int) -> e a, (list<a>) -> list<a>) -> e list<a> */  { tailcall: while(1)
{
  if ($std_core_types._int_le(lo_2,hi_2)) {
     
    var x_0_10512 = f_2(lo_2);
    if ($std_core_hnd._yielding()) {
      return $std_core_hnd.yield_extend(function(_trmc_x10081_0 /* 2118 */ ) {
        return stridefunction_fs__mlift_trmcm_list_10425(_accm_0, f_2, hi_2, lo_2, stride_2, _trmc_x10081_0);
      });
    }
    else {
      {
        // tail call
        var _x39 = $std_core_types._int_add(lo_2,stride_2);
        var _x40 = function(__at_accm_037 /* (list<2118>) -> list<2118> */ , _x_0_1051238 /* 2118 */ ) {
          return function(_trmc_x10080_0 /* list<2118> */ ) {
            return __at_accm_037($std_core_types.Cons(_x_0_1051238, _trmc_x10080_0));
          };
        }(_accm_0, x_0_10512);
        lo_2 = _x39;
        _accm_0 = _x40;
        continue tailcall;
      }
    }
  }
  else {
    return _accm_0($std_core_types.Nil);
  }
}}
 
 
// Returns an integer list of increasing elements from `lo`  to `hi` with stride `stride`.
// If `lo > hi`  the function returns the empty list.
export function stridefunction_fs_list(lo_3, hi_3, stride_3, f_3) /* forall<a,e> (lo : int, hi : int, stride : int, f : (int) -> e a) -> e list<a> */  {
  var _x41 = $std_core_hnd._evv_is_affine();
  if (_x41) {
    return stridefunction_fs__trmc_list(lo_3, hi_3, stride_3, f_3, $std_core_types._cctx_empty());
  }
  else {
    return stridefunction_fs__trmcm_list(lo_3, hi_3, stride_3, f_3, function(_trmc_x10079 /* list<2118> */ ) {
        return _trmc_x10079;
      });
  }
}
 
 
// Apply a function `f` to each character in a string
export function string_fs_map(s, f) /* forall<e> (s : string, f : (char) -> e char) -> e string */  {
   
  var xs_10517 = $std_core_string.list(s);
   
  var _x42 = $std_core_hnd._evv_is_affine();
  if (_x42) {
    var x_10515 = _trmc_map(xs_10517, f, $std_core_types._cctx_empty());
  }
  else {
    var x_10515 = _trmcm_map(xs_10517, f, function(_trmc_x10065 /* list<char> */ ) {
        return _trmc_x10065;
      });
  }
  if ($std_core_hnd._yielding()) {
    return $std_core_hnd.yield_extend($std_core_string.listchar_fs_string);
  }
  else {
    return $std_core_string.listchar_fs_string(x_10515);
  }
}
 
 
// monadic lift
export function _mlift_order2_10426(z, _y_x10227) /* forall<a,e> (z : a, order2<list<a>>) -> e order2<list<a>> */  {
  if (_y_x10227._tag === 2) {
    return $std_core_types.Eq2($std_core_types.Cons(z, _y_x10227.eq));
  }
  else if (_y_x10227._tag === 1) {
    return $std_core_types.Lt2($std_core_types.Cons(z, _y_x10227.lt), $std_core_types.Cons(z, _y_x10227.gt));
  }
  else {
    return $std_core_types.Gt2($std_core_types.Cons(z, _y_x10227.lt), $std_core_types.Cons(z, _y_x10227.gt));
  }
}
 
 
// monadic lift
export function _mlift_order2_10427(_implicit_fs_order2, xx, yy, _y_x10226) /* forall<a,e> (?order2 : (a, a) -> e order2<a>, xx : list<a>, yy : list<a>, order2<a>) -> e order2<list<a>> */  {
  if (_y_x10226._tag === 2) {
     
    var x_10519 = order2(xx, yy, _implicit_fs_order2);
    if ($std_core_hnd._yielding()) {
      return $std_core_hnd.yield_extend(function(_y_x10227_0 /* order2<list<2513>> */ ) {
        return _mlift_order2_10426(_y_x10226.eq, _y_x10227_0);
      });
    }
    else {
      return _mlift_order2_10426(_y_x10226.eq, x_10519);
    }
  }
  else if (_y_x10226._tag === 1) {
    return $std_core_types.Lt2($std_core_types.Cons(_y_x10226.lt, xx), $std_core_types.Cons(_y_x10226.gt, yy));
  }
  else {
    return $std_core_types.Gt2($std_core_types.Cons(_y_x10226.lt, yy), $std_core_types.Cons(_y_x10226.gt, xx));
  }
}
 
 
// Order2 on lists
export function order2(xs, ys, _implicit_fs_order2_0) /* forall<a,e> (xs : list<a>, ys : list<a>, ?order2 : (a, a) -> e order2<a>) -> e order2<list<a>> */  {
  if (xs === null) {
    if (ys === null) {
      return $std_core_types.Eq2($std_core_types.Nil);
    }
    else {
      return $std_core_types.Lt2($std_core_types.Nil, ys);
    }
  }
  else {
    if (ys === null) {
      return $std_core_types.Gt2($std_core_types.Nil, $std_core_types.Cons(xs.head, xs.tail));
    }
    else {
       
      var x_1_10521 = _implicit_fs_order2_0(xs.head, ys.head);
      if ($std_core_hnd._yielding()) {
        return $std_core_hnd.yield_extend(function(_y_x10226_0 /* order2<2513> */ ) {
          return _mlift_order2_10427(_implicit_fs_order2_0, xs.tail, ys.tail, _y_x10226_0);
        });
      }
      else {
        if (x_1_10521._tag === 2) {
           
          var x_2_10524 = order2(xs.tail, ys.tail, _implicit_fs_order2_0);
          if ($std_core_hnd._yielding()) {
            return $std_core_hnd.yield_extend(function(_y_x10227_1 /* order2<list<2513>> */ ) {
              return _mlift_order2_10426(x_1_10521.eq, _y_x10227_1);
            });
          }
          else {
            if (x_2_10524._tag === 2) {
              return $std_core_types.Eq2($std_core_types.Cons(x_1_10521.eq, x_2_10524.eq));
            }
            else if (x_2_10524._tag === 1) {
              return $std_core_types.Lt2($std_core_types.Cons(x_1_10521.eq, x_2_10524.lt), $std_core_types.Cons(x_1_10521.eq, x_2_10524.gt));
            }
            else {
              return $std_core_types.Gt2($std_core_types.Cons(x_1_10521.eq, x_2_10524.lt), $std_core_types.Cons(x_1_10521.eq, x_2_10524.gt));
            }
          }
        }
        else if (x_1_10521._tag === 1) {
          return $std_core_types.Lt2($std_core_types.Cons(x_1_10521.lt, xs.tail), $std_core_types.Cons(x_1_10521.gt, ys.tail));
        }
        else {
          return $std_core_types.Gt2($std_core_types.Cons(x_1_10521.lt, ys.tail), $std_core_types.Cons(x_1_10521.gt, xs.tail));
        }
      }
    }
  }
}
 
 
// monadic lift
export function _mlift_trmc_lift_map_indexed_6004_10428(_acc, f, i_0_10018, yy, _trmc_x10082) /* forall<a,b,e> (ctx<list<b>>, f : (idx : int, value : a) -> e b, i@0@10018 : int, yy : list<a>, b) -> e list<b> */  {
   
  var _trmc_x10083 = undefined;
   
  var _trmc_x10084 = $std_core_types.Cons(_trmc_x10082, _trmc_x10083);
  return _trmc_lift_map_indexed_6004(f, yy, i_0_10018, $std_core_types._cctx_extend(_acc,_trmc_x10084,({obj: _trmc_x10084, field_name: "tail"})));
}
 
 
// monadic lift
export function _mlift_trmcm_lift_map_indexed_6004_10429(_accm, f_0, i_0_10018_0, yy_0, _trmc_x10087) /* forall<a,b,e> ((list<b>) -> list<b>, f : (idx : int, value : a) -> e b, i@0@10018 : int, yy : list<a>, b) -> e list<b> */  {
  return _trmcm_lift_map_indexed_6004(f_0, yy_0, i_0_10018_0, function(_trmc_x10086 /* list<2589> */ ) {
      return _accm($std_core_types.Cons(_trmc_x10087, _trmc_x10086));
    });
}
 
 
// lifted local: map-indexed, map-idx
export function _trmc_lift_map_indexed_6004(f_1, ys, i, _acc_0) /* forall<a,b,e> (f : (idx : int, value : a) -> e b, ys : list<a>, i : int, ctx<list<b>>) -> e list<b> */  { tailcall: while(1)
{
  if (ys !== null) {
     
    var i_0_10018_1 = $std_core_types._int_add(i,1);
     
    var x_10527 = f_1(i, ys.head);
    if ($std_core_hnd._yielding()) {
      return $std_core_hnd.yield_extend(function(_trmc_x10082_0 /* 2589 */ ) {
        return _mlift_trmc_lift_map_indexed_6004_10428(_acc_0, f_1, i_0_10018_1, ys.tail, _trmc_x10082_0);
      });
    }
    else {
       
      var _trmc_x10083_0 = undefined;
       
      var _trmc_x10084_0 = $std_core_types.Cons(x_10527, _trmc_x10083_0);
      {
        // tail call
        var _x42 = $std_core_types._cctx_extend(_acc_0,_trmc_x10084_0,({obj: _trmc_x10084_0, field_name: "tail"}));
        ys = ys.tail;
        i = i_0_10018_1;
        _acc_0 = _x42;
        continue tailcall;
      }
    }
  }
  else {
    return $std_core_types._cctx_apply(_acc_0,($std_core_types.Nil));
  }
}}
 
 
// lifted local: map-indexed, map-idx
export function _trmcm_lift_map_indexed_6004(f_2, ys_0, i_0, _accm_0) /* forall<a,b,e> (f : (idx : int, value : a) -> e b, ys : list<a>, i : int, (list<b>) -> list<b>) -> e list<b> */  { tailcall: while(1)
{
  if (ys_0 !== null) {
     
    var i_0_10018_2 = $std_core_types._int_add(i_0,1);
     
    var x_0_10530 = f_2(i_0, ys_0.head);
    if ($std_core_hnd._yielding()) {
      return $std_core_hnd.yield_extend(function(_trmc_x10087_0 /* 2589 */ ) {
        return _mlift_trmcm_lift_map_indexed_6004_10429(_accm_0, f_2, i_0_10018_2, ys_0.tail, _trmc_x10087_0);
      });
    }
    else {
      {
        // tail call
        var _x45 = function(__at_accm_043 /* (list<2589>) -> list<2589> */ , _x_0_1053044 /* 2589 */ ) {
          return function(_trmc_x10086_0 /* list<2589> */ ) {
            return __at_accm_043($std_core_types.Cons(_x_0_1053044, _trmc_x10086_0));
          };
        }(_accm_0, x_0_10530);
        ys_0 = ys_0.tail;
        i_0 = i_0_10018_2;
        _accm_0 = _x45;
        continue tailcall;
      }
    }
  }
  else {
    return _accm_0($std_core_types.Nil);
  }
}}
 
 
// lifted local: map-indexed, map-idx
export function _lift_map_indexed_6004(f_3, ys_1, i_1) /* forall<a,b,e> (f : (idx : int, value : a) -> e b, ys : list<a>, i : int) -> e list<b> */  {
  var _x46 = $std_core_hnd._evv_is_affine();
  if (_x46) {
    return _trmc_lift_map_indexed_6004(f_3, ys_1, i_1, $std_core_types._cctx_empty());
  }
  else {
    return _trmcm_lift_map_indexed_6004(f_3, ys_1, i_1, function(_trmc_x10085 /* list<2589> */ ) {
        return _trmc_x10085;
      });
  }
}
 
 
// Apply a function `f` to each element of the input list in sequence where takes
// both the index of the current element and the element itself as arguments.
export function map_indexed(xs, f) /* forall<a,b,e> (xs : list<a>, f : (idx : int, value : a) -> e b) -> e list<b> */  {
  var _x47 = $std_core_hnd._evv_is_affine();
  if (_x47) {
    return _trmc_lift_map_indexed_6004(f, xs, 0, $std_core_types._cctx_empty());
  }
  else {
    return _trmcm_lift_map_indexed_6004(f, xs, 0, function(_trmc_x10085 /* list<2589> */ ) {
        return _trmc_x10085;
      });
  }
}
 
 
// monadic lift
export function _mlift_trmc_lift_map_peek_6005_10430(_acc, f, yy, _trmc_x10088) /* forall<a,b,e> (ctx<list<b>>, f : (value : a, rest : list<a>) -> e b, yy : list<a>, b) -> e list<b> */  {
   
  var _trmc_x10089 = undefined;
   
  var _trmc_x10090 = $std_core_types.Cons(_trmc_x10088, _trmc_x10089);
  return _trmc_lift_map_peek_6005(f, yy, $std_core_types._cctx_extend(_acc,_trmc_x10090,({obj: _trmc_x10090, field_name: "tail"})));
}
 
 
// monadic lift
export function _mlift_trmcm_lift_map_peek_6005_10431(_accm, f_0, yy_0, _trmc_x10093) /* forall<a,b,e> ((list<b>) -> list<b>, f : (value : a, rest : list<a>) -> e b, yy : list<a>, b) -> e list<b> */  {
  return _trmcm_lift_map_peek_6005(f_0, yy_0, function(_trmc_x10092 /* list<2651> */ ) {
      return _accm($std_core_types.Cons(_trmc_x10093, _trmc_x10092));
    });
}
 
 
// lifted local: map-peek, mappeek
export function _trmc_lift_map_peek_6005(f_1, ys, _acc_0) /* forall<a,b,e> (f : (value : a, rest : list<a>) -> e b, ys : list<a>, ctx<list<b>>) -> e list<b> */  { tailcall: while(1)
{
  if (ys !== null) {
     
    var x_10536 = f_1(ys.head, ys.tail);
    if ($std_core_hnd._yielding()) {
      return $std_core_hnd.yield_extend(function(_trmc_x10088_0 /* 2651 */ ) {
        return _mlift_trmc_lift_map_peek_6005_10430(_acc_0, f_1, ys.tail, _trmc_x10088_0);
      });
    }
    else {
       
      var _trmc_x10089_0 = undefined;
       
      var _trmc_x10090_0 = $std_core_types.Cons(x_10536, _trmc_x10089_0);
      {
        // tail call
        var _x48 = $std_core_types._cctx_extend(_acc_0,_trmc_x10090_0,({obj: _trmc_x10090_0, field_name: "tail"}));
        ys = ys.tail;
        _acc_0 = _x48;
        continue tailcall;
      }
    }
  }
  else {
    return $std_core_types._cctx_apply(_acc_0,($std_core_types.Nil));
  }
}}
 
 
// lifted local: map-peek, mappeek
export function _trmcm_lift_map_peek_6005(f_2, ys_0, _accm_0) /* forall<a,b,e> (f : (value : a, rest : list<a>) -> e b, ys : list<a>, (list<b>) -> list<b>) -> e list<b> */  { tailcall: while(1)
{
  if (ys_0 !== null) {
     
    var x_0_10539 = f_2(ys_0.head, ys_0.tail);
    if ($std_core_hnd._yielding()) {
      return $std_core_hnd.yield_extend(function(_trmc_x10093_0 /* 2651 */ ) {
        return _mlift_trmcm_lift_map_peek_6005_10431(_accm_0, f_2, ys_0.tail, _trmc_x10093_0);
      });
    }
    else {
      {
        // tail call
        var _x51 = function(__at_accm_049 /* (list<2651>) -> list<2651> */ , _x_0_1053950 /* 2651 */ ) {
          return function(_trmc_x10092_0 /* list<2651> */ ) {
            return __at_accm_049($std_core_types.Cons(_x_0_1053950, _trmc_x10092_0));
          };
        }(_accm_0, x_0_10539);
        ys_0 = ys_0.tail;
        _accm_0 = _x51;
        continue tailcall;
      }
    }
  }
  else {
    return _accm_0($std_core_types.Nil);
  }
}}
 
 
// lifted local: map-peek, mappeek
export function _lift_map_peek_6005(f_3, ys_1) /* forall<a,b,e> (f : (value : a, rest : list<a>) -> e b, ys : list<a>) -> e list<b> */  {
  var _x52 = $std_core_hnd._evv_is_affine();
  if (_x52) {
    return _trmc_lift_map_peek_6005(f_3, ys_1, $std_core_types._cctx_empty());
  }
  else {
    return _trmcm_lift_map_peek_6005(f_3, ys_1, function(_trmc_x10091 /* list<2651> */ ) {
        return _trmc_x10091;
      });
  }
}
 
 
// Apply a function `f` to each element of the input list in sequence where `f` takes
// both the current element and the tail list as arguments.
export function map_peek(xs, f) /* forall<a,b,e> (xs : list<a>, f : (value : a, rest : list<a>) -> e b) -> e list<b> */  {
  return _lift_map_peek_6005(f, xs);
}
 
 
// monadic lift
export function _mlift_trmc_lift_map_indexed_peek_6006_10432(_acc, f, i_0_10021, yy, _trmc_x10094) /* forall<a,b,e> (ctx<list<b>>, f : (idx : int, value : a, rest : list<a>) -> e b, i@0@10021 : int, yy : list<a>, b) -> e list<b> */  {
   
  var _trmc_x10095 = undefined;
   
  var _trmc_x10096 = $std_core_types.Cons(_trmc_x10094, _trmc_x10095);
  return _trmc_lift_map_indexed_peek_6006(f, yy, i_0_10021, $std_core_types._cctx_extend(_acc,_trmc_x10096,({obj: _trmc_x10096, field_name: "tail"})));
}
 
 
// monadic lift
export function _mlift_trmcm_lift_map_indexed_peek_6006_10433(_accm, f_0, i_0_10021_0, yy_0, _trmc_x10099) /* forall<a,b,e> ((list<b>) -> list<b>, f : (idx : int, value : a, rest : list<a>) -> e b, i@0@10021 : int, yy : list<a>, b) -> e list<b> */  {
  return _trmcm_lift_map_indexed_peek_6006(f_0, yy_0, i_0_10021_0, function(_trmc_x10098 /* list<2722> */ ) {
      return _accm($std_core_types.Cons(_trmc_x10099, _trmc_x10098));
    });
}
 
 
// lifted local: map-indexed-peek, mapidx
export function _trmc_lift_map_indexed_peek_6006(f_1, ys, i, _acc_0) /* forall<a,b,e> (f : (idx : int, value : a, rest : list<a>) -> e b, ys : list<a>, i : int, ctx<list<b>>) -> e list<b> */  { tailcall: while(1)
{
  if (ys !== null) {
     
    var i_0_10021_1 = $std_core_types._int_add(i,1);
     
    var x_10542 = f_1(i, ys.head, ys.tail);
    if ($std_core_hnd._yielding()) {
      return $std_core_hnd.yield_extend(function(_trmc_x10094_0 /* 2722 */ ) {
        return _mlift_trmc_lift_map_indexed_peek_6006_10432(_acc_0, f_1, i_0_10021_1, ys.tail, _trmc_x10094_0);
      });
    }
    else {
       
      var _trmc_x10095_0 = undefined;
       
      var _trmc_x10096_0 = $std_core_types.Cons(x_10542, _trmc_x10095_0);
      {
        // tail call
        var _x53 = $std_core_types._cctx_extend(_acc_0,_trmc_x10096_0,({obj: _trmc_x10096_0, field_name: "tail"}));
        ys = ys.tail;
        i = i_0_10021_1;
        _acc_0 = _x53;
        continue tailcall;
      }
    }
  }
  else {
    return $std_core_types._cctx_apply(_acc_0,($std_core_types.Nil));
  }
}}
 
 
// lifted local: map-indexed-peek, mapidx
export function _trmcm_lift_map_indexed_peek_6006(f_2, ys_0, i_0, _accm_0) /* forall<a,b,e> (f : (idx : int, value : a, rest : list<a>) -> e b, ys : list<a>, i : int, (list<b>) -> list<b>) -> e list<b> */  { tailcall: while(1)
{
  if (ys_0 !== null) {
     
    var i_0_10021_2 = $std_core_types._int_add(i_0,1);
     
    var x_0_10545 = f_2(i_0, ys_0.head, ys_0.tail);
    if ($std_core_hnd._yielding()) {
      return $std_core_hnd.yield_extend(function(_trmc_x10099_0 /* 2722 */ ) {
        return _mlift_trmcm_lift_map_indexed_peek_6006_10433(_accm_0, f_2, i_0_10021_2, ys_0.tail, _trmc_x10099_0);
      });
    }
    else {
      {
        // tail call
        var _x56 = function(__at_accm_054 /* (list<2722>) -> list<2722> */ , _x_0_1054555 /* 2722 */ ) {
          return function(_trmc_x10098_0 /* list<2722> */ ) {
            return __at_accm_054($std_core_types.Cons(_x_0_1054555, _trmc_x10098_0));
          };
        }(_accm_0, x_0_10545);
        ys_0 = ys_0.tail;
        i_0 = i_0_10021_2;
        _accm_0 = _x56;
        continue tailcall;
      }
    }
  }
  else {
    return _accm_0($std_core_types.Nil);
  }
}}
 
 
// lifted local: map-indexed-peek, mapidx
export function _lift_map_indexed_peek_6006(f_3, ys_1, i_1) /* forall<a,b,e> (f : (idx : int, value : a, rest : list<a>) -> e b, ys : list<a>, i : int) -> e list<b> */  {
  var _x57 = $std_core_hnd._evv_is_affine();
  if (_x57) {
    return _trmc_lift_map_indexed_peek_6006(f_3, ys_1, i_1, $std_core_types._cctx_empty());
  }
  else {
    return _trmcm_lift_map_indexed_peek_6006(f_3, ys_1, i_1, function(_trmc_x10097 /* list<2722> */ ) {
        return _trmc_x10097;
      });
  }
}
 
 
// Apply a function `f` to each element of the input list in sequence where takes
// both the index of the current element, the element itself, and the tail list as arguments.
export function map_indexed_peek(xs, f) /* forall<a,b,e> (xs : list<a>, f : (idx : int, value : a, rest : list<a>) -> e b) -> e list<b> */  {
  var _x58 = $std_core_hnd._evv_is_affine();
  if (_x58) {
    return _trmc_lift_map_indexed_peek_6006(f, xs, 0, $std_core_types._cctx_empty());
  }
  else {
    return _trmcm_lift_map_indexed_peek_6006(f, xs, 0, function(_trmc_x10097 /* list<2722> */ ) {
        return _trmc_x10097;
      });
  }
}
 
 
// Create a list of `n` repeated elements `x`
export function _trmc_replicate(x, n, _acc) /* forall<a> (x : a, n : int, ctx<list<a>>) -> list<a> */  { tailcall: while(1)
{
  if ($std_core_types._int_gt(n,0)) {
     
    var _trmc_x10100 = undefined;
     
    var _trmc_x10101 = $std_core_types.Cons(x, _trmc_x10100);
    {
      // tail call
      var _x59 = $std_core_types._int_sub(n,1);
      var _x60 = $std_core_types._cctx_extend(_acc,_trmc_x10101,({obj: _trmc_x10101, field_name: "tail"}));
      n = _x59;
      _acc = _x60;
      continue tailcall;
    }
  }
  else {
    return $std_core_types._cctx_apply(_acc,($std_core_types.Nil));
  }
}}
 
 
// Create a list of `n` repeated elements `x`
export function replicate(x_0, n_0) /* forall<a> (x : a, n : int) -> list<a> */  {
  return _trmc_replicate(x_0, n_0, $std_core_types._cctx_empty());
}
 
 
// monadic lift
export function _mlift_remove_10434(_y_x10261) /* forall<e> (bool) -> e bool */  {
  return (_y_x10261) ? false : true;
}
 
 
// Remove those elements of a list that satisfy the given predicate `pred`.
// For example: `remove([1,2,3],odd?) == [2]`
export function remove(xs, pred) /* forall<a,e> (xs : list<a>, pred : (a) -> e bool) -> e list<a> */  {
   
  function pred_0_10552(x) /* (2821) -> 2822 bool */  {
     
    var x_0_10553 = pred(x);
    if ($std_core_hnd._yielding()) {
      return $std_core_hnd.yield_extend(function(_y_x10261 /* bool */ ) {
        return (_y_x10261) ? false : true;
      });
    }
    else {
      return (x_0_10553) ? false : true;
    }
  }
  var _x61 = $std_core_hnd._evv_is_affine();
  if (_x61) {
    return _trmc_filter(xs, pred_0_10552, $std_core_types._cctx_empty());
  }
  else {
    return _trmcm_filter(xs, pred_0_10552, function(_trmc_x10054 /* list<2821> */ ) {
        return _trmc_x10054;
      });
  }
}
 
 
// monadic lift
export function _mlift_partition_acc_10435(acc1, acc2, pred, x, xx, _y_x10263) /* forall<a,e> (acc1 : ctx<list<a>>, acc2 : ctx<list<a>>, pred : (a) -> e bool, x : a, xx : list<a>, bool) -> e (list<a>, list<a>) */  {
  if (_y_x10263) {
     
    var _cctx_x2924 = $std_core_types.Cons(x, undefined);
     
    var _cctx_x2925 = {obj: _cctx_x2924, field_name: "tail"};
    return partition_acc(xx, pred, $std_core_types._cctx_compose(acc1,($std_core_types._cctx_create(_cctx_x2924,_cctx_x2925))), acc2);
  }
  else {
     
    var _cctx_x2971 = $std_core_types.Cons(x, undefined);
     
    var _cctx_x2972 = {obj: _cctx_x2971, field_name: "tail"};
    return partition_acc(xx, pred, acc1, $std_core_types._cctx_compose(acc2,($std_core_types._cctx_create(_cctx_x2971,_cctx_x2972))));
  }
}
 
export function partition_acc(xs, pred_0, acc1_0, acc2_0) /* forall<a,e> (xs : list<a>, pred : (a) -> e bool, acc1 : ctx<list<a>>, acc2 : ctx<list<a>>) -> e (list<a>, list<a>) */  { tailcall: while(1)
{
  if (xs === null) {
    return $std_core_types.Tuple2($std_core_types._cctx_apply(acc1_0,($std_core_types.Nil)), $std_core_types._cctx_apply(acc2_0,($std_core_types.Nil)));
  }
  else {
     
    var x_1_10556 = pred_0(xs.head);
    if ($std_core_hnd._yielding()) {
      return $std_core_hnd.yield_extend(function(_y_x10263_0 /* bool */ ) {
        return _mlift_partition_acc_10435(acc1_0, acc2_0, pred_0, xs.head, xs.tail, _y_x10263_0);
      });
    }
    else {
      if (x_1_10556) {
         
        var _cctx_x2924_0 = $std_core_types.Cons(xs.head, undefined);
         
        var _cctx_x2925_0 = {obj: _cctx_x2924_0, field_name: "tail"};
        {
          // tail call
          var _x62 = $std_core_types._cctx_compose(acc1_0,($std_core_types._cctx_create(_cctx_x2924_0,_cctx_x2925_0)));
          xs = xs.tail;
          acc1_0 = _x62;
          continue tailcall;
        }
      }
      else {
         
        var _cctx_x2971_0 = $std_core_types.Cons(xs.head, undefined);
         
        var _cctx_x2972_0 = {obj: _cctx_x2971_0, field_name: "tail"};
        {
          // tail call
          var _x63 = $std_core_types._cctx_compose(acc2_0,($std_core_types._cctx_create(_cctx_x2971_0,_cctx_x2972_0)));
          xs = xs.tail;
          acc2_0 = _x63;
          continue tailcall;
        }
      }
    }
  }
}}
 
 
// Partition a list in two lists where the first list contains
// those elements that satisfy the given predicate `pred`.
// For example: `partition([1,2,3],odd?) == ([1,3],[2])`
export function partition(xs, pred) /* forall<a,e> (xs : list<a>, pred : (a) -> e bool) -> e (list<a>, list<a>) */  {
  return partition_acc(xs, pred, $std_core_types._cctx_empty(), $std_core_types._cctx_empty());
}
 
 
// monadic lift
export function _mlift_lookup_10436(kv, _y_x10269) /* forall<a,b,e> (kv : (a, b), bool) -> e maybe<b> */  {
  if (_y_x10269) {
    var _x64 = kv.snd;
    return $std_core_types.Just(_x64);
  }
  else {
    return $std_core_types.Nothing;
  }
}
 
 
// Lookup the first element satisfying some predicate
export function lookup(xs, pred) /* forall<a,b,e> (xs : list<(a, b)>, pred : (a) -> e bool) -> e maybe<b> */  {
  return foreach_while(xs, function(kv /* (3167, 3168) */ ) {
       
      var _x65 = kv.fst;
      var x_10559 = pred(_x65);
       
      function next_10560(_y_x10269) /* (bool) -> 3169 maybe<3168> */  {
        if (_y_x10269) {
          var _x66 = kv.snd;
          return $std_core_types.Just(_x66);
        }
        else {
          return $std_core_types.Nothing;
        }
      }
      if ($std_core_hnd._yielding()) {
        return $std_core_hnd.yield_extend(next_10560);
      }
      else {
        return next_10560(x_10559);
      }
    });
}
 
 
// monadic lift
export function _mlift_index_of_acc_10437(idx, pred, xx, _y_x10272) /* forall<a,e> (idx : int, pred : (a) -> e bool, xx : list<a>, bool) -> e int */  {
  if (_y_x10272) {
    return idx;
  }
  else {
    return index_of_acc(xx, pred, $std_core_types._int_add(idx,1));
  }
}
 
export function index_of_acc(xs, pred_0, idx_0) /* forall<a,e> (xs : list<a>, pred : (a) -> e bool, idx : int) -> e int */  { tailcall: while(1)
{
  if (xs !== null) {
     
    var x_0_10563 = pred_0(xs.head);
    if ($std_core_hnd._yielding()) {
      return $std_core_hnd.yield_extend(function(_y_x10272_0 /* bool */ ) {
        return _mlift_index_of_acc_10437(idx_0, pred_0, xs.tail, _y_x10272_0);
      });
    }
    else {
      if (x_0_10563) {
        return idx_0;
      }
      else {
        {
          // tail call
          var _x65 = $std_core_types._int_add(idx_0,1);
          xs = xs.tail;
          idx_0 = _x65;
          continue tailcall;
        }
      }
    }
  }
  else {
    return -1;
  }
}}
 
 
// Returns the index of the first element where `pred` holds, or `-1` if no such element exists.
export function index_of(xs, pred) /* forall<a,e> (xs : list<a>, pred : (a) -> e bool) -> e int */  {
  return index_of_acc(xs, pred, 0);
}
 
 
// monadic lift
export function _mlift_foreach_10438(action, xx, wild__) /* forall<a,e> (action : (a) -> e (), xx : list<a>, wild_ : ()) -> e () */  {
  return foreach(xx, action);
}
 
 
// Invoke `action` for each element of a list
export function foreach(xs, action_0) /* forall<a,e> (xs : list<a>, action : (a) -> e ()) -> e () */  { tailcall: while(1)
{
  if (xs !== null) {
     
    var x_0_10566 = action_0(xs.head);
    if ($std_core_hnd._yielding()) {
      return $std_core_hnd.yield_extend(function(wild___0 /* () */ ) {
        return _mlift_foreach_10438(action_0, xs.tail, wild___0);
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
export function _mlift_trmc_map_while_10439(_acc, action, xx, _y_x10280) /* forall<a,b,e> (ctx<list<b>>, action : (a) -> e maybe<b>, xx : list<a>, maybe<b>) -> e list<b> */  {
  if (_y_x10280 !== null) {
     
    var _trmc_x10102 = undefined;
     
    var _trmc_x10103 = $std_core_types.Cons(_y_x10280.value, _trmc_x10102);
    return _trmc_map_while(xx, action, $std_core_types._cctx_extend(_acc,_trmc_x10103,({obj: _trmc_x10103, field_name: "tail"})));
  }
  else {
    return $std_core_types._cctx_apply(_acc,($std_core_types.Nil));
  }
}
 
 
// monadic lift
export function _mlift_trmcm_map_while_10440(_accm, action_0, xx_0, _y_x10284) /* forall<a,b,e> ((list<b>) -> list<b>, action : (a) -> e maybe<b>, xx : list<a>, maybe<b>) -> e list<b> */  {
  if (_y_x10284 !== null) {
    return _trmcm_map_while(xx_0, action_0, function(_trmc_x10105 /* list<3382> */ ) {
        return _accm($std_core_types.Cons(_y_x10284.value, _trmc_x10105));
      });
  }
  else {
    return _accm($std_core_types.Nil);
  }
}
 
 
// Invoke `action` on each element of a list while `action` returns `Just`
export function _trmc_map_while(xs, action_1, _acc_0) /* forall<a,b,e> (xs : list<a>, action : (a) -> e maybe<b>, ctx<list<b>>) -> e list<b> */  { tailcall: while(1)
{
  if (xs === null) {
    return $std_core_types._cctx_apply(_acc_0,($std_core_types.Nil));
  }
  else {
     
    var x_0_10569 = action_1(xs.head);
    if ($std_core_hnd._yielding()) {
      return $std_core_hnd.yield_extend(function(_y_x10280_0 /* maybe<3382> */ ) {
        return _mlift_trmc_map_while_10439(_acc_0, action_1, xs.tail, _y_x10280_0);
      });
    }
    else {
      if (x_0_10569 !== null) {
         
        var _trmc_x10102_0 = undefined;
         
        var _trmc_x10103_0 = $std_core_types.Cons(x_0_10569.value, _trmc_x10102_0);
        {
          // tail call
          var _x66 = $std_core_types._cctx_extend(_acc_0,_trmc_x10103_0,({obj: _trmc_x10103_0, field_name: "tail"}));
          xs = xs.tail;
          _acc_0 = _x66;
          continue tailcall;
        }
      }
      else {
        return $std_core_types._cctx_apply(_acc_0,($std_core_types.Nil));
      }
    }
  }
}}
 
 
// Invoke `action` on each element of a list while `action` returns `Just`
export function _trmcm_map_while(xs_0, action_2, _accm_0) /* forall<a,b,e> (xs : list<a>, action : (a) -> e maybe<b>, (list<b>) -> list<b>) -> e list<b> */  { tailcall: while(1)
{
  if (xs_0 === null) {
    return _accm_0($std_core_types.Nil);
  }
  else {
     
    var x_2_10572 = action_2(xs_0.head);
    if ($std_core_hnd._yielding()) {
      return $std_core_hnd.yield_extend(function(_y_x10284_0 /* maybe<3382> */ ) {
        return _mlift_trmcm_map_while_10440(_accm_0, action_2, xs_0.tail, _y_x10284_0);
      });
    }
    else {
      if (x_2_10572 !== null) {
        {
          // tail call
          var _x69 = function(__at_accm_067 /* (list<3382>) -> list<3382> */ , _y_268 /* 3382 */ ) {
            return function(_trmc_x10105_0 /* list<3382> */ ) {
              return __at_accm_067($std_core_types.Cons(_y_268, _trmc_x10105_0));
            };
          }(_accm_0, x_2_10572.value);
          xs_0 = xs_0.tail;
          _accm_0 = _x69;
          continue tailcall;
        }
      }
      else {
        return _accm_0($std_core_types.Nil);
      }
    }
  }
}}
 
 
// Invoke `action` on each element of a list while `action` returns `Just`
export function map_while(xs_1, action_3) /* forall<a,b,e> (xs : list<a>, action : (a) -> e maybe<b>) -> e list<b> */  {
  var _x70 = $std_core_hnd._evv_is_affine();
  if (_x70) {
    return _trmc_map_while(xs_1, action_3, $std_core_types._cctx_empty());
  }
  else {
    return _trmcm_map_while(xs_1, action_3, function(_trmc_x10104 /* list<3382> */ ) {
        return _trmc_x10104;
      });
  }
}
 
 
// monadic lift
export function _mlift_foreach_indexed_10441(i, _y_x10294) /* forall<h,e> (i : local-var<h,int>, int) -> <local<h>|e> () */  {
  return ((i).value = ($std_core_types._int_add(_y_x10294,1)));
}
 
 
// monadic lift
export function _mlift_foreach_indexed_10442(i, wild__) /* forall<h,e> (i : local-var<h,int>, wild_ : ()) -> <local<h>|e> () */  {
   
  var x_10575 = ((i).value);
   
  function next_10576(_y_x10294) /* (int) -> <local<3510>|3521> () */  {
    return ((i).value = ($std_core_types._int_add(_y_x10294,1)));
  }
  if ($std_core_hnd._yielding()) {
    return $std_core_hnd.yield_extend(next_10576);
  }
  else {
    return next_10576(x_10575);
  }
}
 
 
// monadic lift
export function _mlift_foreach_indexed_10443(action, i, x, j) /* forall<h,a,e> (action : (int, a) -> e (), i : local-var<h,int>, x : a, j : int) -> <local<h>|e> () */  {
   
  var x_0_10579 = action(j, x);
  if ($std_core_hnd._yielding()) {
    return $std_core_hnd.yield_extend(function(wild__ /* () */ ) {
      return _mlift_foreach_indexed_10442(i, wild__);
    });
  }
  else {
    return _mlift_foreach_indexed_10442(i, x_0_10579);
  }
}
 
 
// Invoke `action` for each element of a list, passing also the position of the element.
export function foreach_indexed(xs, action) /* forall<a,e> (xs : list<a>, action : (int, a) -> e ()) -> e () */  {
  return function() {
     
    var loc = { value: 0 };
     
    var res = foreach(xs, function(x /* 3520 */ ) {
         
        var x_0_10584 = ((loc).value);
        if ($std_core_hnd._yielding()) {
          return $std_core_hnd.yield_extend(function(j /* int */ ) {
            return _mlift_foreach_indexed_10443(action, loc, x, j);
          });
        }
        else {
          return _mlift_foreach_indexed_10443(action, loc, x, x_0_10584);
        }
      });
    return $std_core_hnd.prompt_local_var(loc, res);
  }();
}
 
 
// lifted local: intersperse, before
export function _trmc_lift_intersperse_6007(sep, ys, _acc) /* forall<a> (sep : a, ys : list<a>, ctx<list<a>>) -> list<a> */  { tailcall: while(1)
{
  if (ys !== null) {
     
    var _trmc_x10106 = $std_core_types.Cons(ys.head, undefined);
    {
      // tail call
      var _x71 = $std_core_types._cctx_extend(_acc,($std_core_types.Cons(sep, _trmc_x10106)),({obj: _trmc_x10106, field_name: "tail"}));
      ys = ys.tail;
      _acc = _x71;
      continue tailcall;
    }
  }
  else {
    return $std_core_types._cctx_apply(_acc,($std_core_types.Nil));
  }
}}
 
 
// lifted local: intersperse, before
export function _lift_intersperse_6007(sep_0, ys_0) /* forall<a> (sep : a, ys : list<a>) -> list<a> */  {
  return _trmc_lift_intersperse_6007(sep_0, ys_0, $std_core_types._cctx_empty());
}
 
 
// Insert a separator `sep`  between all elements of a list `xs` .
export function intersperse(xs, sep) /* forall<a> (xs : list<a>, sep : a) -> list<a> */  {
  if (xs !== null) {
    return $std_core_types.Cons(xs.head, _trmc_lift_intersperse_6007(sep, xs.tail, $std_core_types._cctx_empty()));
  }
  else {
    return $std_core_types.Nil;
  }
}
 
 
// Concatenate all strings in a list
export function joinsep(xs, sep) /* (xs : list<string>, sep : string) -> string */  {
  if (xs === null) {
    return "";
  }
  if (xs !== null && xs.tail === null) {
    return xs.head;
  }
  if (xs !== null && xs.tail !== null && xs.tail.tail === null) {
    if ((sep === (""))){
      return $std_core_types._lp__plus__plus__rp_(xs.head, xs.tail.head);
    }
  }
  return ((($std_core_vector.unvlist(xs))).join(sep));
}
 
 
// Concatenate all strings in a list
export function concat_fs_join(xs) /* (xs : list<string>) -> string */  {
  if (xs === null) {
    return "";
  }
  else if (xs !== null && xs.tail === null) {
    return xs.head;
  }
  else if (xs !== null && xs.tail !== null && xs.tail.tail === null) {
    return $std_core_types._lp__plus__plus__rp_(xs.head, xs.tail.head);
  }
  else {
    return ((($std_core_vector.unvlist(xs))).join(''));
  }
}
 
 
// Concatenate all strings in a list using a specific separator
export function joinsep_fs_join(xs, sep) /* (xs : list<string>, sep : string) -> string */  {
  return joinsep(xs, sep);
}
 
 
// monadic lift
export function _mlift_show_10444(_y_x10299) /* forall<e> (list<string>) -> e string */  {
  match: {
    if (_y_x10299 === null) {
      var _x72 = "";
      break match;
    }
    if (_y_x10299 !== null && _y_x10299.tail === null) {
      var _x72 = _y_x10299.head;
      break match;
    }
    if (_y_x10299 !== null && _y_x10299.tail !== null && _y_x10299.tail.tail === null) {
      if (((",") === (""))){
        var _x72 = $std_core_types._lp__plus__plus__rp_(_y_x10299.head, _y_x10299.tail.head);
        break match;
      }
    }
    var _x72 = ((($std_core_vector.unvlist(_y_x10299))).join((",")));
  }
  return $std_core_types._lp__plus__plus__rp_("[", $std_core_types._lp__plus__plus__rp_(_x72, "]"));
}
 
 
// Show a list
export function show(xs, _implicit_fs_show) /* forall<a,e> (xs : list<a>, ?show : (a) -> e string) -> e string */  {
   
  var x_10588 = map(xs, _implicit_fs_show);
  if ($std_core_hnd._yielding()) {
    return $std_core_hnd.yield_extend(function(_y_x10299 /* list<string> */ ) {
      return _mlift_show_10444(_y_x10299);
    });
  }
  else {
    match: {
      if (x_10588 === null) {
        var _x73 = "";
        break match;
      }
      if (x_10588 !== null && x_10588.tail === null) {
        var _x73 = x_10588.head;
        break match;
      }
      if (x_10588 !== null && x_10588.tail !== null && x_10588.tail.tail === null) {
        if (((",") === (""))){
          var _x73 = $std_core_types._lp__plus__plus__rp_(x_10588.head, x_10588.tail.head);
          break match;
        }
      }
      var _x73 = ((($std_core_vector.unvlist(x_10588))).join((",")));
    }
    return $std_core_types._lp__plus__plus__rp_("[", $std_core_types._lp__plus__plus__rp_(_x73, "]"));
  }
}
 
 
// _deprecated_, use `list/show` instead.
export function show_list(xs, show_elem) /* forall<a,e> (xs : list<a>, show-elem : (a) -> e string) -> e string */  {
  return show(xs, show_elem);
}
 
 
// Zip two lists together by pairing the corresponding elements.
// The returned list is only as long as the smallest input list.
export function _trmc_zip(xs, ys, _acc) /* forall<a,b> (xs : list<a>, ys : list<b>, ctx<list<(a, b)>>) -> list<(a, b)> */  { tailcall: while(1)
{
  if (xs !== null) {
    if (ys !== null) {
       
      var _trmc_x10108 = undefined;
       
      var _trmc_x10109 = $std_core_types.Cons($std_core_types.Tuple2(xs.head, ys.head), _trmc_x10108);
      {
        // tail call
        var _x74 = $std_core_types._cctx_extend(_acc,_trmc_x10109,({obj: _trmc_x10109, field_name: "tail"}));
        xs = xs.tail;
        ys = ys.tail;
        _acc = _x74;
        continue tailcall;
      }
    }
    else {
      return $std_core_types._cctx_apply(_acc,($std_core_types.Nil));
    }
  }
  else {
    return $std_core_types._cctx_apply(_acc,($std_core_types.Nil));
  }
}}
 
 
// Zip two lists together by pairing the corresponding elements.
// The returned list is only as long as the smallest input list.
export function zip(xs_0, ys_0) /* forall<a,b> (xs : list<a>, ys : list<b>) -> list<(a, b)> */  {
  return _trmc_zip(xs_0, ys_0, $std_core_types._cctx_empty());
}
 
 
// monadic lift
export function _mlift_trmc_zipwith_10445(_acc, f, xx, yy, _trmc_x10110) /* forall<a,b,c,e> (ctx<list<c>>, f : (a, b) -> e c, xx : list<a>, yy : list<b>, c) -> e list<c> */  {
   
  var _trmc_x10111 = undefined;
   
  var _trmc_x10112 = $std_core_types.Cons(_trmc_x10110, _trmc_x10111);
  return _trmc_zipwith(xx, yy, f, $std_core_types._cctx_extend(_acc,_trmc_x10112,({obj: _trmc_x10112, field_name: "tail"})));
}
 
 
// monadic lift
export function _mlift_trmcm_zipwith_10446(_accm, f_0, xx_0, yy_0, _trmc_x10115) /* forall<a,b,c,e> ((list<c>) -> list<c>, f : (a, b) -> e c, xx : list<a>, yy : list<b>, c) -> e list<c> */  {
  return _trmcm_zipwith(xx_0, yy_0, f_0, function(_trmc_x10114 /* list<4000> */ ) {
      return _accm($std_core_types.Cons(_trmc_x10115, _trmc_x10114));
    });
}
 
 
// Zip two lists together by apply a function `f` to all corresponding elements.
// The returned list is only as long as the smallest input list.
export function _trmc_zipwith(xs, ys, f_1, _acc_0) /* forall<a,b,c,e> (xs : list<a>, ys : list<b>, f : (a, b) -> e c, ctx<list<c>>) -> e list<c> */  { tailcall: while(1)
{
  if (xs !== null) {
    if (ys !== null) {
       
      var x_0_10591 = f_1(xs.head, ys.head);
      if ($std_core_hnd._yielding()) {
        return $std_core_hnd.yield_extend(function(_trmc_x10110_0 /* 4000 */ ) {
          return _mlift_trmc_zipwith_10445(_acc_0, f_1, xs.tail, ys.tail, _trmc_x10110_0);
        });
      }
      else {
         
        var _trmc_x10111_0 = undefined;
         
        var _trmc_x10112_0 = $std_core_types.Cons(x_0_10591, _trmc_x10111_0);
        {
          // tail call
          var _x75 = $std_core_types._cctx_extend(_acc_0,_trmc_x10112_0,({obj: _trmc_x10112_0, field_name: "tail"}));
          xs = xs.tail;
          ys = ys.tail;
          _acc_0 = _x75;
          continue tailcall;
        }
      }
    }
    else {
      return $std_core_types._cctx_apply(_acc_0,($std_core_types.Nil));
    }
  }
  else {
    return $std_core_types._cctx_apply(_acc_0,($std_core_types.Nil));
  }
}}
 
 
// Zip two lists together by apply a function `f` to all corresponding elements.
// The returned list is only as long as the smallest input list.
export function _trmcm_zipwith(xs_0, ys_0, f_2, _accm_0) /* forall<a,b,c,e> (xs : list<a>, ys : list<b>, f : (a, b) -> e c, (list<c>) -> list<c>) -> e list<c> */  { tailcall: while(1)
{
  if (xs_0 !== null) {
    if (ys_0 !== null) {
       
      var x_2_10594 = f_2(xs_0.head, ys_0.head);
      if ($std_core_hnd._yielding()) {
        return $std_core_hnd.yield_extend(function(_trmc_x10115_0 /* 4000 */ ) {
          return _mlift_trmcm_zipwith_10446(_accm_0, f_2, xs_0.tail, ys_0.tail, _trmc_x10115_0);
        });
      }
      else {
        {
          // tail call
          var _x78 = function(__at_accm_076 /* (list<4000>) -> list<4000> */ , _x_2_1059477 /* 4000 */ ) {
            return function(_trmc_x10114_0 /* list<4000> */ ) {
              return __at_accm_076($std_core_types.Cons(_x_2_1059477, _trmc_x10114_0));
            };
          }(_accm_0, x_2_10594);
          xs_0 = xs_0.tail;
          ys_0 = ys_0.tail;
          _accm_0 = _x78;
          continue tailcall;
        }
      }
    }
    else {
      return _accm_0($std_core_types.Nil);
    }
  }
  else {
    return _accm_0($std_core_types.Nil);
  }
}}
 
 
// Zip two lists together by apply a function `f` to all corresponding elements.
// The returned list is only as long as the smallest input list.
export function zipwith(xs_1, ys_1, f_3) /* forall<a,b,c,e> (xs : list<a>, ys : list<b>, f : (a, b) -> e c) -> e list<c> */  {
  var _x79 = $std_core_hnd._evv_is_affine();
  if (_x79) {
    return _trmc_zipwith(xs_1, ys_1, f_3, $std_core_types._cctx_empty());
  }
  else {
    return _trmcm_zipwith(xs_1, ys_1, f_3, function(_trmc_x10113 /* list<4000> */ ) {
        return _trmc_x10113;
      });
  }
}
 
 
// monadic lift
export function _mlift_trmc_zipwith_iter_10447(_acc, f, i, xx, yy, _trmc_x10116) /* forall<a,b,c,d,e> (ctx<list<a>>, f : (int, b, c) -> e d, i : int, xx : list<b>, yy : list<c>, d) -> e list<d> */  {
   
  var _trmc_x10117 = undefined;
   
  var _trmc_x10118 = $std_core_types.Cons(_trmc_x10116, _trmc_x10117);
  return _trmc_zipwith_iter($std_core_types._int_add(i,1), xx, yy, f, $std_core_types._cctx_extend(_acc,_trmc_x10118,({obj: _trmc_x10118, field_name: "tail"})));
}
 
 
// monadic lift
export function _mlift_trmcm_zipwith_iter_10448(_accm, f_0, i_0, xx_0, yy_0, _trmc_x10121) /* forall<a,b,c,d,e> ((list<a>) -> list<a>, f : (int, b, c) -> e d, i : int, xx : list<b>, yy : list<c>, d) -> e list<d> */  {
  return _trmcm_zipwith_iter($std_core_types._int_add(i_0,1), xx_0, yy_0, f_0, function(_trmc_x10120 /* list<4086> */ ) {
      return _accm($std_core_types.Cons(_trmc_x10121, _trmc_x10120));
    });
}
 
export function _trmc_zipwith_iter(i_1, xs, ys, f_1, _acc_0) /* forall<a,b,c,e> (i : int, xs : list<a>, ys : list<b>, f : (int, a, b) -> e c, ctx<list<c>>) -> e list<c> */  { tailcall: while(1)
{
  if (xs !== null) {
    if (ys !== null) {
       
      var x_0_10597 = f_1(i_1, xs.head, ys.head);
      if ($std_core_hnd._yielding()) {
        return $std_core_hnd.yield_extend(function(_trmc_x10116_0 /* 4086 */ ) {
          return _mlift_trmc_zipwith_iter_10447(_acc_0, f_1, i_1, xs.tail, ys.tail, _trmc_x10116_0);
        });
      }
      else {
         
        var _trmc_x10117_0 = undefined;
         
        var _trmc_x10118_0 = $std_core_types.Cons(x_0_10597, _trmc_x10117_0);
        {
          // tail call
          var _x80 = $std_core_types._int_add(i_1,1);
          var _x81 = $std_core_types._cctx_extend(_acc_0,_trmc_x10118_0,({obj: _trmc_x10118_0, field_name: "tail"}));
          i_1 = _x80;
          xs = xs.tail;
          ys = ys.tail;
          _acc_0 = _x81;
          continue tailcall;
        }
      }
    }
    else {
      return $std_core_types._cctx_apply(_acc_0,($std_core_types.Nil));
    }
  }
  else {
    return $std_core_types._cctx_apply(_acc_0,($std_core_types.Nil));
  }
}}
 
export function _trmcm_zipwith_iter(i_2, xs_0, ys_0, f_2, _accm_0) /* forall<a,b,c,e> (i : int, xs : list<a>, ys : list<b>, f : (int, a, b) -> e c, (list<c>) -> list<c>) -> e list<c> */  { tailcall: while(1)
{
  if (xs_0 !== null) {
    if (ys_0 !== null) {
       
      var x_2_10600 = f_2(i_2, xs_0.head, ys_0.head);
      if ($std_core_hnd._yielding()) {
        return $std_core_hnd.yield_extend(function(_trmc_x10121_0 /* 4086 */ ) {
          return _mlift_trmcm_zipwith_iter_10448(_accm_0, f_2, i_2, xs_0.tail, ys_0.tail, _trmc_x10121_0);
        });
      }
      else {
        {
          // tail call
          var _x84 = $std_core_types._int_add(i_2,1);
          var _x85 = function(__at_accm_082 /* (list<4082>) -> list<4082> */ , _x_2_1060083 /* 4086 */ ) {
            return function(_trmc_x10120_0 /* list<4086> */ ) {
              return __at_accm_082($std_core_types.Cons(_x_2_1060083, _trmc_x10120_0));
            };
          }(_accm_0, x_2_10600);
          i_2 = _x84;
          xs_0 = xs_0.tail;
          ys_0 = ys_0.tail;
          _accm_0 = _x85;
          continue tailcall;
        }
      }
    }
    else {
      return _accm_0($std_core_types.Nil);
    }
  }
  else {
    return _accm_0($std_core_types.Nil);
  }
}}
 
export function zipwith_iter(i_3, xs_1, ys_1, f_3) /* forall<a,b,c,e> (i : int, xs : list<a>, ys : list<b>, f : (int, a, b) -> e c) -> e list<c> */  {
  var _x86 = $std_core_hnd._evv_is_affine();
  if (_x86) {
    return _trmc_zipwith_iter(i_3, xs_1, ys_1, f_3, $std_core_types._cctx_empty());
  }
  else {
    return _trmcm_zipwith_iter(i_3, xs_1, ys_1, f_3, function(_trmc_x10119 /* list<4086> */ ) {
        return _trmc_x10119;
      });
  }
}
 
 
// Zip two lists together by apply a function `f` to all corresponding elements
// and their index in the list.
// The returned list is only as long as the smallest input list.
export function zipwith_indexed(xs0, ys0, f) /* forall<a,b,c,e> (xs0 : list<a>, ys0 : list<b>, f : (int, a, b) -> e c) -> e list<c> */  {
  var _x87 = $std_core_hnd._evv_is_affine();
  if (_x87) {
    return _trmc_zipwith_iter(0, xs0, ys0, f, $std_core_types._cctx_empty());
  }
  else {
    return _trmcm_zipwith_iter(0, xs0, ys0, f, function(_trmc_x10119 /* list<4129> */ ) {
        return _trmc_x10119;
      });
  }
}
 
 
// lifted local: unzip, iter
export function _lift_unzip_6008(ys, acc1, acc2) /* forall<a,b,c,d> (ys : list<(a, b)>, acc1 : cctx<c,list<a>>, acc2 : cctx<d,list<b>>) -> (c, d) */  { tailcall: while(1)
{
  if (ys !== null) {
     
    var _cctx_x4190 = $std_core_types.Cons(ys.head.fst, undefined);
     
    var _cctx_x4191 = {obj: _cctx_x4190, field_name: "tail"};
     
    var _cctx_x4233 = $std_core_types.Cons(ys.head.snd, undefined);
     
    var _cctx_x4234 = {obj: _cctx_x4233, field_name: "tail"};
    {
      // tail call
      var _x88 = $std_core_types._cctx_compose(acc1,($std_core_types._cctx_create(_cctx_x4190,_cctx_x4191)));
      var _x89 = $std_core_types._cctx_compose(acc2,($std_core_types._cctx_create(_cctx_x4233,_cctx_x4234)));
      ys = ys.tail;
      acc1 = _x88;
      acc2 = _x89;
      continue tailcall;
    }
  }
  else {
    return $std_core_types.Tuple2($std_core_types._cctx_apply(acc1,($std_core_types.Nil)), $std_core_types._cctx_apply(acc2,($std_core_types.Nil)));
  }
}}
 
 
// Unzip a list of pairs into two lists
export function unzip(xs) /* forall<a,b> (xs : list<(a, b)>) -> (list<a>, list<b>) */  {
  return _lift_unzip_6008(xs, $std_core_types._cctx_empty(), $std_core_types._cctx_empty());
}
 
 
// lifted local: unzip3, iter
export function _lift_unzip3_6009(ys, acc1, acc2, acc3) /* forall<a,b,c,d,a1,b1> (ys : list<(a, b, c)>, acc1 : cctx<d,list<a>>, acc2 : cctx<a1,list<b>>, acc3 : cctx<b1,list<c>>) -> (d, a1, b1) */  { tailcall: while(1)
{
  if (ys !== null) {
     
    var _cctx_x4406 = $std_core_types.Cons(ys.head.fst, undefined);
     
    var _cctx_x4407 = {obj: _cctx_x4406, field_name: "tail"};
     
    var _cctx_x4449 = $std_core_types.Cons(ys.head.snd, undefined);
     
    var _cctx_x4450 = {obj: _cctx_x4449, field_name: "tail"};
     
    var _cctx_x4492 = $std_core_types.Cons(ys.head.thd, undefined);
     
    var _cctx_x4493 = {obj: _cctx_x4492, field_name: "tail"};
    {
      // tail call
      var _x90 = $std_core_types._cctx_compose(acc1,($std_core_types._cctx_create(_cctx_x4406,_cctx_x4407)));
      var _x91 = $std_core_types._cctx_compose(acc2,($std_core_types._cctx_create(_cctx_x4449,_cctx_x4450)));
      var _x92 = $std_core_types._cctx_compose(acc3,($std_core_types._cctx_create(_cctx_x4492,_cctx_x4493)));
      ys = ys.tail;
      acc1 = _x90;
      acc2 = _x91;
      acc3 = _x92;
      continue tailcall;
    }
  }
  else {
    return $std_core_types.Tuple3($std_core_types._cctx_apply(acc1,($std_core_types.Nil)), $std_core_types._cctx_apply(acc2,($std_core_types.Nil)), $std_core_types._cctx_apply(acc3,($std_core_types.Nil)));
  }
}}
 
 
// Unzip a list of triples into three lists
export function unzip3(xs) /* forall<a,b,c> (xs : list<(a, b, c)>) -> (list<a>, list<b>, list<c>) */  {
  return _lift_unzip3_6009(xs, $std_core_types._cctx_empty(), $std_core_types._cctx_empty(), $std_core_types._cctx_empty());
}
 
 
// lifted local: unzip4, iter
export function _lift_unzip4_6010(ys, acc1, acc2, acc3, acc4) /* forall<a,b,c,d,a1,b1,c1,d1> (ys : list<(a, b, c, d)>, acc1 : cctx<a1,list<a>>, acc2 : cctx<b1,list<b>>, acc3 : cctx<c1,list<c>>, acc4 : cctx<d1,list<d>>) -> (a1, b1, c1, d1) */  { tailcall: while(1)
{
  if (ys !== null) {
     
    var _cctx_x4713 = $std_core_types.Cons(ys.head.fst, undefined);
     
    var _cctx_x4714 = {obj: _cctx_x4713, field_name: "tail"};
     
    var _cctx_x4756 = $std_core_types.Cons(ys.head.snd, undefined);
     
    var _cctx_x4757 = {obj: _cctx_x4756, field_name: "tail"};
     
    var _cctx_x4799 = $std_core_types.Cons(ys.head.thd, undefined);
     
    var _cctx_x4800 = {obj: _cctx_x4799, field_name: "tail"};
     
    var _cctx_x4842 = $std_core_types.Cons(ys.head.field4, undefined);
     
    var _cctx_x4843 = {obj: _cctx_x4842, field_name: "tail"};
    {
      // tail call
      var _x93 = $std_core_types._cctx_compose(acc1,($std_core_types._cctx_create(_cctx_x4713,_cctx_x4714)));
      var _x94 = $std_core_types._cctx_compose(acc2,($std_core_types._cctx_create(_cctx_x4756,_cctx_x4757)));
      var _x95 = $std_core_types._cctx_compose(acc3,($std_core_types._cctx_create(_cctx_x4799,_cctx_x4800)));
      var _x96 = $std_core_types._cctx_compose(acc4,($std_core_types._cctx_create(_cctx_x4842,_cctx_x4843)));
      ys = ys.tail;
      acc1 = _x93;
      acc2 = _x94;
      acc3 = _x95;
      acc4 = _x96;
      continue tailcall;
    }
  }
  else {
    return $std_core_types.Tuple4($std_core_types._cctx_apply(acc1,($std_core_types.Nil)), $std_core_types._cctx_apply(acc2,($std_core_types.Nil)), $std_core_types._cctx_apply(acc3,($std_core_types.Nil)), $std_core_types._cctx_apply(acc4,($std_core_types.Nil)));
  }
}}
 
 
// Unzip a list of quadruples into four lists
export function unzip4(xs) /* forall<a,b,c,d> (xs : list<(a, b, c, d)>) -> (list<a>, list<b>, list<c>, list<d>) */  {
  return _lift_unzip4_6010(xs, $std_core_types._cctx_empty(), $std_core_types._cctx_empty(), $std_core_types._cctx_empty(), $std_core_types._cctx_empty());
}
 
 
// monadic lift
export function _mlift_lift_span_6011_10449(acc, predicate, y, ys, yy, _y_x10324) /* forall<a,b,e> (acc : cctx<a,list<b>>, predicate : (b) -> e bool, y : b, ys : list<b>, yy : list<b>, bool) -> e (a, list<b>) */  {
  if (_y_x10324) {
     
    var _cctx_x5097 = $std_core_types.Cons(y, undefined);
     
    var _cctx_x5098 = {obj: _cctx_x5097, field_name: "tail"};
     
    var acc_0_10030 = $std_core_types._cctx_compose(acc,($std_core_types._cctx_create(_cctx_x5097,_cctx_x5098)));
    return _lift_span_6011(predicate, yy, acc_0_10030);
  }
  else {
    return $std_core_types.Tuple2($std_core_types._cctx_apply(acc,($std_core_types.Nil)), ys);
  }
}
 
 
// lifted local: span, span-acc
export function _lift_span_6011(predicate_0, ys_0, acc_0) /* forall<a,e,b> (predicate : (a) -> e bool, ys : list<a>, acc : cctx<b,list<a>>) -> e (b, list<a>) */  { tailcall: while(1)
{
  if (ys_0 !== null) {
     
    var x_10607 = predicate_0(ys_0.head);
    if ($std_core_hnd._yielding()) {
      return $std_core_hnd.yield_extend(function(_y_x10324_0 /* bool */ ) {
        return _mlift_lift_span_6011_10449(acc_0, predicate_0, ys_0.head, ys_0, ys_0.tail, _y_x10324_0);
      });
    }
    else {
      if (x_10607) {
         
        var _cctx_x5097_0 = $std_core_types.Cons(ys_0.head, undefined);
         
        var _cctx_x5098_0 = {obj: _cctx_x5097_0, field_name: "tail"};
         
        var acc_0_10030_0 = $std_core_types._cctx_compose(acc_0,($std_core_types._cctx_create(_cctx_x5097_0,_cctx_x5098_0)));
        {
          // tail call
          ys_0 = ys_0.tail;
          acc_0 = acc_0_10030_0;
          continue tailcall;
        }
      }
      else {
        return $std_core_types.Tuple2($std_core_types._cctx_apply(acc_0,($std_core_types.Nil)), ys_0);
      }
    }
  }
  else {
    return $std_core_types.Tuple2($std_core_types._cctx_apply(acc_0,($std_core_types.Nil)), ys_0);
  }
}}
 
export function span(xs, predicate) /* forall<a,e> (xs : list<a>, predicate : (a) -> e bool) -> e (list<a>, list<a>) */  {
   
  var acc = $std_core_types._cctx_empty();
  return _lift_span_6011(predicate, xs, acc);
}
 
 
// monadic lift
export function _mlift_trmc_take_while_10450(_acc, predicate, x, xx, _y_x10329) /* forall<a,e> (ctx<list<a>>, predicate : (a) -> e bool, x : a, xx : list<a>, bool) -> e list<a> */  {
  if (_y_x10329) {
     
    var _trmc_x10122 = undefined;
     
    var _trmc_x10123 = $std_core_types.Cons(x, _trmc_x10122);
    return _trmc_take_while(xx, predicate, $std_core_types._cctx_extend(_acc,_trmc_x10123,({obj: _trmc_x10123, field_name: "tail"})));
  }
  else {
    return $std_core_types._cctx_apply(_acc,($std_core_types.Nil));
  }
}
 
 
// monadic lift
export function _mlift_trmcm_take_while_10451(_accm, predicate_0, x_0, xx_0, _y_x10333) /* forall<a,e> ((list<a>) -> list<a>, predicate : (a) -> e bool, x : a, xx : list<a>, bool) -> e list<a> */  {
  if (_y_x10333) {
    return _trmcm_take_while(xx_0, predicate_0, function(_trmc_x10125 /* list<5253> */ ) {
        return _accm($std_core_types.Cons(x_0, _trmc_x10125));
      });
  }
  else {
    return _accm($std_core_types.Nil);
  }
}
 
 
// Keep only those initial elements that satisfy `predicate`
export function _trmc_take_while(xs, predicate_1, _acc_0) /* forall<a,e> (xs : list<a>, predicate : (a) -> e bool, ctx<list<a>>) -> e list<a> */  { tailcall: while(1)
{
  if (xs !== null) {
     
    var x_2_10610 = predicate_1(xs.head);
    if ($std_core_hnd._yielding()) {
      return $std_core_hnd.yield_extend(function(_y_x10329_0 /* bool */ ) {
        return _mlift_trmc_take_while_10450(_acc_0, predicate_1, xs.head, xs.tail, _y_x10329_0);
      });
    }
    else {
      if (x_2_10610) {
         
        var _trmc_x10122_0 = undefined;
         
        var _trmc_x10123_0 = $std_core_types.Cons(xs.head, _trmc_x10122_0);
        {
          // tail call
          var _x97 = $std_core_types._cctx_extend(_acc_0,_trmc_x10123_0,({obj: _trmc_x10123_0, field_name: "tail"}));
          xs = xs.tail;
          _acc_0 = _x97;
          continue tailcall;
        }
      }
      else {
        return $std_core_types._cctx_apply(_acc_0,($std_core_types.Nil));
      }
    }
  }
  else {
    return $std_core_types._cctx_apply(_acc_0,($std_core_types.Nil));
  }
}}
 
 
// Keep only those initial elements that satisfy `predicate`
export function _trmcm_take_while(xs_0, predicate_2, _accm_0) /* forall<a,e> (xs : list<a>, predicate : (a) -> e bool, (list<a>) -> list<a>) -> e list<a> */  { tailcall: while(1)
{
  if (xs_0 !== null) {
     
    var x_4_10613 = predicate_2(xs_0.head);
    if ($std_core_hnd._yielding()) {
      return $std_core_hnd.yield_extend(function(_y_x10333_0 /* bool */ ) {
        return _mlift_trmcm_take_while_10451(_accm_0, predicate_2, xs_0.head, xs_0.tail, _y_x10333_0);
      });
    }
    else {
      if (x_4_10613) {
        {
          // tail call
          var _x100 = function(__at_accm_098 /* (list<5253>) -> list<5253> */ , _x_399 /* 5253 */ ) {
            return function(_trmc_x10125_0 /* list<5253> */ ) {
              return __at_accm_098($std_core_types.Cons(_x_399, _trmc_x10125_0));
            };
          }(_accm_0, xs_0.head);
          xs_0 = xs_0.tail;
          _accm_0 = _x100;
          continue tailcall;
        }
      }
      else {
        return _accm_0($std_core_types.Nil);
      }
    }
  }
  else {
    return _accm_0($std_core_types.Nil);
  }
}}
 
 
// Keep only those initial elements that satisfy `predicate`
export function take_while(xs_1, predicate_3) /* forall<a,e> (xs : list<a>, predicate : (a) -> e bool) -> e list<a> */  {
  var _x101 = $std_core_hnd._evv_is_affine();
  if (_x101) {
    return _trmc_take_while(xs_1, predicate_3, $std_core_types._cctx_empty());
  }
  else {
    return _trmcm_take_while(xs_1, predicate_3, function(_trmc_x10124 /* list<5253> */ ) {
        return _trmc_x10124;
      });
  }
}
 
 
// Concatenate all strings in a list in reverse order
export function reverse_join(xs) /* (xs : list<string>) -> string */  {
   
  var xs_0_10031 = reverse_acc($std_core_types.Nil, xs);
  if (xs_0_10031 === null) {
    return "";
  }
  else if (xs_0_10031 !== null && xs_0_10031.tail === null) {
    return xs_0_10031.head;
  }
  else if (xs_0_10031 !== null && xs_0_10031.tail !== null && xs_0_10031.tail.tail === null) {
    return $std_core_types._lp__plus__plus__rp_(xs_0_10031.head, xs_0_10031.tail.head);
  }
  else {
    return ((($std_core_vector.unvlist(xs_0_10031))).join(''));
  }
}
 
 
// Append `end` to each string in the list `xs` and join them all together.\
// `join-end([],end) === ""`\
// `join-end(["a","b"],"/") === "a/b/"`
export function join_end(xs, end) /* (xs : list<string>, end : string) -> string */  {
  if (xs === null) {
    return "";
  }
  else {
    return $std_core_types._lp__plus__plus__rp_(joinsep(xs, end), end);
  }
}
 
 
// lifted local: concat, concat-pre
export function _trmc_lift_concat_6012(ys, zss, _acc) /* forall<a> (ys : list<a>, zss : list<list<a>>, ctx<list<a>>) -> list<a> */  { tailcall: while(1)
{
  if (ys !== null) {
     
    var _trmc_x10126 = undefined;
     
    var _trmc_x10127 = $std_core_types.Cons(ys.head, _trmc_x10126);
    {
      // tail call
      var _x102 = $std_core_types._cctx_extend(_acc,_trmc_x10127,({obj: _trmc_x10127, field_name: "tail"}));
      ys = ys.tail;
      _acc = _x102;
      continue tailcall;
    }
  }
  else {
    if (zss !== null) {
      {
        // tail call
        ys = zss.head;
        zss = zss.tail;
        continue tailcall;
      }
    }
    else {
      return $std_core_types._cctx_apply(_acc,($std_core_types.Nil));
    }
  }
}}
 
 
// lifted local: concat, concat-pre
export function _lift_concat_6012(ys_0, zss_0) /* forall<a> (ys : list<a>, zss : list<list<a>>) -> list<a> */  {
  return _trmc_lift_concat_6012(ys_0, zss_0, $std_core_types._cctx_empty());
}
 
 
// Concatenate all lists in a list (e.g. flatten the list). (tail-recursive)
export function concat(xss) /* forall<a> (xss : list<list<a>>) -> list<a> */  {
  return _trmc_lift_concat_6012($std_core_types.Nil, xss, $std_core_types._cctx_empty());
}
 
 
// monadic lift
export function _mlift_trmc_lift_flatmap_6013_10452(_acc, f, zz, ys_1_10038) /* forall<a,b,e> (ctx<list<b>>, f : (a) -> e list<b>, zz : list<a>, ys@1@10038 : list<b>) -> e list<b> */  {
  return _trmc_lift_flatmap_6013(f, ys_1_10038, zz, _acc);
}
 
 
// monadic lift
export function _mlift_trmcm_lift_flatmap_6013_10453(_accm, f_0, zz_0, ys_1_10038_0) /* forall<a,b,e> ((list<b>) -> list<b>, f : (a) -> e list<b>, zz : list<a>, ys@1@10038 : list<b>) -> e list<b> */  {
  return _trmcm_lift_flatmap_6013(f_0, ys_1_10038_0, zz_0, _accm);
}
 
 
// lifted local: flatmap, flatmap-pre
export function _trmc_lift_flatmap_6013(f_1, ys, zs, _acc_0) /* forall<a,b,e> (f : (a) -> e list<b>, ys : list<b>, zs : list<a>, ctx<list<b>>) -> e list<b> */  { tailcall: while(1)
{
  if (ys !== null) {
     
    var _trmc_x10128 = undefined;
     
    var _trmc_x10129 = $std_core_types.Cons(ys.head, _trmc_x10128);
    {
      // tail call
      var _x103 = $std_core_types._cctx_extend(_acc_0,_trmc_x10129,({obj: _trmc_x10129, field_name: "tail"}));
      ys = ys.tail;
      _acc_0 = _x103;
      continue tailcall;
    }
  }
  else {
    if (zs !== null) {
       
      var x_10618 = f_1(zs.head);
      if ($std_core_hnd._yielding()) {
        return $std_core_hnd.yield_extend(function(ys_1_10038_1 /* list<5455> */ ) {
          return _mlift_trmc_lift_flatmap_6013_10452(_acc_0, f_1, zs.tail, ys_1_10038_1);
        });
      }
      else {
        {
          // tail call
          ys = x_10618;
          zs = zs.tail;
          continue tailcall;
        }
      }
    }
    else {
      return $std_core_types._cctx_apply(_acc_0,($std_core_types.Nil));
    }
  }
}}
 
 
// lifted local: flatmap, flatmap-pre
export function _trmcm_lift_flatmap_6013(f_2, ys_0, zs_0, _accm_0) /* forall<a,b,e> (f : (a) -> e list<b>, ys : list<b>, zs : list<a>, (list<b>) -> list<b>) -> e list<b> */  { tailcall: while(1)
{
  if (ys_0 !== null) {
    {
      // tail call
      var _x106 = function(__at_accm_0104 /* (list<5455>) -> list<5455> */ , _y_0105 /* 5455 */ ) {
        return function(_trmc_x10131 /* list<5455> */ ) {
          return __at_accm_0104($std_core_types.Cons(_y_0105, _trmc_x10131));
        };
      }(_accm_0, ys_0.head);
      ys_0 = ys_0.tail;
      _accm_0 = _x106;
      continue tailcall;
    }
  }
  else {
    if (zs_0 !== null) {
       
      var x_0_10621 = f_2(zs_0.head);
      if ($std_core_hnd._yielding()) {
        return $std_core_hnd.yield_extend(function(ys_1_10038_3 /* list<5455> */ ) {
          return _mlift_trmcm_lift_flatmap_6013_10453(_accm_0, f_2, zs_0.tail, ys_1_10038_3);
        });
      }
      else {
        {
          // tail call
          ys_0 = x_0_10621;
          zs_0 = zs_0.tail;
          continue tailcall;
        }
      }
    }
    else {
      return _accm_0($std_core_types.Nil);
    }
  }
}}
 
 
// lifted local: flatmap, flatmap-pre
export function _lift_flatmap_6013(f_3, ys_1, zs_1) /* forall<a,b,e> (f : (a) -> e list<b>, ys : list<b>, zs : list<a>) -> e list<b> */  {
  var _x107 = $std_core_hnd._evv_is_affine();
  if (_x107) {
    return _trmc_lift_flatmap_6013(f_3, ys_1, zs_1, $std_core_types._cctx_empty());
  }
  else {
    return _trmcm_lift_flatmap_6013(f_3, ys_1, zs_1, function(_trmc_x10130 /* list<5455> */ ) {
        return _trmc_x10130;
      });
  }
}
 
 
// Concatenate the result lists from applying a function to all elements.
export function flatmap(xs, f) /* forall<a,b,e> (xs : list<a>, f : (a) -> e list<b>) -> e list<b> */  {
  var _x108 = $std_core_hnd._evv_is_affine();
  if (_x108) {
    return _trmc_lift_flatmap_6013(f, $std_core_types.Nil, xs, $std_core_types._cctx_empty());
  }
  else {
    return _trmcm_lift_flatmap_6013(f, $std_core_types.Nil, xs, function(_trmc_x10130 /* list<5455> */ ) {
        return _trmc_x10130;
      });
  }
}
 
 
// monadic lift
export function _mlift_trmc_flatmap_maybe_10454(_acc, f, xx, _y_x10354) /* forall<a,b,e> (ctx<list<b>>, f : (a) -> e maybe<b>, xx : list<a>, maybe<b>) -> e list<b> */  {
  if (_y_x10354 !== null) {
     
    var _trmc_x10132 = undefined;
     
    var _trmc_x10133 = $std_core_types.Cons(_y_x10354.value, _trmc_x10132);
    return _trmc_flatmap_maybe(xx, f, $std_core_types._cctx_extend(_acc,_trmc_x10133,({obj: _trmc_x10133, field_name: "tail"})));
  }
  else {
    return _trmc_flatmap_maybe(xx, f, _acc);
  }
}
 
 
// monadic lift
export function _mlift_trmcm_flatmap_maybe_10455(_accm, f_0, xx_0, _y_x10359) /* forall<a,b,e> ((list<b>) -> list<b>, f : (a) -> e maybe<b>, xx : list<a>, maybe<b>) -> e list<b> */  {
  if (_y_x10359 !== null) {
    return _trmcm_flatmap_maybe(xx_0, f_0, function(_trmc_x10135 /* list<5542> */ ) {
        return _accm($std_core_types.Cons(_y_x10359.value, _trmc_x10135));
      });
  }
  else {
    return _trmcm_flatmap_maybe(xx_0, f_0, _accm);
  }
}
 
 
// Concatenate the `Just` result elements from applying a function to all elements.
export function _trmc_flatmap_maybe(xs, f_1, _acc_0) /* forall<a,b,e> (xs : list<a>, f : (a) -> e maybe<b>, ctx<list<b>>) -> e list<b> */  { tailcall: while(1)
{
  if (xs !== null) {
     
    var x_0_10627 = f_1(xs.head);
    if ($std_core_hnd._yielding()) {
      return $std_core_hnd.yield_extend(function(_y_x10354_0 /* maybe<5542> */ ) {
        return _mlift_trmc_flatmap_maybe_10454(_acc_0, f_1, xs.tail, _y_x10354_0);
      });
    }
    else {
      if (x_0_10627 !== null) {
         
        var _trmc_x10132_0 = undefined;
         
        var _trmc_x10133_0 = $std_core_types.Cons(x_0_10627.value, _trmc_x10132_0);
        {
          // tail call
          var _x109 = $std_core_types._cctx_extend(_acc_0,_trmc_x10133_0,({obj: _trmc_x10133_0, field_name: "tail"}));
          xs = xs.tail;
          _acc_0 = _x109;
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
  }
  else {
    return $std_core_types._cctx_apply(_acc_0,($std_core_types.Nil));
  }
}}
 
 
// Concatenate the `Just` result elements from applying a function to all elements.
export function _trmcm_flatmap_maybe(xs_0, f_2, _accm_0) /* forall<a,b,e> (xs : list<a>, f : (a) -> e maybe<b>, (list<b>) -> list<b>) -> e list<b> */  { tailcall: while(1)
{
  if (xs_0 !== null) {
     
    var x_2_10630 = f_2(xs_0.head);
    if ($std_core_hnd._yielding()) {
      return $std_core_hnd.yield_extend(function(_y_x10359_0 /* maybe<5542> */ ) {
        return _mlift_trmcm_flatmap_maybe_10455(_accm_0, f_2, xs_0.tail, _y_x10359_0);
      });
    }
    else {
      if (x_2_10630 !== null) {
        {
          // tail call
          var _x112 = function(__at_accm_0110 /* (list<5542>) -> list<5542> */ , _y_2111 /* 5542 */ ) {
            return function(_trmc_x10135_0 /* list<5542> */ ) {
              return __at_accm_0110($std_core_types.Cons(_y_2111, _trmc_x10135_0));
            };
          }(_accm_0, x_2_10630.value);
          xs_0 = xs_0.tail;
          _accm_0 = _x112;
          continue tailcall;
        }
      }
      else {
        {
          // tail call
          xs_0 = xs_0.tail;
          continue tailcall;
        }
      }
    }
  }
  else {
    return _accm_0($std_core_types.Nil);
  }
}}
 
 
// Concatenate the `Just` result elements from applying a function to all elements.
export function flatmap_maybe(xs_1, f_3) /* forall<a,b,e> (xs : list<a>, f : (a) -> e maybe<b>) -> e list<b> */  {
  var _x113 = $std_core_hnd._evv_is_affine();
  if (_x113) {
    return _trmc_flatmap_maybe(xs_1, f_3, $std_core_types._cctx_empty());
  }
  else {
    return _trmcm_flatmap_maybe(xs_1, f_3, function(_trmc_x10134 /* list<5542> */ ) {
        return _trmc_x10134;
      });
  }
}
 
 
// Concatenate a list of `:maybe` values
export function _trmc_concat_maybe(xs, _acc) /* forall<a> (xs : list<maybe<a>>, ctx<list<a>>) -> list<a> */  { tailcall: while(1)
{
  if (xs !== null) {
    if (xs.head !== null) {
       
      var _trmc_x10136 = undefined;
       
      var _trmc_x10137 = $std_core_types.Cons(xs.head.value, _trmc_x10136);
      {
        // tail call
        var _x114 = $std_core_types._cctx_extend(_acc,_trmc_x10137,({obj: _trmc_x10137, field_name: "tail"}));
        xs = xs.tail;
        _acc = _x114;
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
 
 
// Concatenate a list of `:maybe` values
export function concat_maybe(xs_0) /* forall<a> (xs : list<maybe<a>>) -> list<a> */  {
  return _trmc_concat_maybe(xs_0, $std_core_types._cctx_empty());
}
 
 
// Return the last element of a list (or `Nothing` for the empty list)
export function last(xs) /* forall<a> (xs : list<a>) -> maybe<a> */  { tailcall: while(1)
{
  if (xs !== null && xs.tail === null) {
    return $std_core_types.Just(xs.head);
  }
  else if (xs !== null) {
    {
      // tail call
      xs = xs.tail;
      continue tailcall;
    }
  }
  else {
    return $std_core_types.Nothing;
  }
}}
 
 
// Return the list without its last element.
// Return an empty list for an empty list.
export function _trmc_init(xs, _acc) /* forall<a> (xs : list<a>, ctx<list<a>>) -> list<a> */  { tailcall: while(1)
{
  if (xs !== null && xs.tail !== null) {
     
    var _trmc_x10138 = undefined;
     
    var _trmc_x10139 = $std_core_types.Cons(xs.head, _trmc_x10138);
    {
      // tail call
      var _x115 = $std_core_types._cctx_extend(_acc,_trmc_x10139,({obj: _trmc_x10139, field_name: "tail"}));
      xs = xs.tail;
      _acc = _x115;
      continue tailcall;
    }
  }
  else {
    return $std_core_types._cctx_apply(_acc,($std_core_types.Nil));
  }
}}
 
 
// Return the list without its last element.
// Return an empty list for an empty list.
export function init(xs_0) /* forall<a> (xs : list<a>) -> list<a> */  {
  return _trmc_init(xs_0, $std_core_types._cctx_empty());
}
 
 
// Get (zero-based) element `n`  of a list. Return a `:maybe` type.
export function _index(xs, n) /* forall<a> (xs : list<a>, n : int) -> maybe<a> */  { tailcall: while(1)
{
  if (xs !== null) {
    if ($std_core_types._int_gt(n,0)) {
      {
        // tail call
        var _x116 = $std_core_types._int_sub(n,1);
        xs = xs.tail;
        n = _x116;
        continue tailcall;
      }
    }
    else {
      if ($std_core_types._int_eq(n,0)) {
        return $std_core_types.Just(xs.head);
      }
      else {
        return $std_core_types.Nothing;
      }
    }
  }
  else {
    return $std_core_types.Nothing;
  }
}}
 
 
// monadic lift
export function _mlift_all_10456(predicate, xx, _y_x10367) /* forall<a,e> (predicate : (a) -> e bool, xx : list<a>, bool) -> e bool */  {
  if (_y_x10367) {
    return all(xx, predicate);
  }
  else {
    return false;
  }
}
 
 
// Do all elements satisfy a predicate ?
export function all(xs, predicate_0) /* forall<a,e> (xs : list<a>, predicate : (a) -> e bool) -> e bool */  { tailcall: while(1)
{
  if (xs !== null) {
     
    var x_0_10633 = predicate_0(xs.head);
    if ($std_core_hnd._yielding()) {
      return $std_core_hnd.yield_extend(function(_y_x10367_0 /* bool */ ) {
        return _mlift_all_10456(predicate_0, xs.tail, _y_x10367_0);
      });
    }
    else {
      if (x_0_10633) {
        {
          // tail call
          xs = xs.tail;
          continue tailcall;
        }
      }
      else {
        return false;
      }
    }
  }
  else {
    return true;
  }
}}
 
 
// monadic lift
export function _mlift_any_10457(predicate, xx, _y_x10371) /* forall<a,e> (predicate : (a) -> e bool, xx : list<a>, bool) -> e bool */  {
  if (_y_x10371) {
    return true;
  }
  else {
    return any(xx, predicate);
  }
}
 
 
// Are there any elements in a list that satisfy a predicate ?
export function any(xs, predicate_0) /* forall<a,e> (xs : list<a>, predicate : (a) -> e bool) -> e bool */  { tailcall: while(1)
{
  if (xs !== null) {
     
    var x_0_10636 = predicate_0(xs.head);
    if ($std_core_hnd._yielding()) {
      return $std_core_hnd.yield_extend(function(_y_x10371_0 /* bool */ ) {
        return _mlift_any_10457(predicate_0, xs.tail, _y_x10371_0);
      });
    }
    else {
      if (x_0_10636) {
        return true;
      }
      else {
        {
          // tail call
          xs = xs.tail;
          continue tailcall;
        }
      }
    }
  }
  else {
    return false;
  }
}}
 
 
// Return the sum of a list of integers
export function sum(xs) /* (xs : list<int>) -> int */  {
  return foldl(xs, 0, $std_core_int._lp__plus__rp_);
}
 
 
// Returns the smallest element of a list of integers (or `default` (=`0`) for the empty list)
export function minimum(xs, $default) /* (xs : list<int>, default : ? int) -> int */  {
  if (xs === null) {
    return ($default !== undefined) ? $default : 0;
  }
  else {
    return foldl(xs.tail, xs.head, $std_core_int.min);
  }
}
 
 
// Returns the largest element of a list of integers (or `default` (=`0`) for the empty list)
export function maximum(xs, $default) /* (xs : list<int>, default : ? int) -> int */  {
  if (xs === null) {
    return ($default !== undefined) ? $default : 0;
  }
  else {
    return foldl(xs.tail, xs.head, $std_core_int.max);
  }
}
 
 
// Split a string into a list of lines
export function lines(s) /* (s : string) -> list<string> */  {
   
  var v_10011 = ((s).split(("\n")));
  return $std_core_vector.vlist(v_10011);
}
 
 
// Join a list of strings with newlines
export function unlines(xs) /* (xs : list<string>) -> string */  {
  if (xs === null) {
    return "";
  }
  if (xs !== null && xs.tail === null) {
    return xs.head;
  }
  if (xs !== null && xs.tail !== null && xs.tail.tail === null) {
    if ((("\n") === (""))){
      return $std_core_types._lp__plus__plus__rp_(xs.head, xs.tail.head);
    }
  }
  return ((($std_core_vector.unvlist(xs))).join(("\n")));
}