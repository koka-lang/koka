// Koka generated module: std/num/ddouble, koka version: 3.2.4
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
import * as $std_num_float64 from './std_num_float64.mjs';
import * as $std_num_decimal from './std_num_decimal.mjs';
import * as $std_text_parse from './std_text_parse.mjs';
 
// externals
 
// type declarations
// type ddouble
export function Ddouble(hi, lo) /* (hi : float64, lo : float64) -> ddouble */  {
  return { hi: hi, lo: lo };
}
// type edouble
export function Edouble(num, err) /* (num : float64, err : float64) -> edouble */  {
  return { num: num, err: err };
}
 
// declarations
 
 
// Automatically generated. Retrieves the `hi` constructor field of the `:ddouble` type.
export function ddouble_fs_hi(ddouble_0) /* (ddouble : ddouble) -> float64 */  {
  return ddouble_0.hi;
}
 
 
// Automatically generated. Retrieves the `lo` constructor field of the `:ddouble` type.
export function ddouble_fs_lo(ddouble_0) /* (ddouble : ddouble) -> float64 */  {
  return ddouble_0.lo;
}
 
export function ddouble_fs__copy(_this, hi, lo) /* (ddouble, hi : ? float64, lo : ? float64) -> ddouble */  {
  if (hi !== undefined) {
    var _x0 = hi;
  }
  else {
    var _x0 = _this.hi;
  }
  if (lo !== undefined) {
    var _x1 = lo;
  }
  else {
    var _x1 = _this.lo;
  }
  return Ddouble(_x0, _x1);
}
 
 
// Create a `:ddouble` from a `:float64`.
export function float64_fs_ddouble(d) /* (d : float64) -> ddouble */  {
  return Ddouble(d, 0.0);
}
 
export var maxprecise;
var maxprecise = $std_core_types._int_const(9007199254740991n);
 
 
// Compare two `:ddouble` values.
export function cmp(x, y) /* (x : ddouble, y : ddouble) -> order */  {
  var _x3 = x.hi;
  var _x4 = y.hi;
  var _x2 = $std_num_float64.cmp(_x3, _x4);
  if (_x2 === 2) {
    var _x5 = x.lo;
    var _x6 = y.lo;
    return $std_num_float64.cmp(_x5, _x6);
  }
  else {
    return _x2;
  }
}
 
export function _lp__excl__eq__rp_(x, y) /* (x : ddouble, y : ddouble) -> bool */  {
  var _x9 = x.hi;
  var _x10 = y.hi;
  var _x8 = $std_num_float64.cmp(_x9, _x10);
  if (_x8 === 2) {
    var _x11 = x.lo;
    var _x12 = y.lo;
    var _x7 = $std_num_float64.cmp(_x11, _x12);
  }
  else {
    var _x7 = _x8;
  }
  return $std_core_order._lp__excl__eq__rp_(_x7, $std_core_types.Eq);
}
 
export function _lp__gt__eq__rp_(x, y) /* (x : ddouble, y : ddouble) -> bool */  {
  var _x15 = x.hi;
  var _x16 = y.hi;
  var _x14 = $std_num_float64.cmp(_x15, _x16);
  if (_x14 === 2) {
    var _x17 = x.lo;
    var _x18 = y.lo;
    var _x13 = $std_num_float64.cmp(_x17, _x18);
  }
  else {
    var _x13 = _x14;
  }
  return $std_core_order._lp__excl__eq__rp_(_x13, $std_core_types.Lt);
}
 
export function _lp__lt__eq__rp_(x, y) /* (x : ddouble, y : ddouble) -> bool */  {
  var _x21 = x.hi;
  var _x22 = y.hi;
  var _x20 = $std_num_float64.cmp(_x21, _x22);
  if (_x20 === 2) {
    var _x23 = x.lo;
    var _x24 = y.lo;
    var _x19 = $std_num_float64.cmp(_x23, _x24);
  }
  else {
    var _x19 = _x20;
  }
  return $std_core_order._lp__excl__eq__rp_(_x19, $std_core_types.Gt);
}
 
 
// Negate a `:ddouble`.
export function _lp__tilde__rp_(x) /* (x : ddouble) -> ddouble */  {
  var _x25 = x.hi;
  var _x26 = x.lo;
  return Ddouble((-_x25), (-_x26));
}
 
export var minprecise;
var minprecise = $std_core_types._int_negate(($std_core_types._int_const(9007199254740991n)));
 
export function is_precise(i) /* (i : int) -> bool */  {
  return ($std_core_types._int_ge(i,minprecise)) ? $std_core_types._int_le(i,($std_core_types._int_const(9007199254740991n))) : false;
}
 
 
// Not-A-Number
export var dd_nan;
var dd_nan = Ddouble($std_num_float64.nan, 0.0);
 
 
// Is this `:ddouble` equal to is-zero
export function is_zero(x) /* (x : ddouble) -> bool */  {
  var _x27 = x.hi;
  return (_x27 === (0.0));
}
 
 
// Is this `:ddouble` negative?
export function is_neg(x) /* (x : ddouble) -> bool */  {
  var _x28 = x.hi;
  return (_x28 < (0.0));
}
 
 
// Is this a finite `:ddouble`? (i.e. not `is-nan` or `is-inf`)
export function is_finite(x) /* (x : ddouble) -> bool */  {
  var _x30 = x.hi;
  var _x29 = isFinite(_x30);
  if (_x29) {
    var _x31 = x.lo;
    return isFinite(_x31);
  }
  else {
    return false;
  }
}
 
export function _lp__eq__eq__rp_(x, y) /* (x : ddouble, y : ddouble) -> bool */  {
  var _x34 = x.hi;
  var _x35 = y.hi;
  var _x33 = $std_num_float64.cmp(_x34, _x35);
  if (_x33 === 2) {
    var _x36 = x.lo;
    var _x37 = y.lo;
    var _x32 = $std_num_float64.cmp(_x36, _x37);
  }
  else {
    var _x32 = _x33;
  }
  return $std_core_order._lp__eq__eq__rp_(_x32, $std_core_types.Eq);
}
 
 
// Automatically generated. Retrieves the `num` constructor field of the `:edouble` type.
export function edouble_fs_num(edouble) /* (edouble : edouble) -> float64 */  {
  return edouble.num;
}
 
 
// Automatically generated. Retrieves the `err` constructor field of the `:edouble` type.
export function edouble_fs_err(edouble) /* (edouble : edouble) -> float64 */  {
  return edouble.err;
}
 
 
// Return the absolute value.
export function abs(x) /* (x : ddouble) -> ddouble */  {
  var _x39 = x.hi;
  var _x38 = (_x39 < (0.0));
  if (_x38) {
    var _x40 = x.hi;
    var _x41 = x.lo;
    return Ddouble((-_x40), (-_x41));
  }
  else {
    return x;
  }
}
 
 
// Convert a `:ddouble` to a `:float64` (losing precision)
export function float64(x) /* (x : ddouble) -> float64 */  {
  return x.hi;
}
 
export function dquicksum(x, y) /* (x : float64, y : float64) -> ddouble */  {
   
  var b_10018 = isFinite(x);
  if (b_10018) {
     
    var z = (x + y);
     
    var err = (y - ((z - x)));
    var _x42 = (isFinite(z)) ? err : z;
    return Ddouble(z, _x42);
  }
  else {
    return Ddouble(x, 0.0);
  }
}
 
 
// often called `twoproduct` in literature (see [@shewchuk])
export function prod(x, y) /* (x : float64, y : float64) -> edouble */  {
   
  var z = (x * y);
   
  var err = $std_num_float64.fmadd(x, y, (-z));
  return Edouble(z, err);
}
 
 
// Multiply two `:ddouble`s
export function _lp__star__rp_(x, y) /* (x : ddouble, y : ddouble) -> ddouble */  {
   
  var _x43 = x.hi;
  var _x44 = y.hi;
  var z = (_x43 * _x44);
   
  var _x45 = x.hi;
  var _x46 = y.hi;
  var err = $std_num_float64.fmadd(_x45, _x46, (-z));
   
  var z_0 = Edouble(z, err);
   
  var _x47 = z_0.err;
  var _x48 = x.hi;
  var _x49 = y.lo;
  var _x50 = x.lo;
  var _x51 = y.hi;
  var e = (_x47 + ((((_x48 * _x49)) + ((_x50 * _x51)))));
  var _x43 = z_0.num;
  return dquicksum(_x43, e);
}
 
 
// As `sum` but with `x.abs >= y.abs`
export function quicksum(x, y) /* (x : float64, y : float64) -> edouble */  {
   
  var z = (x + y);
   
  var err = (y - ((z - x)));
  var _x44 = (isFinite(z)) ? err : z;
  return Edouble(z, _x44);
}
 
 
// often called `twosum` in literature (see [@shewchuk])
export function sum(x, y) /* (x : float64, y : float64) -> edouble */  {
   
  var z = (x + y);
   
  var diff = (z - x);
   
  var err = (((x - ((z - diff)))) + ((y - diff)));
  var _x45 = (isFinite(z)) ? err : z;
  return Edouble(z, _x45);
}
 
 
// Add two `:ddouble`s
export function _lp__plus__rp_(x, y) /* (x : ddouble, y : ddouble) -> ddouble */  {
   
  var _x46 = x.hi;
  var _x47 = y.hi;
  var z1 = sum(_x46, _x47);
   
  var _x48 = x.lo;
  var _x49 = y.lo;
  var lo = sum(_x48, _x49);
   
  var _x50 = z1.err;
  var _x51 = lo.num;
  var e1 = (_x50 + _x51);
   
  var _x52 = z1.num;
  var z = (_x52 + e1);
   
  var _x53 = z1.num;
  var err = (e1 - ((z - _x53)));
   
  var _x54 = (isFinite(z)) ? err : z;
  var z2 = Edouble(z, _x54);
   
  var _x55 = z2.err;
  var _x56 = lo.err;
  var e2 = (_x55 + _x56);
  var _x46 = z2.num;
  return dquicksum(_x46, e2);
}
 
 
// Subtract two values.
export function _lp__dash__rp_(x, y) /* (x : ddouble, y : ddouble) -> ddouble */  {
  var _x47 = y.hi;
  var _x48 = y.lo;
  return _lp__plus__rp_(x, Ddouble((-_x47), (-_x48)));
}
 
 
// Divide two `:ddouble`s
export function _lp__fs__rp_(x, y) /* (x : ddouble, y : ddouble) -> ddouble */  {
   
  var _x49 = x.hi;
  var _x50 = y.hi;
  var d_10042 = (_x49 / _x50);
   
  var q1 = Ddouble(d_10042, 0.0);
   
  var _x52 = q1.hi;
  var _x51 = isFinite(_x52);
  if (_x51) {
    var _x53 = q1.lo;
    var b_10045 = isFinite(_x53);
  }
  else {
    var b_10045 = false;
  }
  if (b_10045) {
     
    var _x49 = y.hi;
    var b_0_10046 = isFinite(_x49);
    if (b_0_10046) {
       
      var y_0_10049 = _lp__star__rp_(y, q1);
       
      var _x49 = y_0_10049.hi;
      var _x50 = y_0_10049.lo;
      var r1 = _lp__plus__rp_(x, Ddouble((-_x49), (-_x50)));
       
      var _x51 = r1.hi;
      var _x52 = y.hi;
      var d_0_10050 = (_x51 / _x52);
       
      var q2 = Ddouble(d_0_10050, 0.0);
       
      var y_1_10054 = _lp__star__rp_(y, q2);
       
      var _x53 = y_1_10054.hi;
      var _x54 = y_1_10054.lo;
      var r2 = _lp__plus__rp_(r1, Ddouble((-_x53), (-_x54)));
       
      var _x55 = r2.hi;
      var _x56 = y.hi;
      var d_1_10055 = (_x55 / _x56);
       
      var _x57 = q1.hi;
      var _x58 = q2.hi;
      var q = dquicksum(_x57, _x58);
      return _lp__plus__rp_(q, Ddouble(d_1_10055, 0.0));
    }
    else {
      return q1;
    }
  }
  else {
    return q1;
  }
}
 
export function dsum(x, y) /* (x : float64, y : float64) -> ddouble */  {
   
  var z = (x + y);
   
  var diff = (z - x);
   
  var err = (((x - ((z - diff)))) + ((y - diff)));
  var _x49 = (isFinite(z)) ? err : z;
  return Ddouble(z, _x49);
}
 
 
// Create a `:ddouble` as the sum of two `:float64`'s.
export function ddouble(x, y) /* (x : float64, y : float64) -> ddouble */  {
  if ((y === (0.0))) {
    return Ddouble(x, 0.0);
  }
  else {
    return dsum(x, y);
  }
}
 
export function prodsqr(x) /* (x : float64) -> edouble */  {
   
  var z = (x * x);
   
  var err = $std_num_float64.fmadd(x, x, (-z));
  return Edouble(z, err);
}
 
 
// Multiply `x` with itself.
export function sqr(x) /* (x : ddouble) -> ddouble */  {
   
  var _x50 = x.hi;
  var _x51 = x.hi;
  var z = (_x50 * _x51);
   
  var _x52 = x.hi;
  var _x53 = x.hi;
  var err = $std_num_float64.fmadd(_x52, _x53, (-z));
   
  var z_0 = Edouble(z, err);
   
  var _x54 = z_0.err;
  var _x55 = x.hi;
  var _x56 = x.lo;
  var _x57 = x.lo;
  var _x58 = x.lo;
  var e = (((_x54 + (((((2.0) * _x55)) * _x56)))) + ((_x57 * _x58)));
  var _x50 = z_0.num;
  return dquicksum(_x50, e);
}
 
export function npwr_acc(x, acc, n) /* (x : ddouble, acc : ddouble, n : int) -> ddouble */  { tailcall: while(1)
{
  if ($std_core_types._int_le(n,0)) {
    return acc;
  }
  else {
     
    var b_10014 = $std_core_types._int_isodd(n);
    if (b_10014) {
      {
        // tail call
        var _x51 = _lp__star__rp_(x, acc);
        var _x52 = $std_core_types._int_sub(n,1);
        acc = _x51;
        n = _x52;
        continue tailcall;
      }
    }
    else {
      {
        // tail call
        var _x53 = sqr(x);
        var _x54 = $std_core_types._int_div(n,2);
        x = _x53;
        n = _x54;
        continue tailcall;
      }
    }
  }
}}
 
 
// One
export var one;
var one = Ddouble(1.0, 0.0);
 
export function npwr(x, n) /* (x : ddouble, n : int) -> ddouble */  {
  if ($std_core_types._int_eq(n,0)) {
    var _x56 = x.hi;
    var _x55 = (_x56 === (0.0));
    return (_x55) ? dd_nan : one;
  }
  else {
    if ($std_core_types._int_eq(n,1)) {
      return x;
    }
    else {
      return npwr_acc(x, one, n);
    }
  }
}
 
 
// Return `x` to the power of `n`.
export function powi(x, n) /* (x : ddouble, n : int) -> ddouble */  {
   
  var n_0_10594 = $std_core_types._int_abs(n);
   
  if ($std_core_types._int_eq(n_0_10594,0)) {
    var _x58 = x.hi;
    var _x57 = (_x58 === (0.0));
    var p = (_x57) ? dd_nan : one;
  }
  else {
    if ($std_core_types._int_eq(n_0_10594,1)) {
      var p = x;
    }
    else {
      var p = npwr_acc(x, one, n_0_10594);
    }
  }
  if ($std_core_types._int_lt(n,0)) {
    return _lp__fs__rp_(one, p);
  }
  else {
    return p;
  }
}
 
 
// Ten (`10.ddouble`)
export var ten;
var ten = Ddouble(10.0, 0.0);
 
 
// Return 10 to the power of `exp`.
export function powi10(exp_0) /* (exp : int) -> ddouble */  {
  return powi(ten, exp_0);
}
 
export function mul_exp10(x, exp_0) /* (x : ddouble, exp : int) -> ddouble */  {
  if ($std_core_types._int_iszero(exp_0)) {
    return x;
  }
  else {
    return _lp__star__rp_(x, powi(ten, exp_0));
  }
}
 
export function small_exp(i, e) /* (i : int, e : int) -> ddouble */  {
   
  var d_10079 = $std_core_types._int_to_double(i);
   
  var dd = Ddouble(d_10079, 0.0);
  if ($std_core_types._int_iszero(e)) {
    return dd;
  }
  else {
    if ($std_core_types._int_iszero(e)) {
      return dd;
    }
    else {
      return _lp__star__rp_(dd, powi(ten, e));
    }
  }
}
 
export function ddouble_int_exp(i, e) /* (i : int, e : int) -> ddouble */  {
  if ($std_core_types._int_ge(i,minprecise)) {
    if ($std_core_types._int_le(i,($std_core_types._int_const(9007199254740991n)))) {
      return small_exp(i, e);
    }
    else {
       
      var p = $std_core_int.count_digits(i);
       
      var px = $std_core_types._int_sub(p,14);
      var _x57 = $std_core_int.cdivmod_exp10(i, px);
       
      var py = $std_core_types._int_sub(px,14);
      if ($std_core_types._int_le(py,0)) {
         
        var e_0_10086 = $std_core_types._int_add(px,e);
         
        var d_10089 = $std_core_types._int_to_double((_x57.fst));
         
        var dd = Ddouble(d_10089, 0.0);
        if ($std_core_types._int_iszero(e_0_10086)) {
          var _x58 = dd;
        }
        else {
          if ($std_core_types._int_iszero(e_0_10086)) {
            var _x58 = dd;
          }
          else {
            var _x58 = _lp__star__rp_(dd, powi(ten, e_0_10086));
          }
        }
        return _lp__plus__rp_(_x58, small_exp(_x57.snd, e));
      }
      else {
        var _x59 = $std_core_int.cdivmod_exp10(_x57.snd, py);
         
        var pz = $std_core_types._int_sub(py,14);
        if ($std_core_types._int_le(pz,0)) {
           
          var e_1_10093 = $std_core_types._int_add(px,e);
           
          var d_0_10096 = $std_core_types._int_to_double((_x57.fst));
           
          var dd_0 = Ddouble(d_0_10096, 0.0);
           
          var e_2_10098 = $std_core_types._int_add(py,e);
           
          var d_1_10101 = $std_core_types._int_to_double((_x59.fst));
           
          var dd_1 = Ddouble(d_1_10101, 0.0);
           
          var e_3_10103 = $std_core_types._int_add(0,e);
           
          var d_2_10106 = $std_core_types._int_to_double((_x59.snd));
           
          var dd_2 = Ddouble(d_2_10106, 0.0);
          if ($std_core_types._int_iszero(e_1_10093)) {
            var _x60 = dd_0;
          }
          else {
            if ($std_core_types._int_iszero(e_1_10093)) {
              var _x60 = dd_0;
            }
            else {
              var _x60 = _lp__star__rp_(dd_0, powi(ten, e_1_10093));
            }
          }
          if ($std_core_types._int_iszero(e_2_10098)) {
            var _x61 = dd_1;
          }
          else {
            if ($std_core_types._int_iszero(e_2_10098)) {
              var _x61 = dd_1;
            }
            else {
              var _x61 = _lp__star__rp_(dd_1, powi(ten, e_2_10098));
            }
          }
          if ($std_core_types._int_iszero(e_3_10103)) {
            var _x62 = dd_2;
          }
          else {
            if ($std_core_types._int_iszero(e_3_10103)) {
              var _x62 = dd_2;
            }
            else {
              var _x62 = _lp__star__rp_(dd_2, powi(ten, e_3_10103));
            }
          }
          return _lp__plus__rp_(_x60, _lp__plus__rp_(_x61, _x62));
        }
        else {
           
          var lo = $std_core_int.cdiv_exp10(_x59.snd, pz);
           
          var e_4_10108 = $std_core_types._int_add(px,e);
           
          var d_3_10111 = $std_core_types._int_to_double((_x57.fst));
           
          var dd_3 = Ddouble(d_3_10111, 0.0);
           
          var e_5_10113 = $std_core_types._int_add(py,e);
           
          var d_4_10116 = $std_core_types._int_to_double((_x59.fst));
           
          var dd_4 = Ddouble(d_4_10116, 0.0);
           
          var e_6_10118 = $std_core_types._int_add(pz,e);
           
          var d_5_10121 = $std_core_types._int_to_double(lo);
           
          var dd_5 = Ddouble(d_5_10121, 0.0);
          if ($std_core_types._int_iszero(e_4_10108)) {
            var _x63 = dd_3;
          }
          else {
            if ($std_core_types._int_iszero(e_4_10108)) {
              var _x63 = dd_3;
            }
            else {
              var _x63 = _lp__star__rp_(dd_3, powi(ten, e_4_10108));
            }
          }
          if ($std_core_types._int_iszero(e_5_10113)) {
            var _x64 = dd_4;
          }
          else {
            if ($std_core_types._int_iszero(e_5_10113)) {
              var _x64 = dd_4;
            }
            else {
              var _x64 = _lp__star__rp_(dd_4, powi(ten, e_5_10113));
            }
          }
          if ($std_core_types._int_iszero(e_6_10118)) {
            var _x65 = dd_5;
          }
          else {
            if ($std_core_types._int_iszero(e_6_10118)) {
              var _x65 = dd_5;
            }
            else {
              var _x65 = _lp__star__rp_(dd_5, powi(ten, e_6_10118));
            }
          }
          return _lp__plus__rp_(_x63, _lp__plus__rp_(_x64, _x65));
        }
      }
    }
  }
  else {
     
    var p_0 = $std_core_int.count_digits(i);
     
    var px_0 = $std_core_types._int_sub(p_0,14);
    var _x66 = $std_core_int.cdivmod_exp10(i, px_0);
     
    var py_0 = $std_core_types._int_sub(px_0,14);
    if ($std_core_types._int_le(py_0,0)) {
       
      var e_0_10086_0 = $std_core_types._int_add(px_0,e);
       
      var d_10089_0 = $std_core_types._int_to_double((_x66.fst));
       
      var dd_6 = Ddouble(d_10089_0, 0.0);
      if ($std_core_types._int_iszero(e_0_10086_0)) {
        var _x67 = dd_6;
      }
      else {
        if ($std_core_types._int_iszero(e_0_10086_0)) {
          var _x67 = dd_6;
        }
        else {
          var _x67 = _lp__star__rp_(dd_6, powi(ten, e_0_10086_0));
        }
      }
      return _lp__plus__rp_(_x67, small_exp(_x66.snd, e));
    }
    else {
      var _x68 = $std_core_int.cdivmod_exp10(_x66.snd, py_0);
       
      var pz_0 = $std_core_types._int_sub(py_0,14);
      if ($std_core_types._int_le(pz_0,0)) {
         
        var e_1_10093_0 = $std_core_types._int_add(px_0,e);
         
        var d_0_10096_0 = $std_core_types._int_to_double((_x66.fst));
         
        var dd_0_0 = Ddouble(d_0_10096_0, 0.0);
         
        var e_2_10098_0 = $std_core_types._int_add(py_0,e);
         
        var d_1_10101_0 = $std_core_types._int_to_double((_x68.fst));
         
        var dd_1_0 = Ddouble(d_1_10101_0, 0.0);
         
        var e_3_10103_0 = $std_core_types._int_add(0,e);
         
        var d_2_10106_0 = $std_core_types._int_to_double((_x68.snd));
         
        var dd_2_0 = Ddouble(d_2_10106_0, 0.0);
        if ($std_core_types._int_iszero(e_1_10093_0)) {
          var _x69 = dd_0_0;
        }
        else {
          if ($std_core_types._int_iszero(e_1_10093_0)) {
            var _x69 = dd_0_0;
          }
          else {
            var _x69 = _lp__star__rp_(dd_0_0, powi(ten, e_1_10093_0));
          }
        }
        if ($std_core_types._int_iszero(e_2_10098_0)) {
          var _x70 = dd_1_0;
        }
        else {
          if ($std_core_types._int_iszero(e_2_10098_0)) {
            var _x70 = dd_1_0;
          }
          else {
            var _x70 = _lp__star__rp_(dd_1_0, powi(ten, e_2_10098_0));
          }
        }
        if ($std_core_types._int_iszero(e_3_10103_0)) {
          var _x71 = dd_2_0;
        }
        else {
          if ($std_core_types._int_iszero(e_3_10103_0)) {
            var _x71 = dd_2_0;
          }
          else {
            var _x71 = _lp__star__rp_(dd_2_0, powi(ten, e_3_10103_0));
          }
        }
        return _lp__plus__rp_(_x69, _lp__plus__rp_(_x70, _x71));
      }
      else {
         
        var lo_0 = $std_core_int.cdiv_exp10(_x68.snd, pz_0);
         
        var e_4_10108_0 = $std_core_types._int_add(px_0,e);
         
        var d_3_10111_0 = $std_core_types._int_to_double((_x66.fst));
         
        var dd_3_0 = Ddouble(d_3_10111_0, 0.0);
         
        var e_5_10113_0 = $std_core_types._int_add(py_0,e);
         
        var d_4_10116_0 = $std_core_types._int_to_double((_x68.fst));
         
        var dd_4_0 = Ddouble(d_4_10116_0, 0.0);
         
        var e_6_10118_0 = $std_core_types._int_add(pz_0,e);
         
        var d_5_10121_0 = $std_core_types._int_to_double(lo_0);
         
        var dd_5_0 = Ddouble(d_5_10121_0, 0.0);
        if ($std_core_types._int_iszero(e_4_10108_0)) {
          var _x72 = dd_3_0;
        }
        else {
          if ($std_core_types._int_iszero(e_4_10108_0)) {
            var _x72 = dd_3_0;
          }
          else {
            var _x72 = _lp__star__rp_(dd_3_0, powi(ten, e_4_10108_0));
          }
        }
        if ($std_core_types._int_iszero(e_5_10113_0)) {
          var _x73 = dd_4_0;
        }
        else {
          if ($std_core_types._int_iszero(e_5_10113_0)) {
            var _x73 = dd_4_0;
          }
          else {
            var _x73 = _lp__star__rp_(dd_4_0, powi(ten, e_5_10113_0));
          }
        }
        if ($std_core_types._int_iszero(e_6_10118_0)) {
          var _x74 = dd_5_0;
        }
        else {
          if ($std_core_types._int_iszero(e_6_10118_0)) {
            var _x74 = dd_5_0;
          }
          else {
            var _x74 = _lp__star__rp_(dd_5_0, powi(ten, e_6_10118_0));
          }
        }
        return _lp__plus__rp_(_x72, _lp__plus__rp_(_x73, _x74));
      }
    }
  }
}
 
 
// Create a `:ddouble` from an `:int`.
// A `:ddouble` can represent integers precisely up to 30 digits.
// If an integer is passed that is out of range an infinity is returned.
export function int_fs_ddouble(i) /* (i : int) -> ddouble */  {
  return ddouble_int_exp(i, 0);
}
 
 
// monadic lift
export function _mlift_pddouble_normal_10797(wild___0) /* (wild_@0 : char) -> std/text/parse/parse int */  {
  return $std_text_parse.pint();
}
 
 
// monadic lift
export function _mlift_pddouble_normal_10798(frac, neg, whole, exp_0) /* (frac : string, neg : bool, whole : string, exp@0 : int) -> std/text/parse/parse ddouble */  {
   
  var _x75 = (neg) ? "-" : "";
  var _x_x1_0_10790 = $std_core_types._lp__plus__plus__rp_(_x75, $std_core_types._lp__plus__plus__rp_(whole, frac));
   
  var _x_x1_10788 = $std_core_hnd._open_none2(function(s /* string */ , hex /* ? bool */ ) {
      var _x76 = (hex !== undefined) ? hex : false;
      return $std_core_int.xparse(s, _x76);
    }, _x_x1_0_10790);
   
  var w = $std_core_hnd._open_none2(function(m /* maybe<int> */ , nothing /* int */ ) {
      return (m === null) ? nothing : m.value;
    }, _x_x1_10788, 0);
   
  var y_10781 = $std_core_string.chars_fs_count(frac);
   
  var e = $std_core_types._int_sub(exp_0,y_10781);
  return $std_core_hnd._open_none2(ddouble_int_exp, w, e);
}
 
 
// monadic lift
export function _mlift_pddouble_normal_10799(wild__) /* (wild_ : char) -> std/text/parse/parse string */  {
  return $std_text_parse.digits();
}
 
 
// monadic lift
export function _mlift_pddouble_normal_10800(neg, whole, _y_x10767) /* (neg : bool, whole : string, string) -> std/text/parse/parse ddouble */  {
   
  var frac = $std_core_sslice.trim_right(_y_x10767, "0");
   
  var x_10808 = $std_text_parse._lp__bar__bar__rp_(function() {
       
      var x_0_10810 = $std_text_parse.char(0x0065);
      if ($std_core_hnd._yielding()) {
        return $std_core_hnd.yield_extend(_mlift_pddouble_normal_10797);
      }
      else {
        return $std_text_parse.pint();
      }
    }, function() {
      return 0;
    });
  if ($std_core_hnd._yielding()) {
    return $std_core_hnd.yield_extend(function(exp_0 /* int */ ) {
      return _mlift_pddouble_normal_10798(frac, neg, whole, exp_0);
    });
  }
  else {
    return _mlift_pddouble_normal_10798(frac, neg, whole, x_10808);
  }
}
 
 
// monadic lift
export function _mlift_pddouble_normal_10801(neg, whole) /* (neg : bool, whole : string) -> std/text/parse/parse ddouble */  {
   
  var x_10812 = $std_text_parse._lp__bar__bar__rp_(function() {
       
      var x_0_10814 = $std_text_parse.char(0x002E);
      if ($std_core_hnd._yielding()) {
        return $std_core_hnd.yield_extend(_mlift_pddouble_normal_10799);
      }
      else {
        return $std_text_parse.digits();
      }
    }, function() {
      return "";
    });
  if ($std_core_hnd._yielding()) {
    return $std_core_hnd.yield_extend(function(_y_x10767 /* string */ ) {
      return _mlift_pddouble_normal_10800(neg, whole, _y_x10767);
    });
  }
  else {
    return _mlift_pddouble_normal_10800(neg, whole, x_10812);
  }
}
 
 
// monadic lift
export function _mlift_pddouble_normal_10802(neg) /* (neg : bool) -> std/text/parse/parse ddouble */  {
   
  var x_10816 = $std_text_parse.digits();
  if ($std_core_hnd._yielding()) {
    return $std_core_hnd.yield_extend(function(whole /* string */ ) {
      return _mlift_pddouble_normal_10801(neg, whole);
    });
  }
  else {
    return _mlift_pddouble_normal_10801(neg, x_10816);
  }
}
 
export function pddouble_normal() /* () -> std/text/parse/parse ddouble */  {
   
  var x_10818 = $std_text_parse.sign();
  if ($std_core_hnd._yielding()) {
    return $std_core_hnd.yield_extend(_mlift_pddouble_normal_10802);
  }
  else {
     
    var x_0_10821 = $std_text_parse.digits();
    if ($std_core_hnd._yielding()) {
      return $std_core_hnd.yield_extend(function(whole /* string */ ) {
        return _mlift_pddouble_normal_10801(x_10818, whole);
      });
    }
    else {
       
      var x_1_10824 = $std_text_parse._lp__bar__bar__rp_(function() {
           
          var x_2_10827 = $std_text_parse.char(0x002E);
          if ($std_core_hnd._yielding()) {
            return $std_core_hnd.yield_extend(_mlift_pddouble_normal_10799);
          }
          else {
            return $std_text_parse.digits();
          }
        }, function() {
          return "";
        });
      if ($std_core_hnd._yielding()) {
        return $std_core_hnd.yield_extend(function(_y_x10767 /* string */ ) {
          return _mlift_pddouble_normal_10800(x_10818, x_0_10821, _y_x10767);
        });
      }
      else {
         
        var frac = $std_core_sslice.trim_right(x_1_10824, "0");
         
        var x_3_10829 = $std_text_parse._lp__bar__bar__rp_(function() {
             
            var x_4_10832 = $std_text_parse.char(0x0065);
            if ($std_core_hnd._yielding()) {
              return $std_core_hnd.yield_extend(_mlift_pddouble_normal_10797);
            }
            else {
              return $std_text_parse.pint();
            }
          }, function() {
            return 0;
          });
        if ($std_core_hnd._yielding()) {
          return $std_core_hnd.yield_extend(function(exp_0 /* int */ ) {
            return _mlift_pddouble_normal_10798(frac, x_10818, x_0_10821, exp_0);
          });
        }
        else {
           
          var _x75 = (x_10818) ? "-" : "";
          var _x_x1_0_10790 = $std_core_types._lp__plus__plus__rp_(_x75, $std_core_types._lp__plus__plus__rp_(x_0_10821, frac));
           
          var _x_x1_10788 = $std_core_hnd._open_none2(function(s /* string */ , hex /* ? bool */ ) {
              var _x76 = (hex !== undefined) ? hex : false;
              return $std_core_int.xparse(s, _x76);
            }, _x_x1_0_10790);
           
          var w = $std_core_hnd._open_none2(function(m /* maybe<int> */ , nothing /* int */ ) {
              return (m === null) ? nothing : m.value;
            }, _x_x1_10788, 0);
           
          var y_10781 = $std_core_string.chars_fs_count(frac);
           
          var e = $std_core_types._int_sub(x_3_10829,y_10781);
          return $std_core_hnd._open_none2(ddouble_int_exp, w, e);
        }
      }
    }
  }
}
 
 
// monadic lift
export function _mlift_pddouble_sum_10803(hi, lo) /* (hi : float64, lo : float64) -> std/text/parse/parse ddouble */  {
  return $std_core_hnd._open_none2(_lp__plus__rp_, Ddouble(hi, 0.0), Ddouble(lo, 0.0));
}
 
 
// monadic lift
export function _mlift_pddouble_sum_10804(hi, wild___0) /* (hi : float64, wild_@0 : string) -> std/text/parse/parse ddouble */  {
   
  var x_10834 = $std_num_float64.pdouble();
   
  function next_10835(lo) /* (float64) -> std/text/parse/parse ddouble */  {
    return $std_core_hnd._open_none2(_lp__plus__rp_, Ddouble(hi, 0.0), Ddouble(lo, 0.0));
  }
  if ($std_core_hnd._yielding()) {
    return $std_core_hnd.yield_extend(next_10835);
  }
  else {
    return next_10835(x_10834);
  }
}
 
 
// monadic lift
export function _mlift_pddouble_sum_10805(hi) /* (hi : float64) -> std/text/parse/parse ddouble */  {
   
  var _x_x1_10794 = isFinite(hi);
  var _x75 = $std_core_hnd._open_none1(function(b /* bool */ ) {
      return (b) ? false : true;
    }, _x_x1_10794);
  if (_x75) {
    return Ddouble(hi, 0.0);
  }
  else {
     
    var x_10838 = $std_text_parse.pstring(" + ");
    if ($std_core_hnd._yielding()) {
      return $std_core_hnd.yield_extend(function(wild___0 /* string */ ) {
        return _mlift_pddouble_sum_10804(hi, wild___0);
      });
    }
    else {
      return _mlift_pddouble_sum_10804(hi, x_10838);
    }
  }
}
 
export function pddouble_sum() /* () -> std/text/parse/parse ddouble */  {
   
  var x_10840 = $std_num_float64.pdouble();
  if ($std_core_hnd._yielding()) {
    return $std_core_hnd.yield_extend(_mlift_pddouble_sum_10805);
  }
  else {
     
    var _x_x1_10794 = isFinite(x_10840);
    var _x76 = $std_core_hnd._open_none1(function(b /* bool */ ) {
        return (b) ? false : true;
      }, _x_x1_10794);
    if (_x76) {
      return Ddouble(x_10840, 0.0);
    }
    else {
       
      var x_0_10843 = $std_text_parse.pstring(" + ");
      if ($std_core_hnd._yielding()) {
        return $std_core_hnd.yield_extend(function(wild___0 /* string */ ) {
          return _mlift_pddouble_sum_10804(x_10840, wild___0);
        });
      }
      else {
         
        var x_1_10846 = $std_num_float64.pdouble();
        if ($std_core_hnd._yielding()) {
          return $std_core_hnd.yield_extend(function(lo /* float64 */ ) {
            return $std_core_hnd._open_none2(_lp__plus__rp_, Ddouble(x_10840, 0.0), Ddouble(lo, 0.0));
          });
        }
        else {
          return $std_core_hnd._open_none2(_lp__plus__rp_, Ddouble(x_10840, 0.0), Ddouble(x_1_10846, 0.0));
        }
      }
    }
  }
}
 
export function pddouble() /* () -> std/text/parse/parse ddouble */  {
  return $std_text_parse._lp__bar__bar__rp_(pddouble_sum, pddouble_normal);
}
 
 
// monadic lift
export function _mlift_parse_ddouble_10806(x, wild__) /* (x : ddouble, wild_ : ()) -> std/text/parse/parse ddouble */  {
  return x;
}
 
 
// monadic lift
export function _mlift_parse_ddouble_10807(x) /* (x : ddouble) -> std/text/parse/parse ddouble */  {
   
  var x_0_10851 = $std_text_parse.eof();
  if ($std_core_hnd._yielding()) {
    return $std_core_hnd.yield_extend(function(wild__ /* () */ ) {
      return x;
    });
  }
  else {
    return x;
  }
}
 
export function parse_ddouble(s) /* (s : string) -> maybe<ddouble> */  {
   
  var s_0_10129 = $std_core_string.to_lower((((((s).replace(/^\s\s*/,'')))).replace(/\s+$/,'')));
   
  var input_10127 = $std_core_sslice.Sslice(s_0_10129, 0, s_0_10129.length);
   
  var perr_10126 = $std_text_parse.parse(input_10127, function() {
       
      var x_10855 = $std_text_parse._lp__bar__bar__rp_(pddouble_sum, pddouble_normal);
      if ($std_core_hnd._yielding()) {
        return $std_core_hnd.yield_extend(_mlift_parse_ddouble_10807);
      }
      else {
        return _mlift_parse_ddouble_10807(x_10855);
      }
    });
  if (perr_10126._tag === 1) {
    return $std_core_types.Just(perr_10126.result);
  }
  else {
    return $std_core_types.Nothing;
  }
}
 
 
// Parse a floating point number with up to 31 digits precision.
// Return `dd-nan` if the string is an invalid number.
export function string_fs_ddouble(s) /* (s : string) -> ddouble */  {
   
  var m_10131 = parse_ddouble(s);
  return (m_10131 === null) ? dd_nan : m_10131.value;
}
 
 
// Decrement by one.
export function dec(x) /* (x : ddouble) -> ddouble */  {
  var _x77 = one.hi;
  var _x78 = one.lo;
  return _lp__plus__rp_(x, Ddouble((-_x77), (-_x78)));
}
 
 
// Zero constant
export var zero;
var zero = Ddouble(0.0, 0.0);
 
 
// Return the sum of a list of doubles.
// Uses [Kahan-Babu&scaron;kan-Neumaier summation](https://en.wikipedia.org/wiki/Kahan_summation_algorithm#Further_enhancements)
// to minimize rounding errors. This
// is more precise as Kahan summation and about as fast.\
// `[1.0e3,1.0e97,1.0e3,-1.0e97].sum == 2000.0`\
// A. Neumaier, _Rundungsfehleranalyse einiger Verfahren zur Summation endlicher Summen_.
// Math. Mechanik, 54:39--51, 1974.
export function list_fs_sum(xs) /* (xs : list<ddouble>) -> ddouble */  {
  return function() {
     
    var loc = { value: zero };
     
    var loc_0 = { value: zero };
     
    $std_core_list.foreach(xs, function(x /* ddouble */ ) {
         
        var t = _lp__plus__rp_(((loc).value), x);
         
        var x_1_10139 = ((loc).value);
         
        var _x80 = x_1_10139.hi;
        var _x79 = (_x80 < (0.0));
        if (_x79) {
          var _x81 = x_1_10139.hi;
          var _x82 = x_1_10139.lo;
          var x_0_10137 = Ddouble((-_x81), (-_x82));
        }
        else {
          var x_0_10137 = x_1_10139;
        }
         
        var y_10138 = abs(x);
         
        var _x86 = x_0_10137.hi;
        var _x87 = y_10138.hi;
        var _x85 = $std_num_float64.cmp(_x86, _x87);
        if (_x85 === 2) {
          var _x88 = x_0_10137.lo;
          var _x89 = y_10138.lo;
          var _x84 = $std_num_float64.cmp(_x88, _x89);
        }
        else {
          var _x84 = _x85;
        }
        var _x83 = $std_core_order._lp__excl__eq__rp_(_x84, $std_core_types.Lt);
        if (_x83) {
           
          var x_3_10143 = ((loc).value);
          var _x90 = t.hi;
          var _x91 = t.lo;
          var c = _lp__plus__rp_(_lp__plus__rp_(x_3_10143, Ddouble((-_x90), (-_x91))), x);
        }
        else {
          var _x92 = t.hi;
          var _x93 = t.lo;
          var c = _lp__plus__rp_(_lp__plus__rp_(x, Ddouble((-_x92), (-_x93))), ((loc).value));
        }
         
        ((loc_0).value = (_lp__plus__rp_(((loc_0).value), c)));
        return ((loc).value = t);
      });
     
    var res_0 = _lp__plus__rp_(((loc).value), ((loc_0).value));
     
    var res = $std_core_hnd.prompt_local_var(loc_0, res_0);
    return $std_core_hnd.prompt_local_var(loc, res);
  }();
}
 
 
// &pi;
export var dd_pi;
var dd_pi = Ddouble(3.141592653589793, 1.2246467991473532e-16);
 
 
// 2&pi;
export var dd_twopi;
var dd_twopi = Ddouble(6.283185307179586, 2.4492935982947064e-16);
 
 
// &pi;/2
export var dd_pi2;
var dd_pi2 = Ddouble(1.5707963267948966, 6.123233995736766e-17);
 
 
// &pi;/4
export var dd_pi4;
var dd_pi4 = Ddouble(0.7853981633974483, 3.061616997868383e-17);
 
 
// &pi;/16
export var dd_pi16;
var dd_pi16 = Ddouble(3.141592653589793, 1.2246467991473532e-16);
 
 
// 3&pi;/4
export var dd_pi34;
var dd_pi34 = Ddouble(2.356194490192345, 9.18485099360515e-17);
 
 
// The _e_ constant.
export var dd_e;
var dd_e = Ddouble(2.718281828459045, 1.4456468917292502e-16);
 
 
// The natural logarithm of 2
export var dd_ln2;
var dd_ln2 = Ddouble(0.6931471805599453, 2.3190468138462996e-17);
 
 
// The natural logarithm of 10
export var dd_ln10;
var dd_ln10 = Ddouble(2.302585092994046, (-2.1707562233822494e-16));
 
 
// The base-2 logarithm of _e_.
export var dd_log2e;
var dd_log2e = Ddouble(1.4426950408889634, 2.035527374093103e-17);
 
 
// The base-10 logarithm of _e_.
export var dd_log10e;
var dd_log10e = Ddouble(0.4342944819032518, 1.098319650216765e-17);
 
 
// The square-root of 2
export var dd_sqrt2;
var dd_sqrt2 = Ddouble(1.4142135623730951, (-9.667293313452913e-17));
 
 
// `1.0 / sqrt(2.0)`
export var dd_sqrt12;
var dd_sqrt12 = Ddouble(0.7071067811865476, (-4.833646656726457e-17));
 
 
// [Euler's constant](https://en.wikipedia.org/wiki/Euler%E2%80%93Mascheroni_constant)
export var dd_euler;
var dd_euler = Ddouble(0.5772156649015329, (-4.942915152430645e-18));
 
 
// The 'machine epsilon': this is not well-defined for a `:ddouble` in general since
// the difference between 1.0 and the next representable `:ddouble` value is `dd-true-min`.
// Instead, we take the square of `flt-epsilon`, i.e. 2^-104^.
export var dd_epsilon;
var dd_epsilon = Ddouble(4.930380657631324e-32, 0.0);
 
 
// 8*dd-epsilon
export var dd_epsilon8;
var dd_epsilon8 = Ddouble(3.944304526105059e-31, 0.0);
 
 
// The maximum representable `:ddouble`
export var dd_max;
var dd_max = Ddouble(1.7976931348623157e308, 9.979201547673598e291);
 
 
// The smallest positive `:ddouble` that is still normalized
export var dd_min;
var dd_min = Ddouble(2.2250738585072014e-308, 0.0);
 
 
// The smallest positive `:ddouble`  (which is subnormal).
export var dd_true_min;
var dd_true_min = Ddouble(5.0e-324, 0.0);
 
 
// Positive infinity
export var dd_posinf;
var dd_posinf = Ddouble($std_num_float64.posinf, 0.0);
 
 
// Negative infinity
export var dd_neginf;
var dd_neginf = Ddouble($std_num_float64.neginf, 0.0);
 
 
// maximal precision in decimal digits of a `:ddouble`.
export var dd_max_prec;
var dd_max_prec = 31;
 
export var dd_default_prec;
var dd_default_prec = -31;
 
export var two;
var two = Ddouble(2.0, 0.0);
 
 
// Is this `:ddouble` positive?
export function is_pos(x) /* (x : ddouble) -> bool */  {
  var _x79 = x.hi;
  return (_x79 > (0.0));
}
 
 
// Is this `:ddouble` not-a-number?
export function is_nan(x) /* (x : ddouble) -> bool */  {
  var _x81 = x.hi;
  var _x80 = isNaN(_x81);
  if (_x80) {
    return true;
  }
  else {
    var _x82 = x.lo;
    return isNaN(_x82);
  }
}
 
 
// Is this an infinite value.
export function is_inf(x) /* (x : ddouble) -> bool */  {
  var _x83 = x.hi;
  return $std_num_float64.is_inf(_x83);
}
 
 
// Does `x` equal positive infinity?
export function is_posinf(x) /* (x : ddouble) -> bool */  {
  var _x84 = x.hi;
  return ((_x84) === Infinity);
}
 
 
// Does `x` equal negative infinity?
export function is_neginf(x) /* (x : ddouble) -> bool */  {
  var _x85 = x.hi;
  return ((_x85) === -Infinity);
}
 
export function _lp__gt__rp_(x, y) /* (x : ddouble, y : ddouble) -> bool */  {
  var _x88 = x.hi;
  var _x89 = y.hi;
  var _x87 = $std_num_float64.cmp(_x88, _x89);
  if (_x87 === 2) {
    var _x90 = x.lo;
    var _x91 = y.lo;
    var _x86 = $std_num_float64.cmp(_x90, _x91);
  }
  else {
    var _x86 = _x87;
  }
  return $std_core_order._lp__eq__eq__rp_(_x86, $std_core_types.Gt);
}
 
export function _lp__lt__rp_(x, y) /* (x : ddouble, y : ddouble) -> bool */  {
  var _x94 = x.hi;
  var _x95 = y.hi;
  var _x93 = $std_num_float64.cmp(_x94, _x95);
  if (_x93 === 2) {
    var _x96 = x.lo;
    var _x97 = y.lo;
    var _x92 = $std_num_float64.cmp(_x96, _x97);
  }
  else {
    var _x92 = _x93;
  }
  return $std_core_order._lp__eq__eq__rp_(_x92, $std_core_types.Lt);
}
 
 
// Return the sign of the `:ddouble`.
export function is_sign(x) /* (x : ddouble) -> order */  {
  var _x99 = x.hi;
  var _x98 = (_x99 === (0.0));
  if (_x98) {
    return $std_core_types.Eq;
  }
  else {
    var _x101 = x.hi;
    var _x100 = (_x101 < (0.0));
    return (_x100) ? $std_core_types.Lt : $std_core_types.Gt;
  }
}
 
 
// The minimum of `x` and `y`.
export function min(x, y) /* (x : ddouble, y : ddouble) -> ddouble */  {
  var _x105 = x.hi;
  var _x106 = y.hi;
  var _x104 = $std_num_float64.cmp(_x105, _x106);
  if (_x104 === 2) {
    var _x107 = x.lo;
    var _x108 = y.lo;
    var _x103 = $std_num_float64.cmp(_x107, _x108);
  }
  else {
    var _x103 = _x104;
  }
  var _x102 = $std_core_order._lp__excl__eq__rp_(_x103, $std_core_types.Gt);
  return (_x102) ? x : y;
}
 
 
// The maximum of `x` and `y`
export function max(x, y) /* (x : ddouble, y : ddouble) -> ddouble */  {
  var _x112 = x.hi;
  var _x113 = y.hi;
  var _x111 = $std_num_float64.cmp(_x112, _x113);
  if (_x111 === 2) {
    var _x114 = x.lo;
    var _x115 = y.lo;
    var _x110 = $std_num_float64.cmp(_x114, _x115);
  }
  else {
    var _x110 = _x111;
  }
  var _x109 = $std_core_order._lp__excl__eq__rp_(_x110, $std_core_types.Lt);
  return (_x109) ? x : y;
}
 
export function edouble_fs__copy(_this, num, err) /* (edouble, num : ? float64, err : ? float64) -> edouble */  {
  if (num !== undefined) {
    var _x116 = num;
  }
  else {
    var _x116 = _this.num;
  }
  if (err !== undefined) {
    var _x117 = err;
  }
  else {
    var _x117 = _this.err;
  }
  return Edouble(_x116, _x117);
}
 
 
// Increment by one.
export function inc(x) /* (x : ddouble) -> ddouble */  {
  return _lp__plus__rp_(x, one);
}
 
 
// Round a `:ddouble` to the nearest integer, rounding to the nearest even number in case of a tie.
export function round(x) /* (x : ddouble) -> ddouble */  {
   
  var _x118 = x.hi;
  var r = $std_core_types._double_round_even(_x118);
   
  var _x119 = x.hi;
  var diff = (r - _x119);
  if ((diff === (0.0))) {
    var _x118 = x.lo;
    return dquicksum(r, $std_core_types._double_round_even(_x118));
  }
  else {
     
    if ((diff === (0.5))) {
      var _x120 = x.lo;
      var _x119 = (_x120 < (0.0));
      if (_x119) {
        var d_10166 = (r - (1.0));
      }
      else {
        if ((diff === ((-0.5)))) {
          var _x122 = x.lo;
          var _x121 = (_x122 > (0.0));
          var d_10166 = (_x121) ? (r + (1.0)) : r;
        }
        else {
          var d_10166 = r;
        }
      }
    }
    else {
      if ((diff === ((-0.5)))) {
        var _x124 = x.lo;
        var _x123 = (_x124 > (0.0));
        var d_10166 = (_x123) ? (r + (1.0)) : r;
      }
      else {
        var d_10166 = r;
      }
    }
    return Ddouble(d_10166, 0.0);
  }
}
 
 
// Remainder of two `:ddouble`s
export function _lp__perc__rp_(x, y) /* (x : ddouble, y : ddouble) -> ddouble */  {
   
  var n = round(_lp__fs__rp_(x, y));
   
  var y_0_10174 = _lp__star__rp_(n, y);
  var _x119 = y_0_10174.hi;
  var _x120 = y_0_10174.lo;
  return _lp__plus__rp_(x, Ddouble((-_x119), (-_x120)));
}
 
 
// Division and remainder of two `:ddouble`s
export function divrem(x, y) /* (x : ddouble, y : ddouble) -> (ddouble, ddouble) */  {
   
  var n = round(_lp__fs__rp_(x, y));
   
  var y_0_10176 = _lp__star__rp_(n, y);
  var _x121 = y_0_10176.hi;
  var _x122 = y_0_10176.lo;
  return $std_core_types.Tuple2(n, _lp__plus__rp_(x, Ddouble((-_x121), (-_x122))));
}
 
 
// Convert a `:ddouble` to a `:decimal` up to a given precision `prec` (= `-1`).
// A negative precision converts precisely. Returns 0 for non-finite `:ddouble`'s.
export function decimal(x, prec) /* (x : ddouble, prec : ? int) -> std/num/decimal/decimal */  {
   
  var _x124 = x.hi;
  var _x123 = isFinite(_x124);
  if (_x123) {
    var _x125 = x.lo;
    var b_10177 = isFinite(_x125);
  }
  else {
    var b_10177 = false;
  }
  if (b_10177) {
    var _x123 = x.hi;
    var _x124 = (prec !== undefined) ? prec : -1;
    var _x125 = x.lo;
    var _x126 = (prec !== undefined) ? prec : -1;
    return $std_num_decimal._lp__plus__rp_($std_num_decimal.float64_fs_decimal(_x123, _x124), $std_num_decimal.float64_fs_decimal(_x125, _x126));
  }
  else {
    var _x128 = undefined;
    var _x127 = (_x128 !== undefined) ? _x128 : 0;
    return $std_num_decimal.decimal_exp(0, _x127);
  }
}
 
 
// Convert a `:ddouble` to the nearest integer (rounding to the nearest even number in case of a tie)
export function int(x, nonfin) /* (x : ddouble, nonfin : ? int) -> int */  {
   
  var _x130 = x.hi;
  var _x129 = isFinite(_x130);
  if (_x129) {
    var _x131 = x.lo;
    var b_10182 = isFinite(_x131);
  }
  else {
    var b_10182 = false;
  }
  if (b_10182) {
    return $std_num_decimal.int(decimal(round(x)));
  }
  else {
    return (nonfin !== undefined) ? nonfin : 0;
  }
}
 
 
// Round to negative infinity.
export function floor(x) /* (x : ddouble) -> ddouble */  {
   
  var _x129 = x.hi;
  var r = Math.floor(_x129);
  var _x130 = x.hi;
  var _x129 = (r === _x130);
  if (_x129) {
    var _x131 = x.lo;
    return dquicksum(r, Math.floor(_x131));
  }
  else {
    return Ddouble(r, 0.0);
  }
}
 
 
// Round to positive infinity.
export function ceiling(x) /* (x : ddouble) -> ddouble */  {
   
  var _x132 = x.hi;
  var r = Math.ceil(_x132);
  var _x133 = x.hi;
  var _x132 = (r === _x133);
  if (_x132) {
    var _x134 = x.lo;
    return dquicksum(r, Math.ceil(_x134));
  }
  else {
    return Ddouble(r, 0.0);
  }
}
 
 
// Round towards zero.
export function truncate(x) /* (x : ddouble) -> ddouble */  {
  var _x136 = x.hi;
  var _x135 = (_x136 < (0.0));
  if (_x135) {
    return ceiling(x);
  }
  else {
    return floor(x);
  }
}
 
 
// The fraction of `x` such that `x.truncate + x.fraction == x`.
export function fraction(x) /* (x : ddouble) -> ddouble */  {
   
  var _x138 = x.hi;
  var _x137 = (_x138 < (0.0));
  if (_x137) {
    var y_10193 = ceiling(x);
  }
  else {
    var y_10193 = floor(x);
  }
  var _x137 = y_10193.hi;
  var _x138 = y_10193.lo;
  return _lp__plus__rp_(x, Ddouble((-_x137), (-_x138)));
}
 
 
// The _floored_ fraction of `x`. This is always positive, such that `x.floor + x.ffraction == x`.
export function ffraction(x) /* (x : ddouble) -> ddouble */  {
   
  var y_10195 = floor(x);
  var _x139 = y_10195.hi;
  var _x140 = y_10195.lo;
  return _lp__plus__rp_(x, Ddouble((-_x139), (-_x140)));
}
 
 
// Round a `:ddouble` to a specified precision.
// Uses `round` if the precision is smaller or equal to zero.
export function round_to_prec(x, prec) /* (x : ddouble, prec : int) -> ddouble */  {
  if ($std_core_types._int_le(prec,0)) {
    return round(x);
  }
  else {
    if ($std_core_types._int_gt(prec,31)) {
      return x;
    }
    else {
       
      var p = powi(ten, prec);
      return _lp__fs__rp_(round(_lp__star__rp_(x, p)), p);
    }
  }
}
 
 
// 'Load exponent': returns `x`&middot;2^`exp`^.
export function ldexp(x, exp_0) /* (x : ddouble, exp : int) -> ddouble */  {
  var _x141 = x.hi;
  var _x142 = x.lo;
  return Ddouble($std_num_float64.ldexp(_x141, exp_0), $std_num_float64.ldexp(_x142, exp_0));
}
 
 
// Create a `:ddouble` `x` such that `x` equals `d`&middot;10^`e`^.
export function float64_fs_ddouble_exp(d, e) /* (d : float64, e : int) -> ddouble */  {
   
  var x_10199 = Ddouble(d, 0.0);
  if ($std_core_types._int_iszero(e)) {
    return x_10199;
  }
  else {
    return _lp__star__rp_(x_10199, powi(ten, e));
  }
}
 
 
// Create a `:ddouble` `x` such that `x` equals `i`&middot;10^`e`^.
export function int_fs_ddouble_exp(i, exp_0) /* (i : int, exp : int) -> ddouble */  {
   
  var x_10203 = ddouble_int_exp(i, 0);
  if ($std_core_types._int_iszero(exp_0)) {
    return x_10203;
  }
  else {
    return _lp__star__rp_(x_10203, powi(ten, exp_0));
  }
}
 
 
// Decode a `:ddouble` `d` into two doubles `(hi,lo)` such that `d` equals  `hi`+`lo`,
// where `lo` &le; 0.5&middot;ulp(`hi`).
export function decode(d) /* (d : ddouble) -> (float64, float64) */  {
  var _x143 = d.hi;
  var _x144 = d.lo;
  return $std_core_types.Tuple2(_x143, _x144);
}
 
 
// Encode a `:ddouble` `d` from two doubles `(hi,lo)` such that `d` equals  `hi`+`lo`.
export function encode(hi, lo) /* (hi : float64, lo : float64) -> ddouble */  {
  return _lp__plus__rp_(Ddouble(hi, 0.0), Ddouble(lo, 0.0));
}
 
 
// The square root of a non-negative `:ddouble` `x`.
// For negative `x`, `dd-nan` is returned.
export function sqrt(x) /* (x : ddouble) -> ddouble */  {
  var _x146 = x.hi;
  var _x145 = (_x146 === (0.0));
  if (_x145) {
    return zero;
  }
  else {
    var _x148 = x.hi;
    var _x147 = (_x148 < (0.0));
    if (_x147) {
      return dd_nan;
    }
    else {
       
      var _x149 = x.hi;
      var a = ((1.0) / (Math.sqrt(_x149)));
       
      var _x150 = x.hi;
      var t1 = (_x150 * a);
       
      var y_10221 = sqr(Ddouble(t1, 0.0));
       
      var _x151 = y_10221.hi;
      var _x152 = y_10221.lo;
      var ddouble_0_3_10219 = _lp__plus__rp_(x, Ddouble((-_x151), (-_x152)));
       
      var _x153 = ddouble_0_3_10219.hi;
      var t2 = (((_x153 * a)) * (0.5));
      return dsum(t1, t2);
    }
  }
}
 
 
// Multiply `x` by a `:float64` `p` where `p` must be a power of 2.
export function mul_pwr2(x, p) /* (x : ddouble, p : float64) -> ddouble */  {
  var _x149 = x.hi;
  var _x150 = x.lo;
  return Ddouble((_x149 * p), (_x150 * p));
}
 
export function half(x) /* (x : ddouble) -> ddouble */  {
  var _x151 = x.hi;
  var _x152 = x.lo;
  return Ddouble((_x151 * (0.5)), (_x152 * (0.5)));
}
 
export function twice(x) /* (x : ddouble) -> ddouble */  {
  var _x153 = x.hi;
  var _x154 = x.lo;
  return Ddouble((_x153 * (2.0)), (_x154 * (2.0)));
}
 
export var exp_factors;
var exp_factors = $std_core_types.Cons(Ddouble(0.16666666666666666, 9.25185853854297e-18), $std_core_types.Cons(Ddouble(4.1666666666666664e-2, 2.3129646346357427e-18), $std_core_types.Cons(Ddouble(8.333333333333333e-3, 1.1564823173178714e-19), $std_core_types.Cons(Ddouble(1.388888888888889e-3, (-5.300543954373577e-20)), $std_core_types.Cons(Ddouble(1.984126984126984e-4, 1.7209558293420705e-22), $std_core_types.Cons(Ddouble(2.48015873015873e-5, 2.1511947866775882e-23), $std_core_types.Nil))))));
 
export function exp_approx(p, t, r, eps, fs, s) /* (p : ddouble, t : ddouble, r : ddouble, eps : float64, fs : list<ddouble>, s : ? ddouble) -> ddouble */  { tailcall: while(1)
{
  if (fs === null) {
    var _x155 = (s !== undefined) ? s : zero;
    return _lp__plus__rp_(_x155, t);
  }
  else {
     
    var _x156 = (s !== undefined) ? s : zero;
    var s1 = _lp__plus__rp_(_x156, t);
     
    var p1 = _lp__star__rp_(p, r);
     
    var t1 = _lp__star__rp_(p1, fs.head);
    var _x157 = t1.hi;
    var _x156 = ((Math.abs(_x157)) <= eps);
    if (_x156) {
      var _x158 = (s !== undefined) ? s : zero;
      return _lp__plus__rp_(_x158, t);
    }
    else {
      {
        // tail call
        var _x159 = s1;
        p = p1;
        t = t1;
        fs = fs.tail;
        s = _x159;
        continue tailcall;
      }
    }
  }
}}
 
 
// Return _e_ (`dd-e`) to the power of `x`.
export function exp(x) /* (x : ddouble) -> ddouble */  {
   
  var inv_k = ((1.0) / (512.0));
  var _x161 = x.hi;
  var _x160 = (_x161 <= ((-709.0)));
  if (_x160) {
    return zero;
  }
  else {
    var _x163 = x.hi;
    var _x162 = (_x163 >= (709.0));
    if (_x162) {
      return dd_posinf;
    }
    else {
      var _x165 = x.hi;
      var _x164 = (_x165 === (0.0));
      if (_x164) {
        return one;
      }
      else {
        var _x169 = x.hi;
        var _x170 = one.hi;
        var _x168 = $std_num_float64.cmp(_x169, _x170);
        if (_x168 === 2) {
          var _x171 = x.lo;
          var _x172 = one.lo;
          var _x167 = $std_num_float64.cmp(_x171, _x172);
        }
        else {
          var _x167 = _x168;
        }
        var _x166 = $std_core_order._lp__eq__eq__rp_(_x167, $std_core_types.Eq);
        if (_x166) {
          return dd_e;
        }
        else {
          return function() {
             
            var _x173 = x.hi;
            var _x174 = dd_ln2.hi;
            var m = $std_core_types._int_double((Math.floor(((((_x173 / _x174)) + (0.5))))));
             
            var y_0_10248 = _lp__star__rp_(ddouble_int_exp(m, 0), dd_ln2);
             
            var _x175 = y_0_10248.hi;
            var _x176 = y_0_10248.lo;
            var x_2_10245 = _lp__plus__rp_(x, Ddouble((-_x175), (-_x176)));
             
            var _x177 = x_2_10245.hi;
            var _x178 = x_2_10245.lo;
            var r = Ddouble((_x177 * inv_k), (_x178 * inv_k));
             
            var p_0 = sqr(r);
             
            var _x179 = p_0.hi;
            var _x180 = p_0.lo;
            var t = _lp__plus__rp_(r, Ddouble((_x179 * (0.5)), (_x180 * (0.5))));
             
            var _x181 = dd_epsilon.hi;
            var init_10861 = exp_approx(p_0, t, r, (inv_k * _x181), exp_factors);
             
            var loc = { value: init_10861 };
             
            $std_core.repeat(9, function() {
                 
                var x_4_10253 = ((loc).value);
                var _x182 = x_4_10253.hi;
                var _x183 = x_4_10253.lo;
                return ((loc).value = (_lp__plus__rp_(Ddouble((_x182 * (2.0)), (_x183 * (2.0))), sqr(((loc).value)))));
              });
             
            ((loc).value = (_lp__plus__rp_(((loc).value), one)));
             
            var x_6_10258 = ((loc).value);
             
            var _x184 = x_6_10258.hi;
            var _x185 = x_6_10258.lo;
            var res = Ddouble($std_num_float64.ldexp(_x184, m), $std_num_float64.ldexp(_x185, m));
            return $std_core_hnd.prompt_local_var(loc, res);
          }();
        }
      }
    }
  }
}
 
 
// The natural logarithm (in base _e_) of `x`.
export function ln(x) /* (x : ddouble) -> ddouble */  {
  var _x176 = x.hi;
  var _x177 = zero.hi;
  var _x175 = $std_num_float64.cmp(_x176, _x177);
  if (_x175 === 2) {
    var _x178 = x.lo;
    var _x179 = zero.lo;
    var _x174 = $std_num_float64.cmp(_x178, _x179);
  }
  else {
    var _x174 = _x175;
  }
  var _x173 = $std_core_order._lp__excl__eq__rp_(_x174, $std_core_types.Gt);
  if (_x173) {
    var _x183 = x.hi;
    var _x184 = zero.hi;
    var _x182 = $std_num_float64.cmp(_x183, _x184);
    if (_x182 === 2) {
      var _x185 = x.lo;
      var _x186 = zero.lo;
      var _x181 = $std_num_float64.cmp(_x185, _x186);
    }
    else {
      var _x181 = _x182;
    }
    var _x180 = $std_core_order._lp__eq__eq__rp_(_x181, $std_core_types.Eq);
    return (_x180) ? dd_neginf : dd_nan;
  }
  else {
    var _x190 = x.hi;
    var _x191 = one.hi;
    var _x189 = $std_num_float64.cmp(_x190, _x191);
    if (_x189 === 2) {
      var _x192 = x.lo;
      var _x193 = one.lo;
      var _x188 = $std_num_float64.cmp(_x192, _x193);
    }
    else {
      var _x188 = _x189;
    }
    var _x187 = $std_core_order._lp__eq__eq__rp_(_x188, $std_core_types.Eq);
    if (_x187) {
      return zero;
    }
    else {
      var _x197 = x.hi;
      var _x198 = dd_e.hi;
      var _x196 = $std_num_float64.cmp(_x197, _x198);
      if (_x196 === 2) {
        var _x199 = x.lo;
        var _x200 = dd_e.lo;
        var _x195 = $std_num_float64.cmp(_x199, _x200);
      }
      else {
        var _x195 = _x196;
      }
      var _x194 = $std_core_order._lp__eq__eq__rp_(_x195, $std_core_types.Eq);
      if (_x194) {
        return one;
      }
      else {
        var _x202 = x.hi;
        var _x201 = ((_x202) === Infinity);
        if (_x201) {
          return x;
        }
        else {
           
          var _x203 = x.hi;
          var d_10272 = Math.log(_x203);
           
          var a0 = Ddouble(d_10272, 0.0);
           
          var _x204 = a0.hi;
          var _x205 = a0.lo;
          var x_5_10274 = _lp__star__rp_(x, exp(Ddouble((-_x204), (-_x205))));
          var _x203 = one.hi;
          var _x204 = one.lo;
          return _lp__plus__rp_(a0, _lp__plus__rp_(x_5_10274, Ddouble((-_x203), (-_x204))));
        }
      }
    }
  }
}
 
 
// `x` to the power of `y` both as `:ddouble`
export function pow(x, y) /* (x : ddouble, y : ddouble) -> ddouble */  {
  return exp(_lp__star__rp_(y, ln(x)));
}
 
 
// Return 10 to the power of `exp`.
export function exp10(exp_0) /* (exp : ddouble) -> ddouble */  {
  return exp(_lp__star__rp_(exp_0, ln(ten)));
}
 
 
// Return 2 to the power of `exp`.
export function exp2(exp_0) /* (exp : ddouble) -> ddouble */  {
  return exp(_lp__star__rp_(exp_0, ln(two)));
}
 
 
// Show a `:ddouble` `x` with a given precision `prec` (=`-31`).
// The precision specifies the  number of digits after the dot (in either scientific of fixed-point notation).
// If the precision is negative, _at most_ `prec` digits are displayed, while for a positive
// precision, exactly `prec` digits behind the dot are displayed.
// This uses `show-fixed` when the exponent of `x` in scientific notation is larger than -5
// and smaller than the precision (or 15 in case of a negative precision), otherwise it uses `show-exp`.
export function show(x, prec) /* (x : ddouble, prec : ? int) -> string */  {
   
  var _x206 = x.hi;
  var _x205 = isFinite(_x206);
  if (_x205) {
    var _x207 = x.lo;
    var b_10280 = isFinite(_x207);
  }
  else {
    var b_10280 = false;
  }
  if (b_10280) {
    var _x205 = (prec !== undefined) ? prec : -31;
    return $std_num_decimal.show(decimal(x), _x205);
  }
  else {
    var _x206 = x.hi;
    return $std_num_float64.show(_x206);
  }
}
 
 
/* Show a `:ddouble` `x` precisely as the sum of two `:float64`s
in [hexadecimal notation](https://books.google.com/books?id=FgMsCwAAQBAJ&pg=PA41).
Use this if you need to guarantee that you can parse back `:ddouble`s exactly,
i.e. `x == x.show-hex.ddouble`.
```
> 0.1.ddouble.show-hex
"0x1.999999999999Ap-4 + 0x0.0p+0"
> "0.1".ddouble.show-hex
"0x1.999999999999Ap-4 + -0x1.999999999999Ap-58"
> dd-pi.show-hex
"0x1.921FB54442D18p+1 + 0x1.1A62633145C07p-53"
> dd-max.show-hex
"0x1.FFFFFFFFFFFFFp+1023 + 0x1.FFFFFFFFFFFFFp+969"
```
.
*/
export function show_hex(x, width, use_capitals, pre) /* (x : ddouble, width : ? int, use-capitals : ? bool, pre : ? string) -> string */  {
   
  var _x208 = x.hi;
  var _x207 = isFinite(_x208);
  if (_x207) {
    var _x209 = x.lo;
    var b_10282 = isFinite(_x209);
  }
  else {
    var b_10282 = false;
  }
  if (b_10282) {
    var _x207 = x.hi;
    var _x208 = (width !== undefined) ? width : 1;
    var _x209 = (use_capitals !== undefined) ? use_capitals : true;
    var _x210 = (pre !== undefined) ? pre : "0x";
    var _x211 = x.lo;
    var _x212 = (width !== undefined) ? width : 1;
    var _x213 = (use_capitals !== undefined) ? use_capitals : true;
    var _x214 = (pre !== undefined) ? pre : "0x";
    return $std_core_types._lp__plus__plus__rp_($std_num_float64.show_hex(_x207, _x208, _x209, _x210), $std_core_types._lp__plus__plus__rp_(" + ", $std_num_float64.show_hex(_x211, _x212, _x213, _x214)));
  }
  else {
    var _x215 = x.hi;
    return $std_num_float64.show(_x215);
  }
}
 
 
/* Show a ddouble `x` with a given precision `prec` (=`-31`) in scientific notation.
The precision specifies the  number of digits after the dot, i.e.
the number of significant digits is `prec+1`.
If the precision is negative, _at most_ `prec` digits are displayed, and if
it is positive exactly `prec` digits are used.
```
> 1.1.ddouble.show-exp
"1.1000000000000000888178419700125"
> 1.1.ddouble.show-exp(-100)
"1.100000000000000088817841970012523233890533447265625"
> 1.1.ddouble.show-exp(5)
"1.10000"
> 1.1.ddouble.show-exp(-5)
"1.1"
```
.
*/
export function show_exp(x, prec) /* (x : ddouble, prec : ? int) -> string */  {
   
  var _x217 = x.hi;
  var _x216 = isFinite(_x217);
  if (_x216) {
    var _x218 = x.lo;
    var b_10286 = isFinite(_x218);
  }
  else {
    var b_10286 = false;
  }
  if (b_10286) {
    var _x216 = (prec !== undefined) ? prec : -31;
    return $std_num_decimal.show_exp(decimal(x), _x216);
  }
  else {
    var _x217 = x.hi;
    return $std_num_float64.show(_x217);
  }
}
 
 
/* Show a ddouble `x` with a given precision `prec` (=`-31`) in fixed-point notation.
The precision specifies the  number of digits after the dot.
If the precision is negative, _at most_  `prec` digits after the dot are displayed,
while for a positive precision, exactly `prec` digits are used.
```
> 0.1.ddouble.show-fixed
"0.1000000000000000055511151231258"
> 0.1.ddouble.show-fixed(-100)
"0.1000000000000000055511151231257827021181583404541015625"
> 0.1.ddouble.show-fixed(5)
"0.10000"
> 0.1.ddouble.show-fixed(-5)
"0.1"
```
.
*/
export function show_fixed(x, prec) /* (x : ddouble, prec : ? int) -> string */  {
   
  var _x219 = x.hi;
  var _x218 = isFinite(_x219);
  if (_x218) {
    var _x220 = x.lo;
    var b_10288 = isFinite(_x220);
  }
  else {
    var b_10288 = false;
  }
  if (b_10288) {
    var _x218 = (prec !== undefined) ? prec : -31;
    return $std_num_decimal.show_fixed(decimal(x), _x218);
  }
  else {
    var _x219 = x.hi;
    return $std_num_float64.show(_x219);
  }
}
 
 
// Show a `:ddouble` as the sum of  `:float64`s with an optional precision.
// Note: use `show-hex` for reliable round-trip parsing.
export function show_sum(x, prec) /* (x : ddouble, prec : ? int) -> string */  {
   
  var _x221 = x.hi;
  var _x220 = isFinite(_x221);
  if (_x220) {
    var _x222 = x.lo;
    var b_10290 = isFinite(_x222);
  }
  else {
    var b_10290 = false;
  }
  if (b_10290) {
    var _x220 = x.hi;
    var _x221 = (prec !== undefined) ? prec : -17;
    var _x222 = x.lo;
    var _x223 = (prec !== undefined) ? prec : -17;
    return $std_core_types._lp__plus__plus__rp_($std_num_float64.show(_x220, _x221), $std_core_types._lp__plus__plus__rp_(" + ", $std_num_float64.show(_x222, _x223)));
  }
  else {
    var _x224 = x.hi;
    return $std_num_float64.show(_x224);
  }
}
 
 
// The `n`-th root of a `:ddouble` number `x`.
// `n` must be positive, and if `n` is even, then
// `x` must not be negative.
export function nroot(x, n) /* (x : ddouble, n : int) -> ddouble */  {
  if ($std_core_types._int_eq(n,2)) {
    return sqrt(x);
  }
  else {
    if ($std_core_types._int_eq(n,1)) {
      return x;
    }
    else {
      if ($std_core_types._int_le(n,0)) {
        return dd_nan;
      }
      else {
         
        var b_10014 = $std_core_types._int_isodd(n);
        if (b_10014) {
          var _x226 = x.hi;
          var _x225 = (_x226 === (0.0));
          if (_x225) {
            return zero;
          }
          else {
             
            var r_0 = abs(x);
             
            var _x227 = r_0.hi;
            var d_4_10311 = Math.exp(((((-(Math.log(_x227)))) / ($std_core_types._int_to_double(n)))));
             
            var a0_0 = Ddouble(d_4_10311, 0.0);
             
            var y_0_10314 = _lp__star__rp_(r_0, powi(a0_0, n));
             
            var _x228 = y_0_10314.hi;
            var _x229 = y_0_10314.lo;
            var a1_0 = _lp__plus__rp_(a0_0, _lp__fs__rp_(_lp__star__rp_(a0_0, _lp__plus__rp_(one, Ddouble((-_x228), (-_x229)))), ddouble_int_exp(n, 0)));
            var _x229 = x.hi;
            var _x228 = (_x229 < (0.0));
            if (_x228) {
              var _x230 = one.hi;
              var _x231 = one.lo;
              var _x227 = Ddouble((-_x230), (-_x231));
            }
            else {
              var _x227 = one;
            }
            return _lp__fs__rp_(_x227, a1_0);
          }
        }
        else {
          var _x233 = x.hi;
          var _x232 = (_x233 < (0.0));
          if (_x232) {
            return dd_nan;
          }
          else {
            var _x235 = x.hi;
            var _x234 = (_x235 === (0.0));
            if (_x234) {
              return zero;
            }
            else {
               
              var r = abs(x);
               
              var _x236 = r.hi;
              var d_1_10301 = Math.exp(((((-(Math.log(_x236)))) / ($std_core_types._int_to_double(n)))));
               
              var a0 = Ddouble(d_1_10301, 0.0);
               
              var y_10304 = _lp__star__rp_(r, powi(a0, n));
               
              var _x237 = y_10304.hi;
              var _x238 = y_10304.lo;
              var a1 = _lp__plus__rp_(a0, _lp__fs__rp_(_lp__star__rp_(a0, _lp__plus__rp_(one, Ddouble((-_x237), (-_x238)))), ddouble_int_exp(n, 0)));
              var _x238 = x.hi;
              var _x237 = (_x238 < (0.0));
              if (_x237) {
                var _x239 = one.hi;
                var _x240 = one.lo;
                var _x236 = Ddouble((-_x239), (-_x240));
              }
              else {
                var _x236 = one;
              }
              return _lp__fs__rp_(_x236, a1);
            }
          }
        }
      }
    }
  }
}
 
export var one_half;
var one_half = Ddouble(0.5, 0.0);
 
 
// Return the logarithm in some base `b` of a `:ddouble` `x`
export function log(x, base) /* (x : ddouble, base : ddouble) -> ddouble */  {
  return _lp__fs__rp_(ln(x), ln(base));
}
 
 
// The logarithm in base 10 of `x`.
export function log10(x) /* (x : ddouble) -> ddouble */  {
  return _lp__fs__rp_(ln(x), dd_ln10);
}
 
 
// The logarithm in base 2 of `x`.
export function log2(x) /* (x : ddouble) -> ddouble */  {
  return _lp__fs__rp_(ln(x), dd_ln2);
}
 
 
// Return `ln(1.0 + x)`.
// Avoids potential imprecision for small `x` where adding `1.0` explicitly
// may lead to rounding errors.
export function ln1p(x) /* (x : ddouble) -> ddouble */  {
  var _x242 = x.hi;
  var _x241 = ((_x242) === Infinity);
  if (_x241) {
    return x;
  }
  else {
     
    var y = _lp__plus__rp_(one, x);
     
    var _x243 = one.hi;
    var _x244 = one.lo;
    var z = _lp__plus__rp_(y, Ddouble((-_x243), (-_x244)));
    var _x244 = z.hi;
    var _x243 = (_x244 === (0.0));
    if (_x243) {
      return x;
    }
    else {
      return _lp__star__rp_(ln(y), _lp__fs__rp_(x, z));
    }
  }
}
 
 
// Return `exp(x - 1.0)`.
// Avoids rounding errors for values of `x` very close to `1.0`.
export function expm1(x) /* (x : ddouble) -> ddouble */  {
  var _x246 = x.hi;
  var _x245 = ((_x246) === Infinity);
  if (_x245) {
    return x;
  }
  else {
     
    var y = exp(x);
    var _x250 = y.hi;
    var _x251 = one.hi;
    var _x249 = $std_num_float64.cmp(_x250, _x251);
    if (_x249 === 2) {
      var _x252 = y.lo;
      var _x253 = one.lo;
      var _x248 = $std_num_float64.cmp(_x252, _x253);
    }
    else {
      var _x248 = _x249;
    }
    var _x247 = $std_core_order._lp__eq__eq__rp_(_x248, $std_core_types.Eq);
    if (_x247) {
      return x;
    }
    else {
       
      var _x254 = one.hi;
      var _x255 = one.lo;
      var ym = _lp__plus__rp_(y, Ddouble((-_x254), (-_x255)));
       
      var _x256 = one.hi;
      var _x257 = one.lo;
      var y_2_10333 = Ddouble((-_x256), (-_x257));
      var _x257 = ym.hi;
      var _x258 = y_2_10333.hi;
      var _x256 = $std_num_float64.cmp(_x257, _x258);
      if (_x256 === 2) {
        var _x259 = ym.lo;
        var _x260 = y_2_10333.lo;
        var _x255 = $std_num_float64.cmp(_x259, _x260);
      }
      else {
        var _x255 = _x256;
      }
      var _x254 = $std_core_order._lp__eq__eq__rp_(_x255, $std_core_types.Eq);
      if (_x254) {
        var _x261 = one.hi;
        var _x262 = one.lo;
        return Ddouble((-_x261), (-_x262));
      }
      else {
        return _lp__star__rp_(ym, _lp__fs__rp_(x, ln(y)));
      }
    }
  }
}
 
export function log2p1(x) /* (x : ddouble) -> ddouble */  {
  return _lp__star__rp_(dd_log2e, ln1p(x));
}
 
export function exp2m1(x) /* (x : ddouble) -> ddouble */  {
  return expm1(_lp__star__rp_(dd_ln2, x));
}
 
 
// Returns `ln(exp(x) + exp(y))`.
// Avoids overlow/underflow errors.
export function lnaddexp(x, y) /* (x : ddouble, y : ddouble) -> ddouble */  {
  var _x266 = x.hi;
  var _x267 = y.hi;
  var _x265 = $std_num_float64.cmp(_x266, _x267);
  if (_x265 === 2) {
    var _x268 = x.lo;
    var _x269 = y.lo;
    var _x264 = $std_num_float64.cmp(_x268, _x269);
  }
  else {
    var _x264 = _x265;
  }
  var _x263 = $std_core_order._lp__eq__eq__rp_(_x264, $std_core_types.Eq);
  if (_x263) {
    return _lp__plus__rp_(x, dd_ln2);
  }
  else {
     
    var _x270 = y.hi;
    var _x271 = y.lo;
    var z = _lp__plus__rp_(x, Ddouble((-_x270), (-_x271)));
    var _x271 = z.hi;
    var _x270 = (_x271 > (0.0));
    if (_x270) {
      var _x272 = z.hi;
      var _x273 = z.lo;
      return _lp__plus__rp_(x, ln1p(exp(Ddouble((-_x272), (-_x273)))));
    }
    else {
      return _lp__plus__rp_(y, ln1p(exp(z)));
    }
  }
}
 
 
// Returns `log2( exp2(x) + exp2(y) )`.
// Avoids overlow/underflow errors.
export function logaddexp2(x, y) /* (x : ddouble, y : ddouble) -> ddouble */  {
  var _x277 = x.hi;
  var _x278 = y.hi;
  var _x276 = $std_num_float64.cmp(_x277, _x278);
  if (_x276 === 2) {
    var _x279 = x.lo;
    var _x280 = y.lo;
    var _x275 = $std_num_float64.cmp(_x279, _x280);
  }
  else {
    var _x275 = _x276;
  }
  var _x274 = $std_core_order._lp__eq__eq__rp_(_x275, $std_core_types.Eq);
  if (_x274) {
    return _lp__plus__rp_(x, one);
  }
  else {
     
    var _x281 = y.hi;
    var _x282 = y.lo;
    var z = _lp__plus__rp_(x, Ddouble((-_x281), (-_x282)));
    var _x282 = z.hi;
    var _x281 = (_x282 > (0.0));
    if (_x281) {
       
      var _x283 = z.hi;
      var _x284 = z.lo;
      var exp_0_10349 = Ddouble((-_x283), (-_x284));
       
      var x_3_10348 = exp(_lp__star__rp_(exp_0_10349, ln(two)));
      return _lp__plus__rp_(x, _lp__star__rp_(dd_log2e, ln1p(x_3_10348)));
    }
    else {
       
      var x_5_10352 = exp(_lp__star__rp_(z, ln(two)));
      return _lp__plus__rp_(y, _lp__star__rp_(dd_log2e, ln1p(x_5_10352)));
    }
  }
}
 
 
// Return if two `:ddouble`s are nearly equal with respect to some `epsilon` (=`8*dd-epsilon`).
// The epsilon is the nearest difference for numbers around 1.0. The routine automatically
// scales the epsilon for larger and smaller numbers, and for numbers close to zero.
export function nearly_eq(x, y, epsilon) /* (x : ddouble, y : ddouble, epsilon : ? ddouble) -> bool */  {
  var _x286 = x.hi;
  var _x287 = y.hi;
  var _x285 = $std_num_float64.cmp(_x286, _x287);
  if (_x285 === 2) {
    var _x288 = x.lo;
    var _x289 = y.lo;
    var _x284 = $std_num_float64.cmp(_x288, _x289);
  }
  else {
    var _x284 = _x285;
  }
  var _x283 = $std_core_order._lp__eq__eq__rp_(_x284, $std_core_types.Eq);
  if (_x283) {
    return true;
  }
  else {
     
    var _x290 = y.hi;
    var _x291 = y.lo;
    var x_1_10358 = _lp__plus__rp_(x, Ddouble((-_x290), (-_x291)));
     
    var _x293 = x_1_10358.hi;
    var _x292 = (_x293 < (0.0));
    if (_x292) {
      var _x294 = x_1_10358.hi;
      var _x295 = x_1_10358.lo;
      var diff = Ddouble((-_x294), (-_x295));
    }
    else {
      var diff = x_1_10358;
    }
    var _x291 = x.hi;
    var _x290 = (_x291 === (0.0));
    if (_x290) {
       
      var x_5_10367 = _lp__star__rp_(two, diff);
       
      var _x292 = (epsilon !== undefined) ? epsilon : dd_epsilon8;
      var y_2_10368 = _lp__star__rp_(_x292, dd_min);
      var _x294 = x_5_10367.hi;
      var _x295 = y_2_10368.hi;
      var _x293 = $std_num_float64.cmp(_x294, _x295);
      if (_x293 === 2) {
        var _x296 = x_5_10367.lo;
        var _x297 = y_2_10368.lo;
        var _x292 = $std_num_float64.cmp(_x296, _x297);
      }
      else {
        var _x292 = _x293;
      }
      return $std_core_order._lp__eq__eq__rp_(_x292, $std_core_types.Lt);
    }
    else {
      var _x299 = y.hi;
      var _x298 = (_x299 === (0.0));
      if (_x298) {
         
        var x_7_10372 = _lp__star__rp_(two, diff);
         
        var _x300 = (epsilon !== undefined) ? epsilon : dd_epsilon8;
        var y_3_10373 = _lp__star__rp_(_x300, dd_min);
        var _x302 = x_7_10372.hi;
        var _x303 = y_3_10373.hi;
        var _x301 = $std_num_float64.cmp(_x302, _x303);
        if (_x301 === 2) {
          var _x304 = x_7_10372.lo;
          var _x305 = y_3_10373.lo;
          var _x300 = $std_num_float64.cmp(_x304, _x305);
        }
        else {
          var _x300 = _x301;
        }
        return $std_core_order._lp__eq__eq__rp_(_x300, $std_core_types.Lt);
      }
      else {
        var _x309 = diff.hi;
        var _x310 = dd_min.hi;
        var _x308 = $std_num_float64.cmp(_x309, _x310);
        if (_x308 === 2) {
          var _x311 = diff.lo;
          var _x312 = dd_min.lo;
          var _x307 = $std_num_float64.cmp(_x311, _x312);
        }
        else {
          var _x307 = _x308;
        }
        var _x306 = $std_core_order._lp__eq__eq__rp_(_x307, $std_core_types.Lt);
        if (_x306) {
           
          var x_9_10376 = _lp__star__rp_(two, diff);
           
          var _x313 = (epsilon !== undefined) ? epsilon : dd_epsilon8;
          var y_5_10377 = _lp__star__rp_(_x313, dd_min);
          var _x315 = x_9_10376.hi;
          var _x316 = y_5_10377.hi;
          var _x314 = $std_num_float64.cmp(_x315, _x316);
          if (_x314 === 2) {
            var _x317 = x_9_10376.lo;
            var _x318 = y_5_10377.lo;
            var _x313 = $std_num_float64.cmp(_x317, _x318);
          }
          else {
            var _x313 = _x314;
          }
          return $std_core_order._lp__eq__eq__rp_(_x313, $std_core_types.Lt);
        }
        else {
           
          var sum_0 = _lp__plus__rp_(abs(x), abs(y));
           
          var _x323 = sum_0.hi;
          var _x324 = dd_max.hi;
          var _x322 = $std_num_float64.cmp(_x323, _x324);
          if (_x322 === 2) {
            var _x325 = sum_0.lo;
            var _x326 = dd_max.lo;
            var _x321 = $std_num_float64.cmp(_x325, _x326);
          }
          else {
            var _x321 = _x322;
          }
          var _x320 = $std_core_order._lp__eq__eq__rp_(_x321, $std_core_types.Gt);
          var _x319 = (_x320) ? dd_max : sum_0;
          var x_10_10378 = _lp__fs__rp_(_lp__star__rp_(two, diff), _x319);
          var _x321 = x_10_10378.hi;
          var _x323 = (epsilon !== undefined) ? epsilon : dd_epsilon8;
          var _x322 = _x323.hi;
          var _x320 = $std_num_float64.cmp(_x321, _x322);
          if (_x320 === 2) {
            var _x324 = x_10_10378.lo;
            var _x326 = (epsilon !== undefined) ? epsilon : dd_epsilon8;
            var _x325 = _x326.lo;
            var _x319 = $std_num_float64.cmp(_x324, _x325);
          }
          else {
            var _x319 = _x320;
          }
          return $std_core_order._lp__eq__eq__rp_(_x319, $std_core_types.Lt);
        }
      }
    }
  }
}
 
 
// Return if two `:ddouble`s are nearly equal with respect to an `epsilon` of `10*dd-epsilon`.
// See also `nearly-eq` which takes an explicit `epsilon`.
export function _lp__tilde__eq__rp_(x, y) /* (x : ddouble, y : ddouble) -> bool */  {
  return nearly_eq(x, y);
}
 
 
// The hypotenuse of `x` and `y`: `sqrt(x*x + y*y)`.
// Prevents overflow for large numbers.
export function hypot(x, y) /* (x : ddouble, y : ddouble) -> ddouble */  {
   
  var xx = abs(x);
   
  var yy = abs(y);
   
  var _x330 = xx.hi;
  var _x331 = yy.hi;
  var _x329 = $std_num_float64.cmp(_x330, _x331);
  if (_x329 === 2) {
    var _x332 = xx.lo;
    var _x333 = yy.lo;
    var _x328 = $std_num_float64.cmp(_x332, _x333);
  }
  else {
    var _x328 = _x329;
  }
  var _x327 = $std_core_order._lp__excl__eq__rp_(_x328, $std_core_types.Gt);
  var lo = (_x327) ? xx : yy;
   
  var _x337 = xx.hi;
  var _x338 = yy.hi;
  var _x336 = $std_num_float64.cmp(_x337, _x338);
  if (_x336 === 2) {
    var _x339 = xx.lo;
    var _x340 = yy.lo;
    var _x335 = $std_num_float64.cmp(_x339, _x340);
  }
  else {
    var _x335 = _x336;
  }
  var _x334 = $std_core_order._lp__excl__eq__rp_(_x335, $std_core_types.Lt);
  var hi = (_x334) ? xx : yy;
  var _x328 = hi.hi;
  var _x327 = (_x328 === (0.0));
  if (_x327) {
    return zero;
  }
  else {
     
    var z = _lp__fs__rp_(lo, hi);
    return _lp__star__rp_(hi, sqrt(_lp__plus__rp_(one, _lp__star__rp_(z, z))));
  }
}
 
 
// The square root of the sum of the squares of three doubles.
// Prevents overflow for large numbers.
export function xyz_fs_hypot(x, y, z) /* (x : ddouble, y : ddouble, z : ddouble) -> ddouble */  {
   
  var xx = abs(x);
   
  var yy = abs(y);
   
  var zz = abs(z);
   
  var _x332 = xx.hi;
  var _x333 = yy.hi;
  var _x331 = $std_num_float64.cmp(_x332, _x333);
  if (_x331 === 2) {
    var _x334 = xx.lo;
    var _x335 = yy.lo;
    var _x330 = $std_num_float64.cmp(_x334, _x335);
  }
  else {
    var _x330 = _x331;
  }
  var _x329 = $std_core_order._lp__excl__eq__rp_(_x330, $std_core_types.Lt);
  var x_0_10393 = (_x329) ? xx : yy;
   
  var _x339 = x_0_10393.hi;
  var _x340 = zz.hi;
  var _x338 = $std_num_float64.cmp(_x339, _x340);
  if (_x338 === 2) {
    var _x341 = x_0_10393.lo;
    var _x342 = zz.lo;
    var _x337 = $std_num_float64.cmp(_x341, _x342);
  }
  else {
    var _x337 = _x338;
  }
  var _x336 = $std_core_order._lp__excl__eq__rp_(_x337, $std_core_types.Lt);
  var hi = (_x336) ? x_0_10393 : zz;
  var _x330 = hi.hi;
  var _x329 = (_x330 === (0.0));
  if (_x329) {
    return zero;
  }
  else {
    return _lp__star__rp_(hi, sqrt(_lp__plus__rp_(_lp__plus__rp_(sqr(_lp__fs__rp_(xx, hi)), sqr(_lp__fs__rp_(yy, hi))), sqr(_lp__fs__rp_(zz, hi)))));
  }
}
 
 
// The maximum of the absolute values.
export function abs_max(x, y) /* (x : ddouble, y : ddouble) -> ddouble */  {
   
  var x_0_10404 = abs(x);
   
  var y_0_10405 = abs(y);
  var _x334 = x_0_10404.hi;
  var _x335 = y_0_10405.hi;
  var _x333 = $std_num_float64.cmp(_x334, _x335);
  if (_x333 === 2) {
    var _x336 = x_0_10404.lo;
    var _x337 = y_0_10405.lo;
    var _x332 = $std_num_float64.cmp(_x336, _x337);
  }
  else {
    var _x332 = _x333;
  }
  var _x331 = $std_core_order._lp__excl__eq__rp_(_x332, $std_core_types.Lt);
  return (_x331) ? x_0_10404 : y_0_10405;
}
 
 
// The maximum of a list of absolute values.
export function list_fs_abs_max(xs) /* (xs : list<ddouble>) -> ddouble */  {
  return $std_core_list.foldr(xs, zero, function(x /* ddouble */ , m /* ddouble */ ) {
       
      var x_0_10408 = abs(x);
      var _x341 = x_0_10408.hi;
      var _x342 = m.hi;
      var _x340 = $std_num_float64.cmp(_x341, _x342);
      if (_x340 === 2) {
        var _x343 = x_0_10408.lo;
        var _x344 = m.lo;
        var _x339 = $std_num_float64.cmp(_x343, _x344);
      }
      else {
        var _x339 = _x340;
      }
      var _x338 = $std_core_order._lp__excl__eq__rp_(_x339, $std_core_types.Lt);
      return (_x338) ? x_0_10408 : m;
    });
}
 
 
// The square root of the sum of squares of a list of doubles.
// Prevents overflow for large numbers and uses Kahan-Babu&scaron;kan-Neumaier summation
// for precision.
export function list_fs_hypot(xs) /* (xs : list<ddouble>) -> ddouble */  {
   
  var hi = list_fs_abs_max(xs);
  var _x346 = hi.hi;
  var _x345 = (_x346 === (0.0));
  if (_x345) {
    return zero;
  }
  else {
    return _lp__star__rp_(hi, sqrt(list_fs_sum($std_core_list.map(xs, function(x_0 /* ddouble */ ) {
          return sqr(_lp__fs__rp_(x_0, hi));
        }))));
  }
}
 
export var ch_factors;
var ch_factors = $std_core_types.Cons(Ddouble(1.6056491947130062e-10, 6.1925234565562596e-27), $std_core_types.Cons(Ddouble((-2.50521080522083e-8), (-3.659819502286579e-25)), $std_core_types.Cons(Ddouble(2.755731922396444e-6, (-2.0315661398415507e-22)), $std_core_types.Cons(Ddouble((-1.984126984126984e-4), 6.857728908107508e-21), $std_core_types.Cons(Ddouble(8.333333333333333e-3, 1.1563735775184918e-19), $std_core_types.Cons(Ddouble((-0.16666666666666666), (-9.251858532166303e-18)), $std_core_types.Cons(Ddouble(1.0, (-6.023956771240347e-31)), $std_core_types.Nil)))))));
 
export var sin16_table;
var sin16_table = $std_core_vector.unvlist($std_core_types.Cons(zero, $std_core_types.Cons(Ddouble(0.19509032201612828, (-7.991079068461731e-18)), $std_core_types.Cons(Ddouble(0.3826834323650898, (-1.0050772696461588e-17)), $std_core_types.Cons(Ddouble(0.5555702330196022, 4.709410940561677e-17), $std_core_types.Cons(Ddouble(0.7071067811865476, (-4.833646656726456e-17)), $std_core_types.Cons(Ddouble(0.8314696123025452, 1.40738569847281e-18), $std_core_types.Cons(Ddouble(0.9238795325112867, 1.7645047084336683e-17), $std_core_types.Cons(Ddouble(0.9807852804032304, 1.8546939997825015e-17), $std_core_types.Cons(one, $std_core_types.Nil))))))))));
 
 
// Return sin(i*pi/16) for 0 <= i <= 8
export function sin16(i) /* (i : int) -> ddouble */  {
   
  var m_10416 = $std_core_vector.at(sin16_table, i);
  return (m_10416 === null) ? dd_nan : m_10416.value;
}
 
 
// Calculate sine and cosine on an angle in radians.
export function sincos(rad) /* (rad : ddouble) -> (ddouble, ddouble) */  {
  var _x348 = rad.hi;
  var _x347 = ((Math.abs(_x348)) < (1.0e-11));
  if (_x347) {
     
    var y_10420 = _lp__fs__rp_(sqr(rad), ddouble_int_exp(3, 0));
     
    var _x349 = y_10420.hi;
    var _x350 = y_10420.lo;
    var s = _lp__star__rp_(rad, _lp__plus__rp_(one, Ddouble((-_x349), (-_x350))));
     
    var y_0_10423 = sqr(s);
     
    var _x351 = y_0_10423.hi;
    var _x352 = y_0_10423.lo;
    var c = sqrt(_lp__plus__rp_(one, Ddouble((-_x351), (-_x352))));
    return $std_core_types.Tuple2(s, c);
  }
  else {
     
    var x1 = _lp__fs__rp_(rad, dd_twopi);
     
    var y_1_10425 = round(x1);
     
    var _x349 = y_1_10425.hi;
    var _x350 = y_1_10425.lo;
    var x3 = _lp__plus__rp_(x1, Ddouble((-_x349), (-_x350)));
     
    var x32 = _lp__plus__rp_(x3, x3);
     
    var x34 = _lp__plus__rp_(x32, x32);
     
    var _x352 = x34.hi;
    var _x351 = isFinite(_x352);
    if (_x351) {
      var _x353 = x34.lo;
      var b_10428 = isFinite(_x353);
    }
    else {
      var b_10428 = false;
    }
     
    if (b_10428) {
      var a = $std_num_decimal.int(decimal(round(x34)));
    }
    else {
      var _x354 = undefined;
      var a = (_x354 !== undefined) ? _x354 : 0;
    }
     
    var y_2_10433 = ddouble_int_exp(a, 0);
     
    var _x355 = y_2_10433.hi;
    var _x356 = y_2_10433.lo;
    var x_3_10429 = _lp__star__rp_(ddouble_int_exp(8, 0), _lp__plus__rp_(x34, Ddouble((-_x355), (-_x356))));
     
    var _x358 = x_3_10429.hi;
    var _x357 = isFinite(_x358);
    if (_x357) {
      var _x359 = x_3_10429.lo;
      var b_0_10435 = isFinite(_x359);
    }
    else {
      var b_0_10435 = false;
    }
     
    if (b_0_10435) {
      var b_1 = $std_num_decimal.int(decimal(round(x_3_10429)));
    }
    else {
      var _x360 = undefined;
      var b_1 = (_x360 !== undefined) ? _x360 : 0;
    }
     
    var x_6_10439 = $std_core_types._int_mul(8,a);
     
    var i_2_10438 = $std_core_types._int_add(x_6_10439,b_1);
     
    var y_3_10437 = _lp__fs__rp_(ddouble_int_exp(i_2_10438, 0), ddouble_int_exp(16, 0));
     
    var _x361 = y_3_10437.hi;
    var _x362 = y_3_10437.lo;
    var s_0 = _lp__star__rp_(dd_pi, _lp__plus__rp_(x32, Ddouble((-_x361), (-_x362))));
     
    var s2 = sqr(s_0);
     
    var sins = _lp__star__rp_(s_0, $std_core_list.foldl(ch_factors, zero, function(acc /* ddouble */ , f /* ddouble */ ) {
          return _lp__plus__rp_(f, _lp__star__rp_(acc, s2));
        }));
     
    var y_5_10443 = sqr(sins);
     
    var _x363 = y_5_10443.hi;
    var _x364 = y_5_10443.lo;
    var coss = sqrt(_lp__plus__rp_(one, Ddouble((-_x363), (-_x364))));
     
    if ($std_core_types._int_ge(b_1,0)) {
       
      var m_10445 = $std_core_vector.at(sin16_table, b_1);
      var sinb = (m_10445 === null) ? dd_nan : m_10445.value;
    }
    else {
       
      var i_5_10448 = $std_core_types._int_negate(b_1);
       
      var m_0_10449 = $std_core_vector.at(sin16_table, i_5_10448);
      var _x366 = (m_0_10449 === null) ? dd_nan : m_0_10449.value;
      var _x365 = _x366.hi;
      var _x368 = (m_0_10449 === null) ? dd_nan : m_0_10449.value;
      var _x367 = _x368.lo;
      var sinb = Ddouble((-_x365), (-_x367));
    }
     
    var y_6_10455 = $std_core_types._int_abs(b_1);
     
    var i_6_10453 = $std_core_types._int_sub(8,y_6_10455);
     
    var m_1_10456 = $std_core_vector.at(sin16_table, i_6_10453);
    if ($std_core_types._int_eq(a,0)) {
       
      var _x349 = (m_1_10456 === null) ? dd_nan : m_1_10456.value;
      var x_13_10458 = _lp__star__rp_(coss, _x349);
       
      var y_7_10459 = _lp__star__rp_(sins, sinb);
      var _x349 = (m_1_10456 === null) ? dd_nan : m_1_10456.value;
      var _x350 = y_7_10459.hi;
      var _x351 = y_7_10459.lo;
      return $std_core_types.Tuple2(_lp__plus__rp_(_lp__star__rp_(sins, _x349), _lp__star__rp_(coss, sinb)), _lp__plus__rp_(x_13_10458, Ddouble((-_x350), (-_x351))));
    }
    else {
      if ($std_core_types._int_eq(a,1)) {
         
        var _x352 = (m_1_10456 === null) ? dd_nan : m_1_10456.value;
        var x_14_10460 = _lp__star__rp_(coss, _x352);
         
        var y_8_10461 = _lp__star__rp_(sins, sinb);
         
        var _x353 = coss.hi;
        var _x354 = coss.lo;
        var x_15_10462 = _lp__star__rp_(Ddouble((-_x353), (-_x354)), sinb);
         
        var _x355 = (m_1_10456 === null) ? dd_nan : m_1_10456.value;
        var y_9_10463 = _lp__star__rp_(sins, _x355);
        var _x352 = y_8_10461.hi;
        var _x353 = y_8_10461.lo;
        var _x354 = y_9_10463.hi;
        var _x355 = y_9_10463.lo;
        return $std_core_types.Tuple2(_lp__plus__rp_(x_14_10460, Ddouble((-_x352), (-_x353))), _lp__plus__rp_(x_15_10462, Ddouble((-_x354), (-_x355))));
      }
      else {
        if ($std_core_types._int_eq(a,(-1))) {
           
          var x_16_10464 = _lp__star__rp_(sins, sinb);
           
          var _x356 = (m_1_10456 === null) ? dd_nan : m_1_10456.value;
          var y_10_10465 = _lp__star__rp_(coss, _x356);
          var _x356 = y_10_10465.hi;
          var _x357 = y_10_10465.lo;
          var _x358 = (m_1_10456 === null) ? dd_nan : m_1_10456.value;
          return $std_core_types.Tuple2(_lp__plus__rp_(x_16_10464, Ddouble((-_x356), (-_x357))), _lp__plus__rp_(_lp__star__rp_(coss, sinb), _lp__star__rp_(sins, _x358)));
        }
        else {
           
          var _x359 = sins.hi;
          var _x360 = sins.lo;
          var _x361 = (m_1_10456 === null) ? dd_nan : m_1_10456.value;
          var x_17_10466 = _lp__star__rp_(Ddouble((-_x359), (-_x360)), _x361);
           
          var y_11_10467 = _lp__star__rp_(coss, sinb);
           
          var x_18_10468 = _lp__star__rp_(sins, sinb);
           
          var _x362 = (m_1_10456 === null) ? dd_nan : m_1_10456.value;
          var y_12_10469 = _lp__star__rp_(coss, _x362);
          var _x359 = y_11_10467.hi;
          var _x360 = y_11_10467.lo;
          var _x361 = y_12_10469.hi;
          var _x362 = y_12_10469.lo;
          return $std_core_types.Tuple2(_lp__plus__rp_(x_17_10466, Ddouble((-_x359), (-_x360))), _lp__plus__rp_(x_18_10468, Ddouble((-_x361), (-_x362))));
        }
      }
    }
  }
}
 
 
// The sine function of a given angle in radians.
export function sin(rad) /* (rad : ddouble) -> ddouble */  {
   
  var tuple2_10470 = sincos(rad);
  return tuple2_10470.fst;
}
 
 
// The cosine function of a given angle in radians.
export function cos(rad) /* (rad : ddouble) -> ddouble */  {
   
  var tuple2_10471 = sincos(rad);
  return tuple2_10471.snd;
}
 
 
// The tangent of a given angle in radians.
export function tan(rad) /* (rad : ddouble) -> ddouble */  {
  var _x363 = sincos(rad);
  return _lp__fs__rp_(_x363.fst, _x363.snd);
}
 
 
// Return `x` with the sign of `y`.
export function with_sign_of(x, y) /* (x : ddouble, y : ddouble) -> ddouble */  {
  var _x365 = y.hi;
  var _x364 = (_x365 < (0.0));
  if (_x364) {
     
    var x_1_10475 = abs(x);
    var _x366 = x_1_10475.hi;
    var _x367 = x_1_10475.lo;
    return Ddouble((-_x366), (-_x367));
  }
  else {
    return abs(x);
  }
}
 
 
// The arc-tangent of a point (`x`,`y`). Returns the angle with respect to the x-axis in radians between -&pi; and &pi;.
export function atan2(y, x) /* (y : ddouble, x : ddouble) -> ddouble */  {
  var _x369 = x.hi;
  var _x368 = (_x369 === (0.0));
  if (_x368) {
    var _x371 = y.hi;
    var _x370 = (_x371 === (0.0));
    if (_x370) {
      return zero;
    }
    else {
      return with_sign_of(dd_pi2, y);
    }
  }
  else {
    var _x373 = y.hi;
    var _x372 = (_x373 === (0.0));
    if (_x372) {
      var _x375 = x.hi;
      var _x374 = (_x375 > (0.0));
      return (_x374) ? zero : dd_pi;
    }
    else {
      var _x379 = x.hi;
      var _x380 = y.hi;
      var _x378 = $std_num_float64.cmp(_x379, _x380);
      if (_x378 === 2) {
        var _x381 = x.lo;
        var _x382 = y.lo;
        var _x377 = $std_num_float64.cmp(_x381, _x382);
      }
      else {
        var _x377 = _x378;
      }
      var _x376 = $std_core_order._lp__eq__eq__rp_(_x377, $std_core_types.Eq);
      if (_x376) {
        var _x384 = y.hi;
        var _x383 = (_x384 > (0.0));
        if (_x383) {
          return dd_pi4;
        }
        else {
          var _x385 = dd_pi34.hi;
          var _x386 = dd_pi34.lo;
          return Ddouble((-_x385), (-_x386));
        }
      }
      else {
         
        var _x387 = y.hi;
        var _x388 = y.lo;
        var y_1_10496 = Ddouble((-_x387), (-_x388));
        var _x390 = x.hi;
        var _x391 = y_1_10496.hi;
        var _x389 = $std_num_float64.cmp(_x390, _x391);
        if (_x389 === 2) {
          var _x392 = x.lo;
          var _x393 = y_1_10496.lo;
          var _x388 = $std_num_float64.cmp(_x392, _x393);
        }
        else {
          var _x388 = _x389;
        }
        var _x387 = $std_core_order._lp__eq__eq__rp_(_x388, $std_core_types.Eq);
        if (_x387) {
          var _x395 = y.hi;
          var _x394 = (_x395 > (0.0));
          if (_x394) {
            return dd_pi34;
          }
          else {
            var _x396 = dd_pi4.hi;
            var _x397 = dd_pi4.lo;
            return Ddouble((-_x396), (-_x397));
          }
        }
        else {
           
          var r = sqrt(_lp__plus__rp_(sqr(x), sqr(y)));
           
          var xr = _lp__fs__rp_(x, r);
           
          var yr = _lp__fs__rp_(y, r);
           
          var _x398 = y.hi;
          var _x399 = x.hi;
          var d_5_10500 = Math.atan2(_x398,_x399);
           
          var z = Ddouble(d_5_10500, 0.0);
          var _x398 = sincos(z);
           
          var ddouble_0_8_10506 = abs(yr);
          var _x400 = xr.hi;
          var _x401 = ddouble_0_8_10506.hi;
          var _x399 = ((Math.abs(_x400)) > _x401);
          if (_x399) {
            var _x402 = _x398.fst.hi;
            var _x403 = _x398.fst.lo;
            return _lp__plus__rp_(z, _lp__fs__rp_(_lp__plus__rp_(yr, Ddouble((-_x402), (-_x403))), _x398.snd));
          }
          else {
             
            var _x404 = _x398.snd.hi;
            var _x405 = _x398.snd.lo;
            var y_3_10510 = _lp__fs__rp_(_lp__plus__rp_(xr, Ddouble((-_x404), (-_x405))), _x398.fst);
            var _x404 = y_3_10510.hi;
            var _x405 = y_3_10510.lo;
            return _lp__plus__rp_(z, Ddouble((-_x404), (-_x405)));
          }
        }
      }
    }
  }
}
 
 
// The arc-sine of `x`. Returns the angle in radians.
export function asin(x) /* (x : ddouble) -> ddouble */  {
   
  var a = abs(x);
  var _x409 = a.hi;
  var _x410 = one.hi;
  var _x408 = $std_num_float64.cmp(_x409, _x410);
  if (_x408 === 2) {
    var _x411 = a.lo;
    var _x412 = one.lo;
    var _x407 = $std_num_float64.cmp(_x411, _x412);
  }
  else {
    var _x407 = _x408;
  }
  var _x406 = $std_core_order._lp__eq__eq__rp_(_x407, $std_core_types.Gt);
  if (_x406) {
    return dd_nan;
  }
  else {
    var _x416 = a.hi;
    var _x417 = one.hi;
    var _x415 = $std_num_float64.cmp(_x416, _x417);
    if (_x415 === 2) {
      var _x418 = a.lo;
      var _x419 = one.lo;
      var _x414 = $std_num_float64.cmp(_x418, _x419);
    }
    else {
      var _x414 = _x415;
    }
    var _x413 = $std_core_order._lp__eq__eq__rp_(_x414, $std_core_types.Eq);
    if (_x413) {
      return with_sign_of(dd_pi2, x);
    }
    else {
       
      var y_1_10518 = sqr(x);
      var _x420 = y_1_10518.hi;
      var _x421 = y_1_10518.lo;
      return atan2(x, sqrt(_lp__plus__rp_(one, Ddouble((-_x420), (-_x421)))));
    }
  }
}
 
 
// The arc-cosine of `x`. Returns the angle in radians.
export function acos(x) /* (x : ddouble) -> ddouble */  {
   
  var a = abs(x);
  var _x425 = a.hi;
  var _x426 = one.hi;
  var _x424 = $std_num_float64.cmp(_x425, _x426);
  if (_x424 === 2) {
    var _x427 = a.lo;
    var _x428 = one.lo;
    var _x423 = $std_num_float64.cmp(_x427, _x428);
  }
  else {
    var _x423 = _x424;
  }
  var _x422 = $std_core_order._lp__eq__eq__rp_(_x423, $std_core_types.Gt);
  if (_x422) {
    return dd_nan;
  }
  else {
    var _x432 = a.hi;
    var _x433 = one.hi;
    var _x431 = $std_num_float64.cmp(_x432, _x433);
    if (_x431 === 2) {
      var _x434 = a.lo;
      var _x435 = one.lo;
      var _x430 = $std_num_float64.cmp(_x434, _x435);
    }
    else {
      var _x430 = _x431;
    }
    var _x429 = $std_core_order._lp__eq__eq__rp_(_x430, $std_core_types.Eq);
    if (_x429) {
      var _x437 = x.hi;
      var _x436 = (_x437 > (0.0));
      return (_x436) ? zero : dd_pi;
    }
    else {
       
      var y_1_10527 = sqr(x);
      var _x438 = y_1_10527.hi;
      var _x439 = y_1_10527.lo;
      return atan2(sqrt(_lp__plus__rp_(one, Ddouble((-_x438), (-_x439)))), x);
    }
  }
}
 
 
// The arc-tangent of `x`. Returns the angle in radians.
export function atan(x) /* (x : ddouble) -> ddouble */  {
  return atan2(x, one);
}
 
 
// The hyperbolic sine of `x`.
export function sinh(x) /* (x : ddouble) -> ddouble */  {
  var _x441 = x.hi;
  var _x440 = (_x441 === (0.0));
  if (_x440) {
    return zero;
  }
  else {
     
    var x_1_10531 = abs(x);
    var _x443 = x_1_10531.hi;
    var _x442 = (_x443 > (5.0e-2));
    if (_x442) {
       
      var ex = exp(x);
       
      var _x445 = ex.hi;
      var _x444 = isFinite(_x445);
      if (_x444) {
        var _x446 = ex.lo;
        var b_10533 = isFinite(_x446);
      }
      else {
        var b_10533 = false;
      }
      if (b_10533) {
         
        var y_10536 = _lp__fs__rp_(one, ex);
         
        var _x444 = y_10536.hi;
        var _x445 = y_10536.lo;
        var x_2_10534 = _lp__plus__rp_(ex, Ddouble((-_x444), (-_x445)));
        var _x444 = x_2_10534.hi;
        var _x445 = x_2_10534.lo;
        return Ddouble((_x444 * (0.5)), (_x445 * (0.5)));
      }
      else {
        return ex;
      }
    }
    else {
       
      var x2 = sqr(x);
      return _lp__star__rp_(x, _lp__plus__rp_(one, _lp__star__rp_(_lp__fs__rp_(x2, ddouble_int_exp(6, 0)), _lp__plus__rp_(one, _lp__star__rp_(_lp__fs__rp_(x2, ddouble_int_exp(20, 0)), _lp__plus__rp_(one, _lp__fs__rp_(x2, ddouble_int_exp(42, 0))))))));
    }
  }
}
 
 
// The hyperbolic cosine of `x`.
export function cosh(x) /* (x : ddouble) -> ddouble */  {
  var _x447 = x.hi;
  var _x446 = (_x447 === (0.0));
  if (_x446) {
    return one;
  }
  else {
     
    var x_1_10547 = abs(x);
    var _x449 = x_1_10547.hi;
    var _x448 = (_x449 > (5.0e-2));
    if (_x448) {
       
      var ex = exp(x);
       
      var _x451 = ex.hi;
      var _x450 = isFinite(_x451);
      if (_x450) {
        var _x452 = ex.lo;
        var b_10549 = isFinite(_x452);
      }
      else {
        var b_10549 = false;
      }
      if (b_10549) {
         
        var x_2_10550 = _lp__plus__rp_(ex, _lp__fs__rp_(one, ex));
        var _x450 = x_2_10550.hi;
        var _x451 = x_2_10550.lo;
        return Ddouble((_x450 * (0.5)), (_x451 * (0.5)));
      }
      else {
        return ex;
      }
    }
    else {
       
      var s = sinh(x);
      return sqrt(_lp__plus__rp_(one, sqr(s)));
    }
  }
}
 
 
// The hyperbolic tangent of `x`.
export function tanh(x) /* (x : ddouble) -> ddouble */  {
  var _x453 = x.hi;
  var _x452 = (_x453 === (0.0));
  if (_x452) {
    return zero;
  }
  else {
     
    var x_1_10558 = abs(x);
    var _x455 = x_1_10558.hi;
    var _x454 = (_x455 > (5.0e-2));
    if (_x454) {
       
      var ex = exp(x);
       
      var iex = _lp__fs__rp_(one, ex);
      var _x457 = ex.hi;
      var _x456 = (_x457 === (0.0));
      if (_x456) {
        var _x458 = one.hi;
        var _x459 = one.lo;
        return Ddouble((-_x458), (-_x459));
      }
      else {
        var _x461 = ex.hi;
        var _x460 = ((_x461) === Infinity);
        if (_x460) {
          return one;
        }
        else {
          var _x462 = iex.hi;
          var _x463 = iex.lo;
          return _lp__fs__rp_(_lp__plus__rp_(ex, Ddouble((-_x462), (-_x463))), _lp__plus__rp_(ex, iex));
        }
      }
    }
    else {
       
      var s = sinh(x);
       
      var c = sqrt(_lp__plus__rp_(one, sqr(s)));
      return _lp__fs__rp_(s, c);
    }
  }
}
 
 
// The area hyperbolic sine of `x`.
export function asinh(x) /* (x : ddouble) -> ddouble */  {
  return ln(_lp__plus__rp_(x, sqrt(_lp__plus__rp_(sqr(x), one))));
}
 
 
// The area hyperbolic cosine of `x`.
export function acosh(x) /* (x : ddouble) -> ddouble */  {
  var _x467 = x.hi;
  var _x468 = one.hi;
  var _x466 = $std_num_float64.cmp(_x467, _x468);
  if (_x466 === 2) {
    var _x469 = x.lo;
    var _x470 = one.lo;
    var _x465 = $std_num_float64.cmp(_x469, _x470);
  }
  else {
    var _x465 = _x466;
  }
  var _x464 = $std_core_order._lp__eq__eq__rp_(_x465, $std_core_types.Lt);
  if (_x464) {
    return dd_nan;
  }
  else {
     
    var x_1_10569 = sqr(x);
    var _x471 = one.hi;
    var _x472 = one.lo;
    return ln(_lp__plus__rp_(x, sqrt(_lp__plus__rp_(x_1_10569, Ddouble((-_x471), (-_x472))))));
  }
}
 
 
// The area hyperbolic tangent of `x`.
export function atanh(x) /* (x : ddouble) -> ddouble */  {
   
  var x_0_10571 = abs(x);
  var _x476 = x_0_10571.hi;
  var _x477 = one.hi;
  var _x475 = $std_num_float64.cmp(_x476, _x477);
  if (_x475 === 2) {
    var _x478 = x_0_10571.lo;
    var _x479 = one.lo;
    var _x474 = $std_num_float64.cmp(_x478, _x479);
  }
  else {
    var _x474 = _x475;
  }
  var _x473 = $std_core_order._lp__eq__eq__rp_(_x474, $std_core_types.Gt);
  if (_x473) {
    return dd_nan;
  }
  else {
     
    var _x480 = x.hi;
    var _x481 = x.lo;
    var x_1_10573 = ln(_lp__fs__rp_(_lp__plus__rp_(one, x), _lp__plus__rp_(one, Ddouble((-_x480), (-_x481)))));
    var _x480 = x_1_10573.hi;
    var _x481 = x_1_10573.lo;
    return Ddouble((_x480 * (0.5)), (_x481 * (0.5)));
  }
}