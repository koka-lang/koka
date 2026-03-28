// Koka generated module: std/num/decimal, koka version: 3.2.4
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
import * as $std_text_parse from './std_text_parse.mjs';
import * as $std_num_float64 from './std_num_float64.mjs';
 
// externals
 
// type declarations
// type decimal
export function Decimal(num, exp) /* (num : int, exp : int) -> decimal */  {
  return { num: num, exp: exp };
}
// type round
export const Half_even = 1; // round
export const Half_ceiling = 2; // round
export const Half_floor = 3; // round
export const Half_truncate = 4; // round
export const Half_away_from_zero = 5; // round
export const Ceiling = 6; // round
export const Floor = 7; // round
export const Truncate = 8; // round
export const Away_from_zero = 9; // round
 
// declarations
 
 
// Automatically generated. Retrieves the `num` constructor field of the `:decimal` type.
export function decimal_fs_num(decimal) /* (decimal : decimal) -> int */  {
  return decimal.num;
}
 
 
// Automatically generated. Retrieves the `exp` constructor field of the `:decimal` type.
export function decimal_fs_exp(decimal) /* (decimal : decimal) -> int */  {
  return decimal.exp;
}
 
export function decimal_fs__copy(_this, num, exp) /* (decimal, num : ? int, exp : ? int) -> decimal */  {
  if (num !== undefined) {
    var _x0 = num;
  }
  else {
    var _x0 = _this.num;
  }
  if (exp !== undefined) {
    var _x1 = exp;
  }
  else {
    var _x1 = _this.exp;
  }
  return Decimal(_x0, _x1);
}
 
 
// The decimal zero.
export var zero;
var zero = Decimal(0, 0);
 
 
// Negate a decimal.
export function _lp__tilde__rp_(x) /* (x : decimal) -> decimal */  {
  var _x2 = x.num;
  var _x3 = x.exp;
  return Decimal($std_core_types._int_negate(_x2), _x3);
}
 
 
// Is this decimal zero?
export function is_zero(x) /* (x : decimal) -> bool */  {
  var _x4 = x.num;
  return $std_core_types._int_iszero(_x4);
}
 
 
// Is the decimal positive?
export function is_pos(x) /* (x : decimal) -> bool */  {
  var _x5 = x.num;
  return $std_core_types._int_gt(_x5,0);
}
 
 
// Is the decimal negative?
export function is_neg(x) /* (x : decimal) -> bool */  {
  var _x6 = x.num;
  return $std_core_types._int_lt(_x6,0);
}
 
 
// The absolute value of a decimal
export function abs(x) /* (x : decimal) -> decimal */  {
  var _x8 = x.num;
  var _x7 = $std_core_types._int_lt(_x8,0);
  if (_x7) {
    var _x9 = x.num;
    var _x10 = x.exp;
    return Decimal($std_core_types._int_negate(_x9), _x10);
  }
  else {
    return x;
  }
}
 
 
// round exponents to specific intervals (7) to avoid too much rescaling
export function round_exp(exp) /* (exp : int) -> int */  {
  if ($std_core_types._int_iszero(exp)) {
    return 0;
  }
  else {
    return $std_core_types._int_mul(7,($std_core_types._int_div(exp,7)));
  }
}
 
 
// Create a decimal from an integer `i` with an optional
// exponent `exp` (=`0`) such that the result equals `i`&times;10^`exp`^.
export function decimal_exp(i, exp) /* (i : int, exp : ? int) -> decimal */  {
   
  var _x12 = (exp !== undefined) ? exp : 0;
  var _x11 = $std_core_types._int_iszero(_x12);
  if (_x11) {
    var x = 0;
  }
  else {
    var _x13 = (exp !== undefined) ? exp : 0;
    var x = $std_core_types._int_mul(7,($std_core_types._int_div(_x13,7)));
  }
   
  var _x14 = (exp !== undefined) ? exp : 0;
  var diff = $std_core_types._int_sub(_x14,x);
  if ($std_core_types._int_iszero(diff)) {
    var _x11 = (exp !== undefined) ? exp : 0;
    return Decimal(i, _x11);
  }
  else {
    return Decimal($std_core_int.mul_exp10(i, $std_core_types._int_abs(diff)), x);
  }
}
 
 
// Ensure a decimal `x` has an exponent such that `x.exp <= e`.
export function expand(x, e) /* (x : decimal, e : int) -> decimal */  {
  var _x13 = x.exp;
  var _x12 = $std_core_types._int_le(_x13,e);
  if (_x12) {
    return x;
  }
  else {
    var _x14 = x.num;
    var _x15 = x.exp;
    return decimal_exp($std_core_int.mul_exp10(_x14, $std_core_types._int_sub(_x15,e)), e);
  }
}
 
 
// Compare decimals.
export function cmp(x, y) /* (x : decimal, y : decimal) -> order */  {
   
  var _x17 = x.exp;
  var _x18 = y.exp;
  var _x16 = $std_core_types._int_le(_x17,_x18);
  if (_x16) {
    var e = x.exp;
  }
  else {
    var e = y.exp;
  }
   
  var xx = expand(x, e);
   
  var yy = expand(y, e);
  var _x17 = xx.num;
  var _x18 = yy.num;
  var _x16 = $std_core_types._int_eq(_x17,_x18);
  if (_x16) {
    return $std_core_types.Eq;
  }
  else {
    var _x20 = xx.num;
    var _x21 = yy.num;
    var _x19 = $std_core_types._int_gt(_x20,_x21);
    return (_x19) ? $std_core_types.Gt : $std_core_types.Lt;
  }
}
 
export function _lp__excl__eq__rp_(x, y) /* (x : decimal, y : decimal) -> bool */  {
  return $std_core_order._lp__excl__eq__rp_(cmp(x, y), $std_core_types.Eq);
}
 
 
// Choose an exponent that minimizes memory usage.
export function reduce(x) /* (x : decimal) -> decimal */  {
   
  var _x22 = x.num;
  var p = $std_core_int.is_exp10(_x22);
   
  var b_10032 = $std_core_types._int_gt(p,0);
  if (b_10032) {
     
    var _x22 = x.exp;
    var expp = $std_core_types._int_add(_x22,p);
    if ($std_core_types._int_iszero(expp)) {
      var _x23 = 0;
    }
    else {
      var _x23 = $std_core_types._int_mul(7,($std_core_types._int_div(expp,7)));
    }
    var _x24 = x.exp;
    var _x22 = $std_core_types._int_eq(_x23,_x24);
    if (_x22) {
      return x;
    }
    else {
      var _x25 = x.num;
      return decimal_exp($std_core_int.cdiv_exp10(_x25, p), expp);
    }
  }
  else {
    return x;
  }
}
 
 
// Multiply two decimals with full precision.
export function _lp__star__rp_(x, y) /* (x : decimal, y : decimal) -> decimal */  {
   
  var _x26 = x.num;
  var _x27 = y.num;
  var _x28 = x.exp;
  var _x29 = y.exp;
  var z = decimal_exp($std_core_types._int_mul(_x26,_x27), $std_core_types._int_add(_x28,_x29));
  var _x27 = z.exp;
  var _x26 = $std_core_types._int_lt(_x27,0);
  if (_x26) {
    return reduce(z);
  }
  else {
    return z;
  }
}
 
 
// Add two decimals.
export function _lp__plus__rp_(x, y) /* (x : decimal, y : decimal) -> decimal */  {
   
  var _x29 = x.exp;
  var _x30 = y.exp;
  var _x28 = $std_core_types._int_le(_x29,_x30);
  if (_x28) {
    var e = x.exp;
  }
  else {
    var e = y.exp;
  }
   
  var xx = expand(x, e);
   
  var yy = expand(y, e);
  var _x28 = xx.num;
  var _x29 = yy.num;
  return Decimal($std_core_types._int_add(_x28,_x29), e);
}
 
 
// Subtract two decimals.
export function _lp__dash__rp_(x, y) /* (x : decimal, y : decimal) -> decimal */  {
   
  var _x30 = y.num;
  var _x31 = y.exp;
  var y_0_10225 = Decimal($std_core_types._int_negate(_x30), _x31);
   
  var _x33 = x.exp;
  var _x34 = y_0_10225.exp;
  var _x32 = $std_core_types._int_le(_x33,_x34);
  if (_x32) {
    var e = x.exp;
  }
  else {
    var e = y_0_10225.exp;
  }
   
  var xx = expand(x, e);
   
  var yy = expand(y_0_10225, e);
  var _x30 = xx.num;
  var _x31 = yy.num;
  return Decimal($std_core_types._int_add(_x30,_x31), e);
}
 
 
/* Divide two decimals with a given extra precision `min-prec` (=`15`).
The `min-prec` is the number of extra digits used to calculate inexact
divisions.
Note: the division uses up to `min-prec` precision using `Floor` rounding
for the last digit if the result is  inexact. To round differently, you can
for example divide with larger precision and use `round-to-prec`.
```
> div( decimal(2), decimal(3), 0 )
0
> div( decimal(2), decimal(3), 1 )
0.6
> div( decimal(2), decimal(3) )  // default precision is 15
0.6666666666666666
> div( decimal(2), decimal(3) ).round-to-prec(6)
0.666667
```
.
*/
export function div(x, y, min_prec) /* (x : decimal, y : decimal, min-prec : ? int) -> decimal */  {
  var _x33 = x.num;
  var _x32 = $std_core_types._int_iszero(_x33);
  if (_x32) {
    return Decimal(0, 0);
  }
  else {
    var _x35 = y.num;
    var _x34 = $std_core_types._int_iszero(_x35);
    if (_x34) {
      return Decimal(0, 0);
    }
    else {
       
      var _x36 = x.exp;
      var _x37 = y.exp;
      var e = $std_core_types._int_sub(_x36,_x37);
       
      var _x38 = x.num;
      var xdigits = $std_core_int.count_digits(_x38);
       
      var _x39 = y.num;
      var ydigits = $std_core_int.count_digits(_x39);
       
      var j_10072 = $std_core_types._int_sub(ydigits,xdigits);
       
      var x_3_10069 = ($std_core_types._int_ge(0,j_10072)) ? 0 : j_10072;
       
      var _x40 = (min_prec !== undefined) ? min_prec : 15;
      var extra = $std_core_types._int_add(x_3_10069,_x40);
      if ($std_core_types._int_gt(extra,0)) {
        var _x37 = x.num;
        var _x38 = y.num;
        var _x36 = decimal_exp($std_core_types._int_div(($std_core_int.mul_exp10(_x37, extra)),_x38), $std_core_types._int_sub(e,extra));
      }
      else {
        var _x39 = x.num;
        var _x40 = y.num;
        var _x36 = decimal_exp($std_core_types._int_div(_x39,_x40), $std_core_types._int_sub(e,extra));
      }
      return reduce(_x36);
    }
  }
}
 
 
// Divide two decimals using 15 digits of extra precision.
export function _lp__fs__rp_(x, y) /* (x : decimal, y : decimal) -> decimal */  {
  return div(x, y);
}
 
export function _lp__lt__eq__rp_(x, y) /* (x : decimal, y : decimal) -> bool */  {
  return $std_core_order._lp__excl__eq__rp_(cmp(x, y), $std_core_types.Gt);
}
 
export function _lp__eq__eq__rp_(x, y) /* (x : decimal, y : decimal) -> bool */  {
  return $std_core_order._lp__eq__eq__rp_(cmp(x, y), $std_core_types.Eq);
}
 
export function _lp__gt__rp_(x, y) /* (x : decimal, y : decimal) -> bool */  {
  return $std_core_order._lp__eq__eq__rp_(cmp(x, y), $std_core_types.Gt);
}
 
export function _lp__gt__eq__rp_(x, y) /* (x : decimal, y : decimal) -> bool */  {
  return $std_core_order._lp__excl__eq__rp_(cmp(x, y), $std_core_types.Lt);
}
 
 
// The maximum of `x` and `y`
export function max(x, y) /* (x : decimal, y : decimal) -> decimal */  {
  var _x41 = $std_core_order._lp__excl__eq__rp_(cmp(x, y), $std_core_types.Lt);
  return (_x41) ? x : y;
}
 
 
// The minimum of `x` and `y`.
export function min(x, y) /* (x : decimal, y : decimal) -> decimal */  {
  var _x42 = $std_core_order._lp__excl__eq__rp_(cmp(x, y), $std_core_types.Gt);
  return (_x42) ? x : y;
}
 
 
// Create a decimal from an integer `i` with an optional
// exponent `exp` (=`0`) such that the result equals `i`&times;10^`exp`^.
export function int_fs_decimal(i, exp) /* (i : int, exp : ? int) -> decimal */  {
  var _x43 = (exp !== undefined) ? exp : 0;
  return decimal_exp(i, _x43);
}
 
 
// Increment a decimal
export function inc(x) /* (x : decimal) -> decimal */  {
  var _x44 = x.num;
  var _x45 = x.exp;
  return Decimal($std_core_types._int_add(_x44,1), _x45);
}
 
 
// Decrement a decimal
export function dec(x) /* (x : decimal) -> decimal */  {
  var _x46 = x.num;
  var _x47 = x.exp;
  return Decimal($std_core_types._int_sub(_x46,1), _x47);
}
 
 
// Is this an even decimal?
export function is_even(x) /* (x : decimal) -> bool */  {
   
  var _x48 = x.num;
  var b_10014 = $std_core_types._int_isodd(_x48);
  return (b_10014) ? false : true;
}
 
 
// Round the decimal-point number `x` to
// to a specified number of digits behind the dot `prec` (=`0`) with an optional
// rounding mode `rnd` (=`Half-even`). The precision can be negative.\
// `decimal(1,485).round-to-prec(2).show == "1.48"` \
// `decimal(112,49).round-to-prec(-1).show == "110"`
export function round_to_prec(x, prec, rnd) /* (x : decimal, prec : ? int, rnd : ? round) -> decimal */  {
  var _x49 = x.exp;
  var _x50 = (prec !== undefined) ? prec : 0;
  var _x48 = $std_core_types._int_ge(_x49,($std_core_types._int_negate(_x50)));
  if (_x48) {
    return x;
  }
  else {
     
    var cx = reduce(x);
     
    var _x51 = cx.exp;
    var x_0_10096 = $std_core_types._int_negate(_x51);
     
    var _x52 = (prec !== undefined) ? prec : 0;
    var p = $std_core_types._int_sub(x_0_10096,_x52);
     
    var b_10099 = $std_core_types._int_gt(p,0);
    if (b_10099) {
      var _x52 = cx.num;
      var _x51 = $std_core_int.divmod_exp10(_x52, p);
       
      var round_half = function(keep_on_eq /* bool */ ) {
         
        var half = $std_core_types._int_div(($std_core_int.mul_exp10(1, p)),2);
        if ($std_core_types._int_eq((_x51.snd),half)) {
          return (keep_on_eq) ? _x51.fst : $std_core_types._int_add((_x51.fst),1);
        }
        else {
          return ($std_core_types._int_gt((_x51.snd),half)) ? $std_core_types._int_add((_x51.fst),1) : _x51.fst;
        }
      };
       
      if ($std_core_types._int_iszero((_x51.snd))) {
        var q1 = _x51.fst;
      }
      else {
        if (rnd !== undefined) {
          if (rnd === 1) {
             
            var b_10014 = $std_core_types._int_isodd((_x51.fst));
            var _x53 = (b_10014) ? false : true;
            var q1 = round_half(_x53);
          }
          else if (rnd === 3) {
            var q1 = round_half(true);
          }
          else if (rnd === 2) {
            var q1 = round_half(false);
          }
          else if (rnd === 4) {
            var q1 = round_half($std_core_types._int_gt((_x51.fst),0));
          }
          else if (rnd === 5) {
            var q1 = round_half($std_core_types._int_lt((_x51.fst),0));
          }
          else if (rnd === 7) {
            var q1 = _x51.fst;
          }
          else if (rnd === 6) {
            var q1 = $std_core_types._int_add((_x51.fst),1);
          }
          else if (rnd === 8) {
             
            var b_0_10111 = $std_core_types._int_lt((_x51.fst),0);
            var q1 = (b_0_10111) ? $std_core_types._int_add((_x51.fst),1) : _x51.fst;
          }
          else {
             
            var b_1_10114 = $std_core_types._int_gt((_x51.fst),0);
            var q1 = (b_1_10114) ? $std_core_types._int_add((_x51.fst),1) : _x51.fst;
          }
        }
        else {
           
          var b_10014_0 = $std_core_types._int_isodd((_x51.fst));
          var _x54 = (b_10014_0) ? false : true;
          var q1 = round_half(_x54);
        }
      }
      var _x53 = (prec !== undefined) ? prec : 0;
      return decimal_exp(q1, $std_core_types._int_negate(_x53));
    }
    else {
      return cx;
    }
  }
}
 
 
// Round a `:decimal` number to a whole number with an optional rounding mode (=`Half-even`).
export function round(x, rnd) /* (x : decimal, rnd : ? round) -> decimal */  {
  var _x54 = (rnd !== undefined) ? rnd : Half_even;
  return round_to_prec(x, 0, _x54);
}
 
export function _lp__lt__rp_(x, y) /* (x : decimal, y : decimal) -> bool */  {
  return $std_core_order._lp__eq__eq__rp_(cmp(x, y), $std_core_types.Lt);
}
 
 
// Decimal to the power of `n`
export function pow(x, n) /* (x : decimal, n : int) -> decimal */  {
   
  var m = $std_core_types._int_abs(n);
   
  var _x55 = x.num;
  var _x56 = x.exp;
  var y = decimal_exp($std_core_int.pow(_x55, m), $std_core_types._int_mul(_x56,m));
  if ($std_core_types._int_lt(n,0)) {
    var _x56 = undefined;
    var _x55 = (_x56 !== undefined) ? _x56 : 0;
    return div(decimal_exp(1, _x55), y, $std_core_types._int_add(3,m));
  }
  else {
    return y;
  }
}
 
 
/* Create a decimal from a `:float64` with a specified maximal precision (=`-1`).
Use a negative maximal precision to create a decimal that precisely represents the `:float64`.
Note: creating a `:decimal` from a `:float64` may lose precision and give surprising results as many decimal
fractions cannot be represented precisely by a `:float64`.
Also, `decimal(i,exp)` is more efficient and better when when exact representations
are required. For example:
```
> decimal(1.1)
1.100000000000000088817841970012523233890533447265625
> decimal(1.1,17)
1.10000000000000008
> decimal(11,-1)
1.1
```
.
*/
export function float64_fs_decimal(d, max_prec) /* (d : float64, max-prec : ? int) -> decimal */  {
  var _x57 = $std_num_float64.decode(d);
  if ($std_core_types._int_ge((_x57.snd),0)) {
     
    var i_10127 = $std_core_types._int_mul((_x57.fst),($std_core_int.pow(2, _x57.snd)));
    var _x59 = undefined;
    var _x58 = (_x59 !== undefined) ? _x59 : 0;
    return decimal_exp(i_10127, _x58);
  }
  else {
     
    var _x61 = (max_prec !== undefined) ? max_prec : -1;
    var _x60 = $std_core_types._int_lt(_x61,0);
    if (_x60) {
      var prec = $std_core_types._int_negate((_x57.snd));
    }
    else {
       
      var j_10132 = $std_core_types._int_negate((_x57.snd));
      var _x63 = (max_prec !== undefined) ? max_prec : -1;
      var _x62 = $std_core_types._int_le(_x63,j_10132);
      if (_x62) {
        var prec = (max_prec !== undefined) ? max_prec : -1;
      }
      else {
        var prec = j_10132;
      }
    }
    var _x61 = undefined;
    var _x60 = (_x61 !== undefined) ? _x61 : 0;
    var _x63 = undefined;
    var _x62 = (_x63 !== undefined) ? _x63 : 0;
    return div(decimal_exp(_x57.fst, _x60), pow(decimal_exp(2, _x62), $std_core_types._int_negate((_x57.snd))), prec);
  }
}
 
 
// Is this an odd decimal?
export function is_odd(x) /* (x : decimal) -> bool */  {
  var _x64 = x.num;
  return $std_core_types._int_isodd(_x64);
}
 
 
// Automatically generated. Tests for the `Half-even` constructor of the `:round` type.
export function is_half_even(round_0) /* (round : round) -> bool */  {
  return (round_0 === 1);
}
 
 
// Automatically generated. Tests for the `Half-ceiling` constructor of the `:round` type.
export function is_half_ceiling(round_0) /* (round : round) -> bool */  {
  return (round_0 === 2);
}
 
 
// Automatically generated. Tests for the `Half-floor` constructor of the `:round` type.
export function is_half_floor(round_0) /* (round : round) -> bool */  {
  return (round_0 === 3);
}
 
 
// Automatically generated. Tests for the `Half-truncate` constructor of the `:round` type.
export function is_half_truncate(round_0) /* (round : round) -> bool */  {
  return (round_0 === 4);
}
 
 
// Automatically generated. Tests for the `Half-away-from-zero` constructor of the `:round` type.
export function is_half_away_from_zero(round_0) /* (round : round) -> bool */  {
  return (round_0 === 5);
}
 
 
// Automatically generated. Tests for the `Ceiling` constructor of the `:round` type.
export function is_ceiling(round_0) /* (round : round) -> bool */  {
  return (round_0 === 6);
}
 
 
// Automatically generated. Tests for the `Floor` constructor of the `:round` type.
export function is_floor(round_0) /* (round : round) -> bool */  {
  return (round_0 === 7);
}
 
 
// Automatically generated. Tests for the `Truncate` constructor of the `:round` type.
export function is_truncate(round_0) /* (round : round) -> bool */  {
  return (round_0 === 8);
}
 
 
// Automatically generated. Tests for the `Away-from-zero` constructor of the `:round` type.
export function is_away_from_zero(round_0) /* (round : round) -> bool */  {
  return (round_0 === 9);
}
 
 
// Round a `:decimal` number to an integer an optional rounding mode `rnd` (=`Half-even`).
export function int(x, rnd) /* (x : decimal, rnd : ? round) -> int */  {
   
  var _x65 = (rnd !== undefined) ? rnd : Half_even;
  var y = round_to_prec(x, 0, _x65);
  var _x66 = y.exp;
  var _x65 = $std_core_types._int_gt(_x66,0);
  if (_x65) {
    var _x67 = y.num;
    var _x68 = y.exp;
    return $std_core_int.mul_exp10(_x67, _x68);
  }
  else {
    return y.num;
  }
}
 
 
// Optimize: Use float64 division when within precision bounds.
export var maxexp;
var maxexp = 308;
 
export var maxpd;
var maxpd = 1.0e15;
 
export var maxprecise;
var maxprecise = $std_core_types._int_double((1.0e15));
 
export var minprecise;
var minprecise = $std_core_types._int_negate(maxprecise);
 
export function is_precise(i) /* (i : int) -> bool */  {
  return ($std_core_types._int_gt(i,minprecise)) ? $std_core_types._int_lt(i,maxprecise) : false;
}
 
 
// The sign of a decimal number.
export function sign(x) /* (x : decimal) -> order */  {
  var _x70 = x.num;
  var _x69 = $std_core_types._int_eq(_x70,0);
  if (_x69) {
    return $std_core_types.Eq;
  }
  else {
    var _x72 = x.num;
    var _x71 = $std_core_types._int_gt(_x72,0);
    return (_x71) ? $std_core_types.Gt : $std_core_types.Lt;
  }
}
 
 
// monadic lift
export function _mlift_pdecimal_10262(wild___0) /* (wild_@0 : char) -> std/text/parse/parse int */  {
  return $std_text_parse.pint();
}
 
 
// monadic lift
export function _mlift_pdecimal_10263(frac, neg, whole, exp) /* (frac : string, neg : bool, whole : string, exp : int) -> std/text/parse/parse decimal */  {
   
  var _x_x1_1_10259 = $std_core_types._lp__plus__plus__rp_(whole, frac);
   
  var _x_x1_0_10257 = $std_core_hnd._open_none2(function(s /* string */ , hex /* ? bool */ ) {
      var _x73 = (hex !== undefined) ? hex : false;
      return $std_core_int.xparse(s, _x73);
    }, _x_x1_1_10259);
   
  var _x_x1_10255 = $std_core_hnd._open_none2(function(m /* maybe<int> */ , nothing /* int */ ) {
      return (m === null) ? nothing : m.value;
    }, _x_x1_0_10257, 0);
   
  var y_10253 = $std_core_string.chars_fs_count(frac);
   
  var _x_x2_10256 = $std_core_types._int_sub(exp,y_10253);
   
  var i = $std_core_hnd._open_none2(decimal_exp, _x_x1_10255, _x_x2_10256);
  if (neg) {
    return $std_core_hnd._open_none1(function(x_1 /* decimal */ ) {
        var _x73 = x_1.num;
        var _x74 = x_1.exp;
        return Decimal($std_core_types._int_negate(_x73), _x74);
      }, i);
  }
  else {
    return i;
  }
}
 
 
// monadic lift
export function _mlift_pdecimal_10264(wild__) /* (wild_ : char) -> std/text/parse/parse string */  {
  return $std_text_parse.digits();
}
 
 
// monadic lift
export function _mlift_pdecimal_10265(neg, whole, frac) /* (neg : bool, whole : string, frac : string) -> std/text/parse/parse decimal */  {
   
  var x_10270 = $std_text_parse._lp__bar__bar__rp_(function() {
       
      var x_0_10272 = $std_text_parse.one_of("eE");
      if ($std_core_hnd._yielding()) {
        return $std_core_hnd.yield_extend(_mlift_pdecimal_10262);
      }
      else {
        return $std_text_parse.pint();
      }
    }, function() {
      return 0;
    });
  if ($std_core_hnd._yielding()) {
    return $std_core_hnd.yield_extend(function(exp /* int */ ) {
      return _mlift_pdecimal_10263(frac, neg, whole, exp);
    });
  }
  else {
    return _mlift_pdecimal_10263(frac, neg, whole, x_10270);
  }
}
 
 
// monadic lift
export function _mlift_pdecimal_10266(neg, whole) /* (neg : bool, whole : string) -> std/text/parse/parse decimal */  {
   
  var x_10274 = $std_text_parse._lp__bar__bar__rp_(function() {
       
      var x_0_10276 = $std_text_parse.char(0x002E);
      if ($std_core_hnd._yielding()) {
        return $std_core_hnd.yield_extend(_mlift_pdecimal_10264);
      }
      else {
        return $std_text_parse.digits();
      }
    }, function() {
      return "";
    });
  if ($std_core_hnd._yielding()) {
    return $std_core_hnd.yield_extend(function(frac /* string */ ) {
      return _mlift_pdecimal_10265(neg, whole, frac);
    });
  }
  else {
    return _mlift_pdecimal_10265(neg, whole, x_10274);
  }
}
 
 
// monadic lift
export function _mlift_pdecimal_10267(neg) /* (neg : bool) -> std/text/parse/parse decimal */  {
   
  var x_10278 = $std_text_parse.digits();
  if ($std_core_hnd._yielding()) {
    return $std_core_hnd.yield_extend(function(whole /* string */ ) {
      return _mlift_pdecimal_10266(neg, whole);
    });
  }
  else {
    return _mlift_pdecimal_10266(neg, x_10278);
  }
}
 
export function pdecimal() /* () -> std/text/parse/parse decimal */  {
   
  var x_10280 = $std_text_parse.sign();
  if ($std_core_hnd._yielding()) {
    return $std_core_hnd.yield_extend(_mlift_pdecimal_10267);
  }
  else {
     
    var x_0_10283 = $std_text_parse.digits();
    if ($std_core_hnd._yielding()) {
      return $std_core_hnd.yield_extend(function(whole /* string */ ) {
        return _mlift_pdecimal_10266(x_10280, whole);
      });
    }
    else {
       
      var x_1_10286 = $std_text_parse._lp__bar__bar__rp_(function() {
           
          var x_2_10289 = $std_text_parse.char(0x002E);
          if ($std_core_hnd._yielding()) {
            return $std_core_hnd.yield_extend(_mlift_pdecimal_10264);
          }
          else {
            return $std_text_parse.digits();
          }
        }, function() {
          return "";
        });
      if ($std_core_hnd._yielding()) {
        return $std_core_hnd.yield_extend(function(frac /* string */ ) {
          return _mlift_pdecimal_10265(x_10280, x_0_10283, frac);
        });
      }
      else {
         
        var x_3_10291 = $std_text_parse._lp__bar__bar__rp_(function() {
             
            var x_4_10294 = $std_text_parse.one_of("eE");
            if ($std_core_hnd._yielding()) {
              return $std_core_hnd.yield_extend(_mlift_pdecimal_10262);
            }
            else {
              return $std_text_parse.pint();
            }
          }, function() {
            return 0;
          });
        if ($std_core_hnd._yielding()) {
          return $std_core_hnd.yield_extend(function(exp /* int */ ) {
            return _mlift_pdecimal_10263(x_1_10286, x_10280, x_0_10283, exp);
          });
        }
        else {
           
          var _x_x1_1_10259 = $std_core_types._lp__plus__plus__rp_(x_0_10283, x_1_10286);
           
          var _x_x1_0_10257 = $std_core_hnd._open_none2(function(s /* string */ , hex /* ? bool */ ) {
              var _x75 = (hex !== undefined) ? hex : false;
              return $std_core_int.xparse(s, _x75);
            }, _x_x1_1_10259);
           
          var _x_x1_10255 = $std_core_hnd._open_none2(function(m /* maybe<int> */ , nothing /* int */ ) {
              return (m === null) ? nothing : m.value;
            }, _x_x1_0_10257, 0);
           
          var y_10253 = $std_core_string.chars_fs_count(x_1_10286);
           
          var _x_x2_10256 = $std_core_types._int_sub(x_3_10291,y_10253);
           
          var i = $std_core_hnd._open_none2(decimal_exp, _x_x1_10255, _x_x2_10256);
          if (x_10280) {
            return $std_core_hnd._open_none1(function(x_1_0 /* decimal */ ) {
                var _x75 = x_1_0.num;
                var _x76 = x_1_0.exp;
                return Decimal($std_core_types._int_negate(_x75), _x76);
              }, i);
          }
          else {
            return i;
          }
        }
      }
    }
  }
}
 
 
// monadic lift
export function _mlift_parse_decimal_10268(x, wild__) /* (x : decimal, wild_ : ()) -> std/text/parse/parse decimal */  {
  return x;
}
 
 
// monadic lift
export function _mlift_parse_decimal_10269(x) /* (x : decimal) -> std/text/parse/parse decimal */  {
   
  var x_0_10296 = $std_text_parse.eof();
  if ($std_core_hnd._yielding()) {
    return $std_core_hnd.yield_extend(function(wild__ /* () */ ) {
      return x;
    });
  }
  else {
    return x;
  }
}
 
 
// Parse a `:decimal` number.
export function parse_decimal(s) /* (s : string) -> maybe<decimal> */  {
   
  var s_0_10156 = (((((s).replace(/^\s\s*/,'')))).replace(/\s+$/,''));
   
  var input_10154 = $std_core_sslice.Sslice(s_0_10156, 0, s_0_10156.length);
   
  var perr_10153 = $std_text_parse.parse(input_10154, function() {
       
      var x_10300 = pdecimal();
      if ($std_core_hnd._yielding()) {
        return $std_core_hnd.yield_extend(_mlift_parse_decimal_10269);
      }
      else {
        return _mlift_parse_decimal_10269(x_10300);
      }
    });
  if (perr_10153._tag === 1) {
    return $std_core_types.Just(perr_10153.result);
  }
  else {
    return $std_core_types.Nothing;
  }
}
 
 
// Take the sum of a list of decimal numbers (0 for the empty list).
export function sum(ds) /* (ds : list<decimal>) -> decimal */  {
  return $std_core_list.foldr(ds, zero, _lp__plus__rp_);
}
 
 
// Round a `:decimal` using to the largest integer that is not larger than `x`.
export function floor(x) /* (x : decimal) -> decimal */  {
  return round_to_prec(x, 0, Floor);
}
 
 
// Round a `:decimal` to the smallest integer that is not less than `x`.
export function ceiling(x) /* (x : decimal) -> decimal */  {
  return round_to_prec(x, 0, Ceiling);
}
 
 
// Truncate a `:decimal` to an integer by rounding towards zero.
export function truncate(x) /* (x : decimal) -> decimal */  {
  return round_to_prec(x, 0, Truncate);
}
 
 
// Convert a decimal to a `:float64`. This may lose precision.
export function float64(x) /* (x : decimal) -> float64 */  {
   
  var _x77 = x.exp;
  var b_10164 = $std_core_types._int_lt(_x77,0);
  if (b_10164) {
    var _x78 = x.num;
    var _x79 = x.exp;
    var _x77 = $std_core_int.divmod_exp10(_x78, $std_core_types._int_negate(_x79));
    var _x80 = x.exp;
    return (($std_core_types._int_to_double((_x77.fst))) + ((($std_core_types._int_to_double((_x77.snd))) * (Math.pow(10.0,($std_core_types._int_to_double(_x80)))))));
  }
  else {
    var _x81 = x.num;
    var _x82 = x.exp;
    return (($std_core_types._int_to_double(_x81)) * (Math.pow((10.0),($std_core_types._int_to_double(_x82)))));
  }
}
 
 
// Return the 'truncated' fraction, always in the range (`-1.0`,`1.0`).
// `x.truncate + x.fraction == x`
export function fraction(x) /* (x : decimal) -> decimal */  {
   
  var _x83 = x.exp;
  var b_10172 = $std_core_types._int_lt(_x83,0);
  if (b_10172) {
     
    var y_10176 = round_to_prec(x, 0, Truncate);
     
    var _x83 = y_10176.num;
    var _x84 = y_10176.exp;
    var y_10228 = Decimal($std_core_types._int_negate(_x83), _x84);
     
    var _x86 = x.exp;
    var _x87 = y_10228.exp;
    var _x85 = $std_core_types._int_le(_x86,_x87);
    if (_x85) {
      var e = x.exp;
    }
    else {
      var e = y_10228.exp;
    }
     
    var xx = expand(x, e);
     
    var yy = expand(y_10228, e);
    var _x83 = xx.num;
    var _x84 = yy.num;
    return Decimal($std_core_types._int_add(_x83,_x84), e);
  }
  else {
    return zero;
  }
}
 
 
// Return the 'floored' fraction, always in the range [`0`,`1.0`).
// `x.floor + x.ffraction == x`
export function ffraction(x) /* (x : decimal) -> decimal */  {
   
  var _x85 = x.exp;
  var b_10180 = $std_core_types._int_lt(_x85,0);
  if (b_10180) {
     
    var y_10184 = round_to_prec(x, 0, Floor);
     
    var _x85 = y_10184.num;
    var _x86 = y_10184.exp;
    var y_10231 = Decimal($std_core_types._int_negate(_x85), _x86);
     
    var _x88 = x.exp;
    var _x89 = y_10231.exp;
    var _x87 = $std_core_types._int_le(_x88,_x89);
    if (_x87) {
      var e = x.exp;
    }
    else {
      var e = y_10231.exp;
    }
     
    var xx = expand(x, e);
     
    var yy = expand(y_10231, e);
    var _x85 = xx.num;
    var _x86 = yy.num;
    return Decimal($std_core_types._int_add(_x85,_x86), e);
  }
  else {
    return zero;
  }
}
 
 
// The exponent of a decimal if displayed in scientific notation.\
// `11.2e-1.decimal.exponent == 0`
export function exponent(d) /* (d : decimal) -> int */  {
   
  var _x87 = d.num;
  var x_0_10190 = $std_core_int.count_digits(_x87);
   
  var _x88 = d.exp;
  var x_10188 = $std_core_types._int_add(x_0_10190,_x88);
  return $std_core_types._int_sub(x_10188,1);
}
 
export function show_frac(frac, prec) /* (frac : string, prec : int) -> string */  {
   
  var frac_trim = $std_core_sslice.trim_right(frac, "0");
   
  if ($std_core_types._int_ge(prec,0)) {
    var frac_full = $std_core_string.pad_right(frac_trim, prec, 0x0030);
  }
  else {
    var frac_full = frac_trim;
  }
  if ((frac_full === (""))) {
    return "";
  }
  else {
    return $std_core_types._lp__plus__plus__rp_(".", frac_full);
  }
}
 
 
/* Show a decimal `d` with a given precision `prec` (=`-1000`) in scientific notation.
The precision specifies the  number of digits after the dot, i.e.
the number of significant digits is `prec+1`.
If the precision is negative, _at most_ `prec` digits are displayed, and if
it is positive exactly `prec` digits are used.
```
> decimal(1,-1).show-exp
"1e-1"
> 1.1.decimal.show-exp
"1.100000000000000088817841970012523233890533447265625"
> 1.1.decimal.show-exp(-20)
"1.10000000000000008882"
> 0.125.decimal.show-exp(-20)
"1.25e-1"
> 0.125.decimal.show-exp(20)
"1.25000000000000000000e-1"
```
.
*/
export function show_exp(d, prec) /* (d : decimal, prec : ? int) -> string */  {
   
  var _x87 = (prec !== undefined) ? prec : -1000;
  var x_10195 = $std_core_types._int_abs(_x87);
   
  var _x88 = d.num;
  var x_0_10190 = $std_core_int.count_digits(_x88);
   
  var _x89 = d.exp;
  var x_10188 = $std_core_types._int_add(x_0_10190,_x89);
   
  var y_10196 = $std_core_types._int_sub(x_10188,1);
   
  var x_0 = round_to_prec(d, $std_core_types._int_sub(x_10195,y_10196));
   
  var _x90 = x_0.num;
  var s = $std_core_int.show($std_core_types._int_abs(_x90));
   
  var digits = $std_core_string.chars_fs_count(s);
   
  var _x91 = x_0.exp;
  var x_1_10198 = $std_core_types._int_add(_x91,digits);
   
  var exp = $std_core_types._int_sub(x_1_10198,1);
   
  var _x93 = x_0.num;
  var _x92 = $std_core_types._int_lt(_x93,0);
  var sign_0 = (_x92) ? "-" : "";
   
  if ($std_core_types._int_eq(exp,0)) {
    var exponent_0 = "";
  }
  else {
    var _x94 = ($std_core_types._int_gt(exp,0)) ? "+" : "";
    var exponent_0 = $std_core_types._lp__plus__plus__rp_("e", $std_core_types._lp__plus__plus__rp_(_x94, $std_core_int.show(exp)));
  }
   
  var frac_10233 = $std_core_sslice.tail(s);
   
  var frac_trim = $std_core_sslice.trim_right(frac_10233, "0");
   
  var _x96 = (prec !== undefined) ? prec : -1000;
  var _x95 = $std_core_types._int_ge(_x96,0);
  if (_x95) {
    var _x97 = (prec !== undefined) ? prec : -1000;
    var frac_full = $std_core_string.pad_right(frac_trim, _x97, 0x0030);
  }
  else {
    var frac_full = frac_trim;
  }
  if ((frac_full === (""))) {
    var _x87 = "";
  }
  else {
    var _x87 = $std_core_types._lp__plus__plus__rp_(".", frac_full);
  }
  return $std_core_types._lp__plus__plus__rp_(sign_0, $std_core_types._lp__plus__plus__rp_($std_core_sslice.head(s), $std_core_types._lp__plus__plus__rp_(_x87, exponent_0)));
}
 
 
/* Show a decimal `d` with a given precision `prec` (=`-1000`) in fixed-point notation.
The precision specifies the  number of digits after the dot.
If the precision is negative, _at most_  `prec` digits after the dot are displayed,
while for a positive precision, exactly `prec` digits are used.
```
> decimal(1,-1).show-fixed
"0.1"
> 0.1.decimal.show-fixed
"0.1000000000000000055511151231257827021181583404541015625"
> 0.1.decimal.show-fixed(20)
"0.1000000000000000555"
> 0.1.decimal.show-fixed(-20)
"0.1000000000000000555"
> decimal(1,-1).show-fixed(20)
"0.1000000000000000000"
```
.
*/
export function show_fixed(d, prec) /* (d : decimal, prec : ? int) -> string */  {
   
  var _x88 = (prec !== undefined) ? prec : -1000;
  var x = round_to_prec(d, $std_core_types._int_abs(_x88));
  var _x89 = x.exp;
  var _x88 = $std_core_types._int_ge(_x89,0);
  if (_x88) {
     
    var _x91 = (prec !== undefined) ? prec : -1000;
    var _x90 = $std_core_types._int_le(_x91,0);
    if (_x90) {
      var frac = "";
    }
    else {
      var _x92 = (prec !== undefined) ? prec : -1000;
      var frac = $std_core_types._lp__plus__plus__rp_(".", $std_core_string.repeatz("0", $std_core_int.ssize__t(_x92)));
    }
    var _x90 = x.num;
    var _x91 = x.exp;
    return $std_core_types._lp__plus__plus__rp_($std_core_int.show(_x90), $std_core_types._lp__plus__plus__rp_($std_core_string.repeatz("0", $std_core_int.ssize__t(_x91)), frac));
  }
  else {
     
    var _x92 = x.exp;
    var digits = $std_core_types._int_negate(_x92);
     
    var _x94 = x.num;
    var _x93 = $std_core_types._int_lt(_x94,0);
    var sign_0 = (_x93) ? "-" : "";
     
    var _x95 = x.num;
    var i_0 = $std_core_types._int_abs(_x95);
     
    var man = $std_core_int.cdiv_exp10(i_0, digits);
     
    var y_10220 = $std_core_int.mul_exp10(man, digits);
     
    var frac_0 = $std_core_types._int_sub(i_0,y_10220);
     
    var frac_1_10235 = $std_core_string.pad_left($std_core_int.show(frac_0), digits, 0x0030);
     
    var frac_trim = $std_core_sslice.trim_right(frac_1_10235, "0");
     
    var _x97 = (prec !== undefined) ? prec : -1000;
    var _x96 = $std_core_types._int_ge(_x97,0);
    if (_x96) {
      var _x98 = (prec !== undefined) ? prec : -1000;
      var frac_full = $std_core_string.pad_right(frac_trim, _x98, 0x0030);
    }
    else {
      var frac_full = frac_trim;
    }
    if ((frac_full === (""))) {
      var _x92 = "";
    }
    else {
      var _x92 = $std_core_types._lp__plus__plus__rp_(".", frac_full);
    }
    return $std_core_types._lp__plus__plus__rp_(sign_0, $std_core_types._lp__plus__plus__rp_($std_core_int.show(man), _x92));
  }
}
 
 
// Show a decimal `d` with a given precision `prec` (=`-1000`).
// The precision specifies the  number of digits after the dot (in either scientific of fixed-point notation).
// If the precision is negative, _at most_ `prec` digits are displayed, while for a positive
// precision, exactly `prec` digits behind the dot are displayed.
// This uses `show-fixed` when the exponent of `d` in scientific notation is larger than -5
// and smaller than the precision (or 15 in case of a negative precision), otherwise it uses `show-exp`.
export function show(d, prec) /* (d : decimal, prec : ? int) -> string */  {
   
  var _x93 = d.num;
  var x_0_10190 = $std_core_int.count_digits(_x93);
   
  var _x94 = d.exp;
  var x_10188 = $std_core_types._int_add(x_0_10190,_x94);
   
  var exp = $std_core_types._int_sub(x_10188,1);
  if ($std_core_types._int_gt(exp,(-5))) {
    var _x96 = (prec !== undefined) ? prec : -1000;
    var _x95 = $std_core_types._int_lt(_x96,0);
    if (_x95) {
      var _x94 = 15;
    }
    else {
      var _x94 = (prec !== undefined) ? prec : -1000;
    }
    var _x93 = $std_core_types._int_lt(exp,_x94);
    if (_x93) {
      var _x97 = (prec !== undefined) ? prec : -1000;
      return show_fixed(d, _x97);
    }
    else {
      var _x98 = (prec !== undefined) ? prec : -1000;
      return show_exp(d, _x98);
    }
  }
  else {
    var _x99 = (prec !== undefined) ? prec : -1000;
    return show_exp(d, _x99);
  }
}
 
 
// Show a decimal `d` using its internal representation.
export function show_raw(d) /* (d : decimal) -> string */  {
  var _x100 = d.num;
  var _x101 = d.exp;
  return $std_core_types._lp__plus__plus__rp_($std_core_int.show(_x100), $std_core_types._lp__plus__plus__rp_("e", $std_core_int.show(_x101)));
}