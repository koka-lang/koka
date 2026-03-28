// Koka generated module: std/num/int64, koka version: 3.2.4
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
 
// declarations
 
 
// clamp an `:int` to fit in an `:int64_t`.
export function int64(i) /* (i : int) -> int64 */  {
  return $std_core_types._int_clamp64(i);
}
 
 
// The 64-bit integer for 1.
export var one;
var one = 1n;
 
 
// The 64-bit integer for zero.
export var zero;
var zero = 0n;
 
 
// Convert a boolean to an `:int64`.
export function bool_fs_int64(b) /* (b : bool) -> int64 */  {
  return (b) ? one : zero;
}
 
 
// Create an `:int64` from the given `hi` and `lo` numbers lowest 32-bits.
// Preserves the sign of `hi`.
export function hilo_fs_int64(hi_0, lo_0) /* (hi : int64, lo : int64) -> int64 */  {
  return ($std_core_types._int64_shl(hi_0,32n)) | (lo_0 & 4294967295n);
}
 
 
// The maximal integer value before overflow happens
export var max_int64;
var max_int64 = int64($std_core_types._int_const(9223372036854775807n));
 
 
// The minimal integer value before underflow happens
export var min_int64;
var min_int64 = int64($std_core_types._int_const(-9223372036854775808n));
 
 
// The number of bits in an `:int64` (always 64)
export var bits_int64;
var bits_int64 = 64n;
 
 
// Convenient shorthand to `int64`, e.g. `1234.i64`
export function i64(i) /* (i : int) -> int64 */  {
  return int64(i);
}
 
 
// Convert an `:int64` to a boolean.
export function bool(i) /* (i : int64) -> bool */  {
  return (i !== zero);
}
 
 
// Return the top 32-bits of an `:int64`.
// Preserves the sign.
export function hi(i) /* (i : int64) -> int64 */  {
  return $std_core_types._int64_sar(i,32n);
}
 
 
// Return the low 32-bits of an `:int64`.
export function lo(i) /* (i : int64) -> int64 */  {
  return i & 4294967295n;
}
 
 
// Convert an `:int64` to an `:int` but interpret the `:int64` as a 64-bit unsigned value.
export function int64_fs_uint(i) /* (i : int64) -> int */  {
  if (0n > i) {
     
    var y_10001 = $std_core_types._int_from_int64(i);
    return $std_core_types._int_add(($std_core_types._int_const(18446744073709551616n)),y_10001);
  }
  else {
    return $std_core_types._int_from_int64(i);
  }
}
 
 
// Convert a pair `(hi,lo)` to an signed integer,
// where `(hi,lo).int == hi.int * 0x1_0000_0000_0000_0000 + lo.uint`
export function hilo_fs_int(_pat_x445__19) /* ((int64, int64)) -> int */  {
   
  var x_10002 = $std_core_types._int_mul(($std_core_types._int_from_int64((_pat_x445__19.fst))),($std_core_types._int_const(18446744073709551616n)));
   
  var y_10003 = int64_fs_uint(_pat_x445__19.snd);
  return $std_core_types._int_add(x_10002,y_10003);
}
 
 
// Convert a pair `(hi,lo)` to an unsigned integer,
// where `(hi,lo).uint == hi.uint * 0x1_0000_0000_0000_0000 + lo.uint`
export function hilo_fs_uint(_pat_x450__20) /* ((int64, int64)) -> int */  {
   
  var x_10004 = $std_core_types._int_mul((int64_fs_uint(_pat_x450__20.fst)),($std_core_types._int_const(18446744073709551616n)));
   
  var y_10005 = int64_fs_uint(_pat_x450__20.snd);
  return $std_core_types._int_add(x_10004,y_10005);
}
 
 
// Convert an `:int64` to a string
export function show(i) /* (i : int64) -> string */  {
  return $std_core_int.show($std_core_types._int_from_int64(i));
}
 
 
// Convert an `:int` to `:int64` but interpret the `int` as an unsigned 64-bit value.
// `i` is clamped between `0` and `0xFFFF_FFFF_FFFF_FFFF`.\
// `0x7FFF_FFFF_FFFF_FFFF.uint64 == 0x7FFF_FFFF_FFFF_FFFF.int64 == max-int64`\
// `0x8000_0000_0000_0000.uint64 == -0x8000_0000_0000_0000.int64 == min-int64`\
// `0xFFFF_FFFF_FFFF_FFFF.uint64 == -1.int64`\
export function int_fs_uint64(i) /* (i : int) -> int64 */  {
  var _x1 = $std_core_types._int_gt(i,($std_core_types._int_from_int64(max_int64)));
  var _x0 = (_x1) ? $std_core_types._int_sub(i,($std_core_types._int_const(18446744073709551616n))) : i;
  return int64(_x0);
}
 
 
// Show an `:int64` in hexadecimal notation
// The `width`  parameter specifies how wide the hex value is where `'0'`  is used to align.\
// The `use-capitals` parameter (= `True`) determines if capital letters should be used to display the hexadecimal digits.\
// The `pre` (=`"0x"`) is an optional prefix for the number (goes between the sign and the number).
export function show_hex(i, width, use_capitals, pre) /* (i : int64, width : ? int, use-capitals : ? bool, pre : ? string) -> string */  {
  var _x2 = (width !== undefined) ? width : 1;
  var _x3 = (use_capitals !== undefined) ? use_capitals : true;
  var _x4 = (pre !== undefined) ? pre : "0x";
  return $std_core_show.show_hex($std_core_types._int_from_int64(i), _x2, _x3, _x4);
}
 
 
// Show an `:int64` in hexadecimal notation interpreted as an unsigned 64-bit value.
// The `width`  parameter specifies how wide the hex value is where `'0'`  is used to align.\
// The `use-capitals` parameter (= `True`) determines if capital letters should be used to display the hexadecimal digits.\
// The `pre` (=`"0x"`) is an optional prefix for the number.
export function show_hex64(i, width, use_capitals, pre) /* (i : int64, width : ? int, use-capitals : ? bool, pre : ? string) -> string */  {
  var _x5 = (width !== undefined) ? width : 16;
  var _x6 = (use_capitals !== undefined) ? use_capitals : true;
  var _x7 = (pre !== undefined) ? pre : "0x";
  return $std_core_show.show_hex(int64_fs_uint(i), _x5, _x6, _x7);
}
 
 
// Compare a 64-integer to zero.
export function sign(i) /* (i : int64) -> order */  {
  if (0n < i) {
    return $std_core_types.Gt;
  }
  else {
    return (0n > i) ? $std_core_types.Lt : $std_core_types.Eq;
  }
}
 
 
// Returns `true` if the integer `i`  is an odd number.
export function is_odd(i) /* (i : int64) -> bool */  {
  return ((i & one) === one);
}
 
 
// Returns `true` if the integer `i`  is an even number.
export function is_even(i) /* (i : int64) -> bool */  {
  return ((i & one) === zero);
}
 
 
// Increment a 64-bit integer.
export function inc(i) /* (i : int64) -> int64 */  {
  return BigInt.asIntN(64,i + 1n);
}
 
 
// Decrement a 64-bit integer.
export function dec(i) /* (i : int64) -> int64 */  {
  return BigInt.asIntN(64,i - 1n);
}
 
export function cmp(x, y) /* (x : int64, y : int64) -> order */  {
  if ((x < y)) {
    return $std_core_types.Lt;
  }
  else {
    return ((x > y)) ? $std_core_types.Gt : $std_core_types.Eq;
  }
}
 
 
// Take the bitwise _xor_ of two `:int64`s
export function _lp__hat__rp_(x, y) /* (x : int64, y : int64) -> int64 */  {
  return x ^ y;
}
 
 
// Negate a 64-bit integer
export function negate(i) /* (i : int64) -> int64 */  {
  return BigInt.asIntN(64,0n - i);
}
 
 
// Return the absolute value of an integer.
// Raises an exception if the `:int64` is `min-int64`
// (since the negation of `min-int64` equals itself and is still negative)
export function abs(i) /* (i : int64) -> exn int64 */  {
   
  var _x_x1_10096 = 0n > i;
  var _x8 = $std_core_hnd._open_none1(function(b /* bool */ ) {
      return (b) ? false : true;
    }, _x_x1_10096);
  if (_x8) {
    return i;
  }
  else {
    if ((i > min_int64)) {
      return $std_core_hnd._open_none1(function(i_0 /* int64 */ ) {
          return BigInt.asIntN(64,0n - i_0);
        }, i);
    }
    else {
      return $std_core_exn.$throw("std/num/int64/abs: cannot make min-int64 into a positive int64 without overflow");
    }
  }
}
 
 
// Return the absolute value of an integer.
// Returns 0 if the `:int64` is `min-int64`
// (since the negation of `min-int64` equals itself and is still negative)
export function abs0(i) /* (i : int64) -> int64 */  {
   
  var b_10008 = 0n > i;
  if (b_10008) {
    if ((i > min_int64)) {
      return BigInt.asIntN(64,0n - i);
    }
    else {
      return 0n;
    }
  }
  else {
    return i;
  }
}
 
 
// Shift an `:int64` `i` to the left by `n % 64` bits.
export function shl(i, shift) /* (i : int64, shift : int) -> int64 */  {
  return $std_core_types._int64_shl(i,(int64(shift)));
}
 
 
// Logical shift an `:int64` to the right by `n % 64` bits. Shift in zeros from the left.
export function shr(i, shift) /* (i : int64, shift : int) -> int64 */  {
  return $std_core_types._int64_shr(i,(int64(shift)));
}
 
 
// Arithmetic shift an `:int64` to the right by `n % 64` bits. Shift in the sign bit from the left.
export function sar(i, shift) /* (i : int64, shift : int) -> int64 */  {
  return $std_core_types._int64_sar(i,(int64(shift)));
}
 
 
// Bitwise rotate an `:int64` `n % 64` bits to the left.
export function rotl(i, shift) /* (i : int64, shift : int) -> int64 */  {
  return $std_core_types._int64_rotl(i,(int64(shift)));
}
 
 
// Bitwise rotate an `:int64` `n % 64` bits to the right.
export function rotr(i, shift) /* (i : int64, shift : int) -> int64 */  {
  return $std_core_types._int64_rotr(i,(int64(shift)));
}
 
 
// Count number of 1-bits.
export function popcount(i) /* (i : int64) -> int */  {
  return $std_core_types._int_from_int64(($std_core_types._int64_popcount(i)));
}
 
 
// Wide carry-less multiplication (or _xor_ multiplication) to `(hi,lo)`,
// where `(hi,lo).int == hi.int * 0x1_0000_0000_0000_0000 + lo.uint`.
// See also <https://en.wikipedia.org/wiki/Carry-less_product>.
export function clmul_wide(x, y) /* (x : int64, y : int64) -> (int64, int64) */  {
  return $std_core._unsupported_external("std/num/int64/@extern-clmul-wide");
}
 
 
// Return the minimum of two integers
export function min(i, j) /* (i : int64, j : int64) -> int64 */  {
  return ((i <= j)) ? i : j;
}
 
 
// Return the maximum of two integers
export function max(i, j) /* (i : int64, j : int64) -> int64 */  {
  return ((i >= j)) ? i : j;
}
 
 
// Full 64x64 to 128-bit unsigned multiply to `(hi,lo)`.
// where `umul(x,y).uint == x.uint * y.uint`
export function umul(i, j) /* (i : int64, j : int64) -> (int64, int64) */  {
  return $std_core_types._int64_umul(i,j);
}
 
 
// Full 64x64 to 128-bit signed multiply to `(hi,lo)`.
// where `imul(x,y).int == x.int * y.int`
export function imul(i, j) /* (i : int64, j : int64) -> (int64, int64) */  {
  return $std_core_types._int64_imul(i,j);
}
 
 
// Truncated division (as in C). See also `(/):(x : int64, y : int64) -> int64`.
export function cdiv(i, j) /* (i : int64, j : int64) -> exn int64 */  {
  if (0n === j) {
    return $std_core_exn.$throw("std/num/int64/cdiv: division by zero");
  }
  else {
    var _x9 = (j === ($std_core_hnd._open_none1(int64, -1)));
    if (_x9) {
      if ((i === min_int64)) {
        return $std_core_exn.$throw("std/num/int64/cdiv: division overflow in cdiv(min-int64, -1.int64)");
      }
      else {
        return i / j;
      }
    }
    else {
      return i / j;
    }
  }
}
 
 
// Truncated modulus (as in C). See also `(%):(x : int64, y : int64) -> int64`.
export function cmod(i, j) /* (i : int64, j : int64) -> exn int64 */  {
  if (0n === j) {
    return $std_core_exn.$throw("std/num/int64/cmod: modulus by zero");
  }
  else {
    var _x10 = (j === ($std_core_hnd._open_none1(int64, -1)));
    if (_x10) {
      if ((i === min_int64)) {
        return $std_core_exn.$throw("std/num/int64/cmod: modulus overflow in cmod(min-int64, -1.int64)");
      }
      else {
        return i % j;
      }
    }
    else {
      return i % j;
    }
  }
}
 
 
// Negate an 64-bit integer
export function _lp__tilde__rp_(i) /* (i : int64) -> int64 */  {
  return BigInt.asIntN(64,0n - i);
}
 
 
/*
Euclidean-0 division.
Euclidean division is defined as: For any `D`  and `d`  where `d!=0` , we have:
1. `D == d*(D/d) + (D%d)`
2. `D%d`  is always positive where `0 <= D%d < abs(d)`
Moreover, Euclidean-0 is a total function, for the case where `d==0`  we have
that `D%0 == D`  and `D/0 == 0` . So property (1) still holds, but not property (2).
Useful laws that hold for Euclidean-0 division:
* `D/(-d) == -(D/d)`
* `D%(-d) == D%d`
* `D/(2^n) == sar(D,n)         `  (with `0 <= n <= 63`)
* `D%(2^n) == D & ((2^n) - 1)  `  (with `0 <= n <= 63`)
Note that an interesting edge case is `min-int64 / -1` which equals `min-int64` since in modulo 64-bit
arithmetic `min-int64 == -1 * min-int64 == -1 * (min-int64 / -1) + (min-int64 % -1)` satisfying property (1).
Of course `(min-int64 + 1) / -1` is again positive (namely `max-int64`).
See also _Division and modulus for computer scientists, Daan Leijen, 2001_
[pdf](https://www.microsoft.com/en-us/research/wp-content/uploads/2016/02/divmodnote-letter.pdf) .
*/
export function _lp__fs__rp_(x, y) /* (x : int64, y : int64) -> int64 */  {
  var _x11 = (y === 0n);
  if (_x11) {
    return 0n;
  }
  else {
    var _x12 = (y === (-1n));
    if (_x12) {
      if ((x === min_int64)) {
        return x;
      }
      else {
         
        var q = x / y;
         
        var r = x % y;
        var _x13 = (r >= 0n);
        if (_x13) {
          return q;
        }
        else {
          var _x14 = (y > 0n);
          if (_x14) {
            return BigInt.asIntN(64,q - 1n);
          }
          else {
            return BigInt.asIntN(64,q + 1n);
          }
        }
      }
    }
    else {
       
      var q_0 = x / y;
       
      var r_0 = x % y;
      var _x15 = (r_0 >= 0n);
      if (_x15) {
        return q_0;
      }
      else {
        var _x16 = (y > 0n);
        if (_x16) {
          return BigInt.asIntN(64,q_0 - 1n);
        }
        else {
          return BigInt.asIntN(64,q_0 + 1n);
        }
      }
    }
  }
}
 
 
// Euclidean-0 modulus. See `(/):(x : int64, y : int64) -> int64` division for more information.
export function _lp__perc__rp_(x, y) /* (x : int64, y : int64) -> int64 */  {
  var _x17 = (y === 0n);
  if (_x17) {
    return x;
  }
  else {
    var _x18 = (y === (-1n));
    if (_x18) {
      if ((x === min_int64)) {
        return 0n;
      }
      else {
         
        var r = x % y;
        var _x19 = (r >= 0n);
        if (_x19) {
          return r;
        }
        else {
          var _x20 = (y > 0n);
          return (_x20) ? BigInt.asIntN(64,r + y) : BigInt.asIntN(64,r - y);
        }
      }
    }
    else {
       
      var r_0 = x % y;
      var _x21 = (r_0 >= 0n);
      if (_x21) {
        return r_0;
      }
      else {
        var _x22 = (y > 0n);
        return (_x22) ? BigInt.asIntN(64,r_0 + y) : BigInt.asIntN(64,r_0 - y);
      }
    }
  }
}
 
export function divmod(x, y) /* (x : int64, y : int64) -> (int64, int64) */  {
  if (0n === y) {
    return $std_core_types.Tuple2(zero, x);
  }
  else {
    var _x23 = (y === (-1n));
    if (_x23) {
      if ((x === min_int64)) {
        return $std_core_types.Tuple2(x, 0n);
      }
      else {
         
        var q = x / y;
         
        var r = x % y;
        var _x24 = (r >= 0n);
        if (_x24) {
          return $std_core_types.Tuple2(q, r);
        }
        else {
          var _x25 = (y > 0n);
          if (_x25) {
            return $std_core_types.Tuple2(BigInt.asIntN(64,q - 1n), BigInt.asIntN(64,r + y));
          }
          else {
            return $std_core_types.Tuple2(BigInt.asIntN(64,q + 1n), BigInt.asIntN(64,r - y));
          }
        }
      }
    }
    else {
       
      var q_0 = x / y;
       
      var r_0 = x % y;
      var _x26 = (r_0 >= 0n);
      if (_x26) {
        return $std_core_types.Tuple2(q_0, r_0);
      }
      else {
        var _x27 = (y > 0n);
        if (_x27) {
          return $std_core_types.Tuple2(BigInt.asIntN(64,q_0 - 1n), BigInt.asIntN(64,r_0 + y));
        }
        else {
          return $std_core_types.Tuple2(BigInt.asIntN(64,q_0 + 1n), BigInt.asIntN(64,r_0 - y));
        }
      }
    }
  }
}
 
 
// monadic lift
export function range_fs__mlift_fold_int64_10100(end, f, start, x) /* forall<a,e> (end : int64, f : (int64, a) -> e a, start : int64, x : a) -> e a */  {
  return range_fs_fold_int64(BigInt.asIntN(64,start + 1n), end, x, f);
}
 
export function range_fs_fold_int64(start_0, end_0, init, f_0) /* forall<a,e> (start : int64, end : int64, init : a, f : (int64, a) -> e a) -> e a */  { tailcall: while(1)
{
  if ((start_0 > end_0)) {
    return init;
  }
  else {
     
    var x_0_10102 = f_0(start_0, init);
    if ($std_core_hnd._yielding()) {
      return $std_core_hnd.yield_extend(function(x_1 /* 3768 */ ) {
        return range_fs__mlift_fold_int64_10100(end_0, f_0, start_0, x_1);
      });
    }
    else {
      {
        // tail call
        var _x28 = BigInt.asIntN(64,start_0 + 1n);
        start_0 = _x28;
        init = x_0_10102;
        continue tailcall;
      }
    }
  }
}}
 
export function fold_int64(n, init, f) /* forall<a,e> (n : int64, init : a, f : (int64, a) -> e a) -> e a */  {
  return range_fs_fold_int64(zero, BigInt.asIntN(64,n - 1n), init, f);
}
 
 
// monadic lift
export function range_fs__mlift_lift_for_while64_4375_10101(action, end, i, _y_x10082) /* forall<a,e> (action : (int64) -> e maybe<a>, end : int64, i : int64, maybe<a>) -> e maybe<a> */  {
  if (_y_x10082 === null) {
     
    var i_0_10018 = BigInt.asIntN(64,i + 1n);
    return range_fs__lift_for_while64_4375(action, end, i_0_10018);
  }
  else {
    return $std_core_types.Just(_y_x10082.value);
  }
}
 
 
// lifted local: range/for-while64, rep
export function range_fs__lift_for_while64_4375(action_0, end_0, i_0) /* forall<a,e> (action : (int64) -> e maybe<a>, end : int64, i : int64) -> e maybe<a> */  { tailcall: while(1)
{
  if ((i_0 <= end_0)) {
     
    var x_0_10105 = action_0(i_0);
    if ($std_core_hnd._yielding()) {
      return $std_core_hnd.yield_extend(function(_y_x10082_0 /* maybe<3879> */ ) {
        return range_fs__mlift_lift_for_while64_4375_10101(action_0, end_0, i_0, _y_x10082_0);
      });
    }
    else {
      if (x_0_10105 === null) {
         
        var i_0_10018_0 = BigInt.asIntN(64,i_0 + 1n);
        {
          // tail call
          i_0 = i_0_10018_0;
          continue tailcall;
        }
      }
      else {
        return $std_core_types.Just(x_0_10105.value);
      }
    }
  }
  else {
    return $std_core_types.Nothing;
  }
}}
 
 
// Executes `action`  for each integer between `start`  upto `end`  (including both `start`  and `end` ).
// If `start > end`  the function returns without any call to `action` .
// If `action` returns `Just`, the iteration is stopped and the result returned
export function range_fs_for_while64(start, end, action) /* forall<a,e> (start : int64, end : int64, action : (int64) -> e maybe<a>) -> e maybe<a> */  {
  return range_fs__lift_for_while64_4375(action, end, start);
}
 
export function for_while64(n, action) /* forall<a,e> (n : int64, action : (int64) -> e maybe<a>) -> e maybe<a> */  {
   
  var end_10020 = BigInt.asIntN(64,n - 1n);
  return range_fs__lift_for_while64_4375(action, end_10020, zero);
}
 
export function _trmc_list64(lo_0, hi_0, _acc) /* (lo : int64, hi : int64, ctx<list<int64>>) -> list<int64> */  { tailcall: while(1)
{
  if ((lo_0 <= hi_0)) {
     
    var _trmc_x10063 = undefined;
     
    var _trmc_x10064 = $std_core_types.Cons(lo_0, _trmc_x10063);
    {
      // tail call
      var _x29 = BigInt.asIntN(64,lo_0 + 1n);
      var _x30 = $std_core_types._cctx_extend(_acc,_trmc_x10064,({obj: _trmc_x10064, field_name: "tail"}));
      lo_0 = _x29;
      _acc = _x30;
      continue tailcall;
    }
  }
  else {
    return $std_core_types._cctx_apply(_acc,($std_core_types.Nil));
  }
}}
 
export function list64(lo_0_0, hi_0_0) /* (lo : int64, hi : int64) -> list<int64> */  {
  return _trmc_list64(lo_0_0, hi_0_0, $std_core_types._cctx_empty());
}
 
export function sumacc64(xs, acc) /* (xs : list<int64>, acc : int64) -> int64 */  { tailcall: while(1)
{
  if (xs !== null) {
    {
      // tail call
      var _x31 = BigInt.asIntN(64,acc + (xs.head));
      xs = xs.tail;
      acc = _x31;
      continue tailcall;
    }
  }
  else {
    return acc;
  }
}}
 
export function sum64(xs) /* (xs : list<int64>) -> int64 */  {
  return sumacc64(xs, 0n);
}
 
 
// carry-less multiply by -1 gives the _prefix sum_
export function test_prefix_sum() /* () -> string */  {
   
  var x = 1295n;
   
  var i_10023 = $std_core._unsupported_external("std/num/int64/clmul");
  var _x33 = undefined;
  var _x32 = (_x33 !== undefined) ? _x33 : 1;
  var _x35 = undefined;
  var _x34 = (_x35 !== undefined) ? _x35 : true;
  var _x37 = undefined;
  var _x36 = (_x37 !== undefined) ? _x37 : "0x";
  return $std_core_show.show_hex($std_core_types._int_from_int64(i_10023), _x32, _x34, _x36);
}
 
 
// carry-less multiply of x by x spreads the bits
export function test_bit_spread1() /* () -> string */  {
   
  var x = 1535n;
   
  var i_10027 = $std_core._unsupported_external("std/num/int64/clmul");
  var _x39 = undefined;
  var _x38 = (_x39 !== undefined) ? _x39 : 1;
  var _x41 = undefined;
  var _x40 = (_x41 !== undefined) ? _x41 : true;
  var _x43 = undefined;
  var _x42 = (_x43 !== undefined) ? _x43 : "0x";
  return $std_core_show.show_hex($std_core_types._int_from_int64(i_10027), _x38, _x40, _x42);
}
 
export function test_bit_spread2() /* () -> string */  {
   
  var x = 1531n;
   
  var i_10031 = $std_core._unsupported_external("std/num/int64/clmul");
  var _x45 = undefined;
  var _x44 = (_x45 !== undefined) ? _x45 : 1;
  var _x47 = undefined;
  var _x46 = (_x47 !== undefined) ? _x47 : true;
  var _x49 = undefined;
  var _x48 = (_x49 !== undefined) ? _x49 : "0x";
  return $std_core_show.show_hex($std_core_types._int_from_int64(i_10031), _x44, _x46, _x48);
}
 
export function zip_clmul(x) /* (x : int64) -> int64 */  {
  return ($std_core._unsupported_external("std/num/int64/clmul")) | ($std_core._unsupported_external("std/num/int64/clmulr"));
}
 
export function test_clmulr1() /* () -> string */  {
   
  var x = 51n;
   
  var i_10035 = $std_core._unsupported_external("std/num/int64/bit-zip");
   
  var i_0_10039 = ($std_core._unsupported_external("std/num/int64/clmul")) | ($std_core._unsupported_external("std/num/int64/clmulr"));
  var _x51 = undefined;
  var _x50 = (_x51 !== undefined) ? _x51 : 1;
  var _x53 = undefined;
  var _x52 = (_x53 !== undefined) ? _x53 : true;
  var _x55 = undefined;
  var _x54 = (_x55 !== undefined) ? _x55 : "0x";
  var _x57 = undefined;
  var _x56 = (_x57 !== undefined) ? _x57 : 1;
  var _x59 = undefined;
  var _x58 = (_x59 !== undefined) ? _x59 : true;
  var _x61 = undefined;
  var _x60 = (_x61 !== undefined) ? _x61 : "0x";
  return $std_core_types._lp__plus__plus__rp_($std_core_show.show_hex($std_core_types._int_from_int64(i_10035), _x50, _x52, _x54), $std_core_types._lp__plus__plus__rp_(" == ", $std_core_show.show_hex($std_core_types._int_from_int64(i_0_10039), _x56, _x58, _x60)));
}
 
export function test_clmulr2() /* () -> string */  {
   
  var x = int64($std_core_types._int_const(81986702028889840n));
   
  var i_10044 = $std_core._unsupported_external("std/num/int64/bit-zip");
   
  var i_0_10048 = ($std_core._unsupported_external("std/num/int64/clmul")) | ($std_core._unsupported_external("std/num/int64/clmulr"));
  var _x63 = undefined;
  var _x62 = (_x63 !== undefined) ? _x63 : 1;
  var _x65 = undefined;
  var _x64 = (_x65 !== undefined) ? _x65 : true;
  var _x67 = undefined;
  var _x66 = (_x67 !== undefined) ? _x67 : "0x";
  var _x69 = undefined;
  var _x68 = (_x69 !== undefined) ? _x69 : 1;
  var _x71 = undefined;
  var _x70 = (_x71 !== undefined) ? _x71 : true;
  var _x73 = undefined;
  var _x72 = (_x73 !== undefined) ? _x73 : "0x";
  return $std_core_types._lp__plus__plus__rp_($std_core_show.show_hex($std_core_types._int_from_int64(i_10044), _x62, _x64, _x66), $std_core_types._lp__plus__plus__rp_(" == ", $std_core_show.show_hex($std_core_types._int_from_int64(i_0_10048), _x68, _x70, _x72)));
}
 
export function clmulrev(x, y) /* (x : int64, y : int64) -> int64 */  {
  return $std_core_types._int64_breverse(($std_core._unsupported_external("std/num/int64/clmul")));
}
 
export function test_clmulrev1() /* () -> string */  {
   
  var x = 305419896n;
   
  var y = $std_core_types._int64_bswap(2309737967n);
   
  var i_10053 = $std_core._unsupported_external("std/num/int64/clmulr");
   
  var i_0_10057 = $std_core_types._int64_breverse(($std_core._unsupported_external("std/num/int64/clmul")));
  var _x75 = undefined;
  var _x74 = (_x75 !== undefined) ? _x75 : 1;
  var _x77 = undefined;
  var _x76 = (_x77 !== undefined) ? _x77 : true;
  var _x79 = undefined;
  var _x78 = (_x79 !== undefined) ? _x79 : "0x";
  var _x81 = undefined;
  var _x80 = (_x81 !== undefined) ? _x81 : 1;
  var _x83 = undefined;
  var _x82 = (_x83 !== undefined) ? _x83 : true;
  var _x85 = undefined;
  var _x84 = (_x85 !== undefined) ? _x85 : "0x";
  return $std_core_types._lp__plus__plus__rp_($std_core_show.show_hex($std_core_types._int_from_int64(i_10053), _x74, _x76, _x78), $std_core_types._lp__plus__plus__rp_(" == ", $std_core_show.show_hex($std_core_types._int_from_int64(i_0_10057), _x80, _x82, _x84)));
}