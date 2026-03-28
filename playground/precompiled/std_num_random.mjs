// Koka generated module: std/num/random, koka version: 3.2.4
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
import * as $std_num_int32 from './std_num_int32.mjs';
import * as $std_num_int64 from './std_num_int64.mjs';
import * as $std_num_float64 from './std_num_float64.mjs';
 
// externals
/*---------------------------------------------------------------------------
-- Copyright 2020-2021, Microsoft Research, Daan Leijen.
--
-- This is free software; you can redistribute it and/or modify it under the
-- terms of the Apache License, Version 2.0. A copy of the License can be
-- found in the LICENSE file at the root of this distribution.
---------------------------------------------------------------------------*/
var crypto = null;
if ($std_core.host() === "node") {
  crypto = await import('crypto');
}
var random_out = new Int32Array(32);
var random_used = random_out.length;
var random_strong = false;
function _srandom_round() {
  if (crypto && crypto.randomFillSync) {
    random_strong = true;
    crypto.randomFillSync(random_out);
  }
  else if (window && window.crypto && window.crypto.getRandomValues) {
    random_strong = true;
    window.crypto.getRandomValues(random_out);
  }
  else if (window && window.msCrypto && window.msCrypto.getRandomValues) {
    random_strong = true;
    window.msCrypto.getRandomValues(random_out);
  }
  else {
    random_strong = false;
    for(var i = 0; i < random_out.length; i++) {
      random_out[i] = ((2.0*Math.random() - 1.0)*2147483647.0)|0;
    }
  }
  random_used = 0;  
}
function _srandom_is_strong() {
  return random_strong;
}
function _srandom_int32() {
  if (random_out.length <= random_used) {
    _srandom_round();
  }
  return random_out[random_used++];
}
function _srandom_double() {
  // use 52-bits for a double in the range [0,1)
  var lo = _srandom_int32();
  var hi = _srandom_int32();
  hi = (0x3FF00000 | (hi >>> 12))|0;
  //lo = (lo << 4)|0;
  return ($std_num_double.double_from_bits(lo,hi) - 1.0); 
}
function _srandom_uint32() {
  return _srandom_int32() + 0x80000000;
}
function _srandom_range_uint32(hi) {  
  if (hi === 0) return 0;
  if (hi === 0xFFFFFFFF) return _srandom_uint32();
  var r = 0xFFFFFFFF % hi;
  var x;
  do {
    x = _srandom_uint32();
  } while (x >= (0xFFFFFFFF - r)); // todo: limit potential to take a loooong time?
  return (x % hi);
}
function _srandom_range_int32(lo,hi) {
  if (lo > hi) {
    var x = lo;
    lo = hi;
    hi = x;
  }
  var delta = hi - lo;
  var x = _srandom_range_uint32(delta);
  return (lo + x);
}
 
// type declarations
 
 
// runtime tag for the effect `:random`
export var random_fs__tag;
var random_fs__tag = "random@random";
// type random
export function _Hnd_random(_cfc, _fun_random_int32) /* forall<e,a> (int, hnd/clause0<int32,random,e,a>) -> random<e,a> */  {
  return { _cfc: _cfc, _fun_random_int32: _fun_random_int32 };
}
// type sfc
export function Sfc(x, y, z, cnt) /* (x : int32, y : int32, z : int32, cnt : int32) -> sfc */  {
  return { x: x, y: y, z: z, cnt: cnt };
}
// type sfc-result
export function Sfc_result(rnd, rstate) /* (rnd : int32, rstate : sfc) -> sfc-result */  {
  return { rnd: rnd, rstate: rstate };
}
 
// declarations
 
 
// Automatically generated. Retrieves the `@cfc` constructor field of the `:random` type.
export function random_fs__cfc(random) /* forall<e,a> (random : random<e,a>) -> int */  {
  return random._cfc;
}
 
 
// handler for the effect `:random`
export function random_fs__handle(hnd, ret, action) /* forall<a,e,b> (hnd : random<e,b>, ret : (res : a) -> e b, action : () -> <random|e> a) -> e b */  {
  return $std_core_hnd._hhandle(random_fs__tag, hnd, ret, action);
}
 
 
// select `random-int32` operation out of effect `:random`
export function random_int32_fs__select(hnd) /* forall<e,a> (hnd : random<e,a>) -> hnd/clause0<int32,random,e,a> */  {
  return hnd._fun_random_int32;
}
 
 
// Call the `fun random-int32` operation of the effect `:random`
export function random_int32() /* () -> random int32 */  {
   
  var ev_10051 = $std_core_hnd._evv_at(0);
  return ev_10051.hnd._fun_random_int32(ev_10051.marker, ev_10051);
}
 
 
// Automatically generated. Retrieves the `@fun-random-int32` constructor field of the `:random` type.
export function random_fs__fun_random_int32(random) /* forall<e,a> (random : random<e,a>) -> hnd/clause0<int32,random,e,a> */  {
  return random._fun_random_int32;
}
 
 
// Automatically generated. Retrieves the `cnt` constructor field of the `:sfc` type.
export function sfc_fs_cnt(sfc) /* (sfc : sfc) -> int32 */  {
  return sfc.cnt;
}
 
 
// Automatically generated. Retrieves the `x` constructor field of the `:sfc` type.
export function sfc_fs_x(sfc) /* (sfc : sfc) -> int32 */  {
  return sfc.x;
}
 
 
// Automatically generated. Retrieves the `y` constructor field of the `:sfc` type.
export function sfc_fs_y(sfc) /* (sfc : sfc) -> int32 */  {
  return sfc.y;
}
 
 
// Automatically generated. Retrieves the `z` constructor field of the `:sfc` type.
export function sfc_fs_z(sfc) /* (sfc : sfc) -> int32 */  {
  return sfc.z;
}
 
export function sfc_fs__copy(_this, x, y, z, cnt) /* (sfc, x : ? int32, y : ? int32, z : ? int32, cnt : ? int32) -> sfc */  {
  if (x !== undefined) {
    var _x0 = x;
  }
  else {
    var _x0 = _this.x;
  }
  if (y !== undefined) {
    var _x1 = y;
  }
  else {
    var _x1 = _this.y;
  }
  if (z !== undefined) {
    var _x2 = z;
  }
  else {
    var _x2 = _this.z;
  }
  if (cnt !== undefined) {
    var _x3 = cnt;
  }
  else {
    var _x3 = _this.cnt;
  }
  return Sfc(_x0, _x1, _x2, _x3);
}
 
 
// Automatically generated. Retrieves the `rnd` constructor field of the `:sfc-result` type.
export function sfc_result_fs_rnd(_this) /* (sfc-result) -> int32 */  {
  return _this.rnd;
}
 
 
// Automatically generated. Retrieves the `rstate` constructor field of the `:sfc-result` type.
export function sfc_result_fs_rstate(_this) /* (sfc-result) -> sfc */  {
  return _this.rstate;
}
 
export function sfc_result_fs__copy(_this, rnd, rstate) /* (sfc-result, rnd : ? int32, rstate : ? sfc) -> sfc-result */  {
  if (rnd !== undefined) {
    var _x4 = rnd;
  }
  else {
    var _x4 = _this.rnd;
  }
  if (rstate !== undefined) {
    var _x5 = rstate;
  }
  else {
    var _x5 = _this.rstate;
  }
  return Sfc_result(_x4, _x5);
}
 
 
// Return a strong random `:int32`
export function srandom_int32() /* () -> ndet int32 */  {
  return _srandom_int32();
}
 
 
// Pick random numbers from a the best strong random source in the OS.
// (e.g. like `/dev/urandom`, `arc4random` etc.). Use `srandom-is-strong` to test if the
// numbers are indeed based on a strong random source.
export function strong_random(action) /* forall<a,e> (action : () -> <random,ndet|e> a) -> <ndet|e> a */  {
  return random_fs__handle(_Hnd_random(1, $std_core_hnd.clause_tail0(srandom_int32)), function(_res /* 477 */ ) {
      return _res;
    }, action);
}
 
export function _default_random(action) /* forall<a,e> (action : () -> <random,ndet|e> a) -> <ndet|e> a */  {
  return random_fs__handle(_Hnd_random(1, $std_core_hnd.clause_tail0(srandom_int32)), function(_res /* 505 */ ) {
      return _res;
    }, action);
}
 
export function sfc_step(sfc) /* (sfc : sfc) -> sfc-result */  {
   
  var res = ((((((sfc.x) + (sfc.y))|0)) + (sfc.cnt))|0);
   
  var y_0_10007 = (sfc.y) >>> 9;
  return Sfc_result(res, Sfc(((sfc.y) ^ y_0_10007), (((sfc.z) + ((sfc.z) << 3))|0), ((($std_core_types._int32_rotl((sfc.z),21)) + res)|0), (((sfc.cnt) + 1)|0)));
}
 
export function sfc_init32(seed1, seed2) /* (seed1 : int32, seed2 : int32) -> sfc */  {
   
  var sfc0 = Sfc(0, seed1, seed2, 1);
  return $std_num_int32.fold_int32(12, sfc0, function(___wildcard_x57__34 /* int32 */ , s /* sfc */ ) {
       
      var _this_10014 = sfc_step(s);
      return _this_10014.rstate;
    });
}
 
export function sfc_init(seed) /* (seed : int) -> sfc */  {
   
  var seed1_10015 = $std_core_types._int_clamp32(seed);
   
  var seed2_10016 = $std_core_types._int_clamp32(($std_core_types._int_div(seed,4294967296)));
   
  var sfc0 = Sfc(0, seed1_10015, seed2_10016, 1);
  return $std_num_int32.fold_int32(12, sfc0, function(___wildcard_x57__34 /* int32 */ , s /* sfc */ ) {
       
      var _this_10017 = sfc_step(s);
      return _this_10017.rstate;
    });
}
 
 
// monadic lift
export function _mlift_pseudo_random_10043(sfc, wild__) /* forall<h,e> (sfc : sfc-result, wild_ : ()) -> <local<h>|e> int32 */  {
  return sfc.rnd;
}
 
 
// monadic lift
export function _mlift_pseudo_random_10044(s, _y_x10023) /* forall<h,e> (s : local-var<h,sfc>, sfc) -> <local<h>|e> int32 */  {
   
  var sfc = sfc_step(_y_x10023);
   
  var _x6 = sfc.rstate;
  var x_10054 = ((s).value = _x6);
  if ($std_core_hnd._yielding()) {
    return $std_core_hnd.yield_extend(function(wild__ /* () */ ) {
      return sfc.rnd;
    });
  }
  else {
    return sfc.rnd;
  }
}
 
 
// Use pseudo random numbers given some initial `seed`. At most
// 64-bits of the initial seed are used. Do not use this for
// cryptographic applications (use `strong-random` instead).
// Uses _sfc32_ by Chris Doty-Humphrey which is a fast random
// number generator with a 128-bit internal state which
// passes PractRand and BigCrush. The worst case minimum cycle
// is 2^32^, where a potential cycle of 2^48^ has a chance
// of 2^-80^.
export function pseudo_random(seed, action) /* forall<a,e> (seed : int, action : () -> <random|e> a) -> e a */  {
  return function() {
     
    var init_10058 = sfc_init(seed);
     
    var loc = { value: init_10058 };
     
    var res = random_fs__handle(_Hnd_random(1, $std_core_hnd.clause_tail0(function() {
           
          var x_10060 = ((loc).value);
          if ($std_core_hnd._yielding()) {
            return $std_core_hnd.yield_extend(function(_y_x10023 /* sfc */ ) {
              return _mlift_pseudo_random_10044(loc, _y_x10023);
            });
          }
          else {
            return _mlift_pseudo_random_10044(loc, x_10060);
          }
        })), function(_res /* 813 */ ) {
        return _res;
      }, action);
    return $std_core_hnd.prompt_local_var(loc, res);
  }();
}
 
 
// monadic lift
export function _mlift_random_bool_10045(_y_x10029) /* (int32) -> random bool */  {
  return (_y_x10029 >= 0);
}
 
 
// Return a random boolean
export function random_bool() /* () -> random bool */  {
   
  var ev_10066 = $std_core_hnd._evv_at(0);
   
  var x_10063 = ev_10066.hnd._fun_random_int32(ev_10066.marker, ev_10066);
  if ($std_core_hnd._yielding()) {
    return $std_core_hnd.yield_extend(_mlift_random_bool_10045);
  }
  else {
    return (x_10063 >= 0);
  }
}
 
 
// monadic lift
export function _mlift_random_int_10046(_y_x10030) /* (int32) -> random int */  {
  return $std_core_types._int_from_int32(_y_x10030);
}
 
 
// Return a random integer in the range [-2^31^, 2^31^).
export function random_int() /* () -> random int */  {
   
  var ev_10071 = $std_core_hnd._evv_at(0);
   
  var x_10068 = ev_10071.hnd._fun_random_int32(ev_10071.marker, ev_10071);
  if ($std_core_hnd._yielding()) {
    return $std_core_hnd.yield_extend(_mlift_random_int_10046);
  }
  else {
    return $std_core_types._int_from_int32(x_10068);
  }
}
 
 
// monadic lift
export function _mlift_random_int64_10047(_y_x10031, _y_x10032) /* (int32, int32) -> random int64 */  {
  return $std_core_types._int64_hi_lo(_y_x10031,_y_x10032);
}
 
 
// monadic lift
export function _mlift_random_int64_10048(_y_x10031) /* (int32) -> random int64 */  {
   
  var ev_10075 = $std_core_hnd._evv_at(0);
   
  var x_10073 = ev_10075.hnd._fun_random_int32(ev_10075.marker, ev_10075);
  if ($std_core_hnd._yielding()) {
    return $std_core_hnd.yield_extend(function(_y_x10032 /* int32 */ ) {
      return $std_core_types._int64_hi_lo(_y_x10031,_y_x10032);
    });
  }
  else {
    return $std_core_types._int64_hi_lo(_y_x10031,x_10073);
  }
}
 
export function random_int64() /* () -> random int64 */  {
   
  var ev_10082 = $std_core_hnd._evv_at(0);
   
  var x_10079 = ev_10082.hnd._fun_random_int32(ev_10082.marker, ev_10082);
  if ($std_core_hnd._yielding()) {
    return $std_core_hnd.yield_extend(_mlift_random_int64_10048);
  }
  else {
     
    var ev_0_10087 = $std_core_hnd._evv_at(0);
     
    var x_0_10084 = ev_0_10087.hnd._fun_random_int32(ev_0_10087.marker, ev_0_10087);
    if ($std_core_hnd._yielding()) {
      return $std_core_hnd.yield_extend(function(_y_x10032 /* int32 */ ) {
        return $std_core_types._int64_hi_lo(x_10079,_y_x10032);
      });
    }
    else {
      return $std_core_types._int64_hi_lo(x_10079,x_0_10084);
    }
  }
}
 
 
// monadic lift
export function _mlift_random_float64_10049(_y_x10033) /* (int64) -> random float64 */  {
   
  var mag = ($std_core_hnd._open_none2(function(i /* int64 */ , shift /* int */ ) {
      return $std_core_types._int64_shr(i,($std_num_int64.int64(shift)));
    }, _y_x10033, 12)) | ($std_core_hnd._open_none1($std_num_int64.int64, $std_core_types._int_const(4607182418800017408n)));
  return (($std_core_hnd._open_none1($std_num_float64.float64_from_bits, mag)) - (1.0));
}
 
 
// Return a random float64 in the range [0,1) using 52-bits of randomness
export function random_float64() /* () -> random float64 */  {
   
  var x_10091 = random_int64();
  if ($std_core_hnd._yielding()) {
    return $std_core_hnd.yield_extend(_mlift_random_float64_10049);
  }
  else {
     
    var mag = ($std_core_hnd._open_none2(function(i /* int64 */ , shift /* int */ ) {
        return $std_core_types._int64_shr(i,($std_num_int64.int64(shift)));
      }, x_10091, 12)) | ($std_core_hnd._open_none1($std_num_int64.int64, $std_core_types._int_const(4607182418800017408n)));
    return (($std_core_hnd._open_none1($std_num_float64.float64_from_bits, mag)) - (1.0));
  }
}
 
 
// Return a strong random boolean
export function srandom_bool() /* () -> ndet bool */  {
  return ((srandom_int32()) >= 0);
}
 
 
// Returns one of its arguments `x`  or `y`  based on a non-deterministic choice.
export function choose(x, y) /* forall<a> (x : a, y : a) -> ndet a */  {
  var _x6 = ((srandom_int32()) >= 0);
  return (_x6) ? x : y;
}
 
 
// Return a strong random integer in the range [-2^31^, 2^31^).
export function srandom_int() /* () -> ndet int */  {
  return $std_core_types._int_from_int32((srandom_int32()));
}
 
 
// Return a strong random `:float64` in the range [0,1) using 52-bits of randomness
export function srandom_float64() /* () -> ndet float64 */  {
  return _srandom_double();
}
 
 
// Are the strong random numbers generated from a strong random source? (like /dev/urandom)
export function srandom_is_strong() /* () -> ndet bool */  {
  return _srandom_is_strong();
}
 
 
// Return a strong random `:int32` uniformly distributed in the range [lo,hi)
export function srandom_int32_range(lo, hi) /* (lo : int32, hi : int32) -> ndet int32 */  {
  return _srandom_range_int32(lo,hi);
}
 
 
// Generate a strong random float64 uniformly distributed in the range [lo, hi)
export function srandom_float64_range(lo, hi) /* (lo : float64, hi : float64) -> ndet float64 */  {
   
  var low = ((lo <= hi)) ? lo : hi;
   
  var high = ((lo <= hi)) ? hi : lo;
   
  var x = (((((high - low)) * (srandom_float64()))) + low);
  return ((x >= high)) ? low : x;
}