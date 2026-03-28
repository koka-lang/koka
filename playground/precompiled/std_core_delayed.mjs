// Koka generated module: std/core/delayed, koka version: 3.2.4
"use strict";
 
// imports
import * as $std_core_types from './std_core_types.mjs';
import * as $std_core_hnd from './std_core_hnd.mjs';
import * as $std_core_unsafe from './std_core_unsafe.mjs';
 
// externals
 
// type declarations
// type computation
export function XComputation(action) /* forall<e,a> (action : () -> e a) -> computation<e,a> */  {
  return { _tag: 1, action: action };
}
export function XDone(value) /* forall<e,a> (value : a) -> computation<e,a> */  {
  return { _tag: 2, value: value };
}
export const XBlocking = { _tag: 3 }; // forall<e,a> computation<e,a>
// type delayed
export function XDelay(dref) /* forall<e,a> (dref : ref<global,computation<e,a>>) -> delayed<e,a> */  {
  return dref;
}
 
// declarations
 
 
// Automatically generated. Tests for the `XComputation` constructor of the `:computation` type.
export function is_xcomputation(computation) /* forall<a,e> (computation : computation<e,a>) -> bool */  {
  return (computation._tag === 1);
}
 
 
// Automatically generated. Tests for the `XDone` constructor of the `:computation` type.
export function is_xdone(computation) /* forall<a,e> (computation : computation<e,a>) -> bool */  {
  return (computation._tag === 2);
}
 
 
// Automatically generated. Tests for the `XBlocking` constructor of the `:computation` type.
export function is_xblocking(computation) /* forall<a,e> (computation : computation<e,a>) -> bool */  {
  return (computation._tag === 3);
}
 
 
// Automatically generated. Retrieves the `dref` constructor field of the `:delayed` type.
export function delayed_fs_dref(delayed) /* forall<e,a> (delayed : delayed<e,a>) -> ref<global,computation<e,a>> */  {
  return delayed;
}
 
export function delayed_fs__copy(_this, dref) /* forall<e,a> (delayed<e,a>, dref : ? (ref<global,computation<e,a>>)) -> delayed<e,a> */  {
  if (dref !== undefined) {
    var _x0 = dref;
  }
  else {
    var _x0 = _this;
  }
  return _x0;
}
 
 
// Create a new `:delayed` value.
export function delay(action) /* forall<a,e> (action : () -> e a) -> delayed<e,a> */  {
   
  var r = { value: (XComputation(action)) };
  return r;
}
 
 
// Create a new `:delayed` value.
export function memo(value) /* forall<a,e> (value : a) -> delayed<e,a> */  {
   
  var r = { value: (XDone(value)) };
  return r;
}
 
 
// monadic lift
export function force_fs__mlift_go_10015(r, x_0) /* forall<a,e> (r : ref<global,computation<e,a>>, x@0 : a) -> <st<global>,div|e> a */  {
   
  ((r).value = (XDone(x_0)));
  return x_0;
}
 
export function force_fs_go(delayed) /* forall<a,e> (delayed : delayed<e,a>) -> <st<global>,div|e> a */  { tailcall: while(1)
{
  var _x2 = delayed;
  var _x1 = _x2.value;
  if (_x1._tag === 2) {
    return _x1.value;
  }
  else if (_x1._tag === 3) {
    {
      // tail call
      continue tailcall;
    }
  }
  else {
     
    var _x3 = delayed;
    ((_x3).value = XBlocking);
     
    var x_1_10016 = _x1.action();
    if ($std_core_hnd._yielding()) {
      return $std_core_hnd.yield_extend(function(x_0_0 /* 543 */ ) {
        var _x3 = delayed;
        return force_fs__mlift_go_10015(_x3, x_0_0);
      });
    }
    else {
       
      var _x4 = delayed;
      ((_x4).value = (XDone(x_1_10016)));
      return x_1_10016;
    }
  }
}}
 
export function unsafe_no_state_div_cast(f) /* forall<a,e> (f : () -> <st<global>,div|e> a) -> (() -> e a) */  {
  return f;
}
 
export function unsafe_no_state_div(f) /* forall<a,e> (f : () -> <st<global>,div|e> a) -> e a */  {
  return unsafe_no_state_div_cast(f)();
}
 
 
// Force a delayed value; the value is computed only on the first
// call to `force` and cached afterwards.
export function force(delayed) /* forall<a,e> (delayed : delayed<e,a>) -> e a */  {
  return unsafe_no_state_div_cast(function() {
    return force_fs_go(delayed);
  })();
}
 
 
// Given a total function to calculate a value `:a`, return
// a total function that only calculates the value once and then
// returns the cached result.
export function once(calc) /* forall<a> (calc : () -> a) -> (() -> a) */  {
   
  var r = { value: ($std_core_types.Nothing) };
  return function() {
    var _x4 = r.value;
    if (_x4 !== null) {
      return _x4.value;
    }
    else {
       
      var x_0 = calc();
       
      ((r).value = ($std_core_types.Just(x_0)));
      return x_0;
    }
  };
}