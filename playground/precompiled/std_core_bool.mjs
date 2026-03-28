// Koka generated module: std/core/bool, koka version: 3.2.4
"use strict";
 
// imports
import * as $std_core_types from './std_core_types.mjs';
 
// externals
 
// type declarations
 
// declarations
 
export function _lp__eq__eq__rp_(x, y) /* (x : bool, y : bool) -> bool */  {
  if (x) {
    return y;
  }
  else {
    return (y) ? false : true;
  }
}
 
export function _lp__excl__eq__rp_(x, y) /* (x : bool, y : bool) -> bool */  {
  if (x) {
    return (y) ? false : true;
  }
  else {
    return y;
  }
}
 
export function _lp__lt__rp_(x, y) /* (x : bool, y : bool) -> bool */  {
  return (x) ? false : y;
}
 
export function _lp__gt__rp_(x, y) /* (x : bool, y : bool) -> bool */  {
  if (x) {
    return (y) ? false : true;
  }
  else {
    return false;
  }
}
 
export function _lp__lt__eq__rp_(x, y) /* (x : bool, y : bool) -> bool */  {
  if (x) {
    return (y);
  }
  else {
    return true;
  }
}
 
export function _lp__gt__eq__rp_(x, y) /* (x : bool, y : bool) -> bool */  {
  if (x) {
    return true;
  }
  else {
    return (y) ? false : true;
  }
}
 
export function xor(x, y) /* (x : bool, y : bool) -> bool */  {
  if (!x) {
    return y;
  }
  else {
    return (y) ? false : true;
  }
}
 
 
// Compare two booleans with `False < True`.
export function cmp(x, y) /* (x : bool, y : bool) -> order */  {
  if (x) {
    if (x) {
      return (y) ? $std_core_types.Eq : $std_core_types.Gt;
    }
    else {
      return $std_core_types.Eq;
    }
  }
  else {
    if (y) {
      return $std_core_types.Lt;
    }
    else {
      if (x) {
        return (y) ? $std_core_types.Eq : $std_core_types.Gt;
      }
      else {
        return $std_core_types.Eq;
      }
    }
  }
}
 
 
// Order two booleans in ascending order.
export function order2(x, y) /* (x : bool, y : bool) -> order2<bool> */  {
  if (x) {
    var _x0 = y;
  }
  else {
    var _x0 = (y) ? false : true;
  }
  if (_x0) {
    return $std_core_types.Eq2(x);
  }
  else {
    if (x) {
      return $std_core_types.Gt2(y, x);
    }
    else {
      if (y) {
        return $std_core_types.Lt2(x, y);
      }
      else {
        return $std_core_types.Gt2(y, x);
      }
    }
  }
}
 
 
// Convert a `:bool` to a string
export function show(b) /* (b : bool) -> string */  {
  return (b) ? "True" : "False";
}
 
 
// Convert a boolean to an `:int`
export function int(b) /* (b : bool) -> int */  {
  return (b) ? 1 : 0;
}