// Koka generated module: std/core/lazy, koka version: 3.2.4
"use strict";
 
// imports
import * as $std_core_types from './std_core_types.mjs';
 
// externals
 
// type declarations
 
// declarations
 
 
// internal: marker for lazy values that will be memoized (used to for correct reuse and reference counting)
export function memoize_target(target, size, scan_size) /* forall<a> (target : a, size : int, scan-size : int) -> () */  {
  return /**/;
}
 
 
// internal: explicitly force update-in-place for lazy values
export function memoize(target, x) /* forall<a> (target : a, x : a) -> a */  {
  return target;
}
 
export var internal;
var internal = 42;