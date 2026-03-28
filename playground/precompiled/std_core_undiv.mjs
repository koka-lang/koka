// Koka generated module: std/core/undiv, koka version: 3.2.4
"use strict";
 
// imports
import * as $std_core_types from './std_core_types.mjs';
 
// externals
 
// type declarations
 
// declarations
 
 
// _Unsafe_. This function pretends that the given action is terminating.
export function pretend_no_div(action) /* forall<a,e> (action : () -> <div|e> a) -> e a */  {
  return action();
}