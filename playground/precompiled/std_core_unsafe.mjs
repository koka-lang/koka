// Koka generated module: std/core/unsafe, koka version: 3.2.4
"use strict";
 
// imports
import * as $std_core_types from './std_core_types.mjs';
 
// externals
 
// type declarations
 
// declarations
 
 
// _Unsafe_. This function calls a function and pretends it did not have any effect at all.
// Use with utmost care as it should not be used to dismiss user-defined effects that need
// a handler and can cause a segfault at runtime in such cases!
//
// You can use `unsafe-total` to dismiss built-in effects without handlers which include:
//
// - behavioral: `:div` (non-termination/divergence), `:ndet` (non-determinism)
// - state: `:alloc`, `:read`, `:write`, `:st`
// - external: `:ui`, `:fsys`, `:net`, `:blocking`
// - combinations: `:io-total` and `:io-noexn`
//
// Do _not_ dismiss `:io` since it has the `:exn` effect that should be handled (and an evidence
// vector should be passed in).
//
// Try to avoid using `unsafe-total` to initialize global values that have a side-effect, but
// use `std/core/delayed/delay` instead:
// ```
// val myglobal = delay( fn() initialize() )
// fun get-global() : e int
//   myglobal.force
// ```
export function unsafe_total(action) /* forall<a,e> (action : () -> e a) -> a */  {
  return action();
}
 
 
// _Unsafe_. This function pretends the given `action` is deterministic
export function pretend_no_ndet(action) /* forall<a,e> (action : () -> <ndet|e> a) -> e a */  {
  return action();
}