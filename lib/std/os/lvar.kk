/*---------------------------------------------------------------------------

Copyright 2020-2021, April Gonçalves, Wen Kokke.

This is free software; you can redistribute it and/or modify it under the
terms of the Apache License, Version 2.0. A copy of the License can be
found in the LICENSE file at the root of this distribution.
---------------------------------------------------------------------------

LVars, lattice-based mutable shared state

Implementation of Kuper et al's LVars.
Stability: Unstable

*/

module std/os/lvar

import std/num/int32

// an LVar comprises a value and two functions that characterise comparing two values
// for the "biggest" one, one returning said value and a second one returning their ordering.
// however, they return a maybe value since the two values may not be comparable.
// mathematically, those functions represent a partial order, and should be representable
// in terms of each other, however, Koka's type system cannot express such invariants,
// so we keep both functions are provided by the user.
abstract struct lvar<a> {
  lv : any;
  join : (a, a) -> maybe<a>;
  compare : (a, a) -> maybe<order>;
}


// FFI for the C implementation
noinline extern unsafe-lvar( bottom : a ) : total any {
  c "kk_lvar_alloc"
}

noinline extern unsafe-put( lv : any, update : a -> null<a>) : total int32 {
  c "kk_lvar_put"
}

// we need to get a null<a> but we don't know how
// `test/lib/lvar3.kk` tests that behaviour
noinline extern unsafe-get( lv : any, in-threshold-set : (a, int32) -> null<a> ) : total a {
  c "kk_lvar_get"
}

noinline extern unsafe-freeze( lv : any ) : total a {
  c "kk_lvar_freeze"
}

// Here, a better interface to LVars is provided

// New LVars take an initial value and the characteristic partial order functions
// The initial value, together with said functions creates a lattice variable.
// Theoretical note: the mathematical structure lattice require more properties,
// but for a lattice variable (LVar), this is enough.
public noinline fun lvar( bottom : a, join : (a, a) -> maybe<a>, compare : (a, a) -> maybe<order>) : total lvar<a> {
  Lvar( unsafe-lvar(bottom), join, compare )
}

// `put` adds a value to an LVar via an update function,
// and is undefined if the new value is not comparable
// or the current LVar is frozen (ie immutable)
// Update functions must obey the following laws:
//
//   d <= update(d)                             -- monotonicity
//   update1(update2(d)) == update2(update1(d)) -- commutativity
//
public fun put( lvar : lvar<a>, update : a -> maybe<a> ) : exn () {
  match (lvar.lv.unsafe-put( fn (d) { d.update.null } ).bool) {
    True -> ()
    False -> throw("LVar was frozen when `put` was attempted")
  }
}

// Variant of `put` where `update = id a`
public fun put( lvar : lvar<a>, d : a ) : pure () {
  lvar.put( fn (d') { lvar.join(d, d') } )
}

// `get` observes value changes of the LVar and notify user that a threshold has been reached
// The returned value is always a value within the threshold set.
// The status of the LVar (frozen or not) should be explicitly passed as a bool.
// Note: all values within the threshold set should be "incomparable"
public fun get( lvar : lvar<a>, in-threshold-set : (a, bool) -> maybe<a> ) : pure a {
  lvar.lv.unsafe-get( fn (d, frz) { in-threshold-set(d, frz.bool).null } )
}

// Variant of `get` which assumes every value in the threshold set is unfrozen.
public fun get( lvar : lvar<a>, in-threshold-set : a -> maybe<a> ) : pure a {
  lvar.get( fn (d, frz) { if frz then Nothing else d.in-threshold-set  } )
}

// Variant of `get` which represents the threshold set as an explicit list of values and frozen status bits.
public fun get( lvar : lvar<a>, threshold-set : list<(a, bool)> ) : pure a {
  lvar.get( fn (d, frz) { threshold-set.find( fn (p') { lvar.compare.is-gte((d, frz), p') } ).map(fst) } )
}

// Variant of `get` which represents the threshold set as an explicit list of values and assumes all values are unfrozen.
public fun get( lvar : lvar<a>, threshold-set : list<a> ) : pure a {
  lvar.get( threshold-set.map( fn (d) { (d, False) } ) )
}

// `frezze` exposes the C interface
public fun freeze( lvar : lvar<a> ) : pure a {
  lvar.lv.unsafe-freeze
}

// Conversion of `EQ`  to bool
private fun is-eq( m : maybe<order> ) : total bool {
  m.maybe(False, fn (ord) { ord == Eq } ) // TODO: default doesn't typecheck !
}

// Conversion of `Gt || Eq` to bool
private fun is-gte( m : maybe<order> ) : total bool {
   m.maybe(False, fn (ord) { ord == Gt || ord == Eq }) // TODO: default doesn't typecheck !
}

// Lift the partial comparison operation on elements of the lattice to a >= comparison on
// elements accompanied by frozen status bits.
private fun is-gte( compare : (a, a) -> total maybe<order>, p : (a, bool), p' : (a, bool)) : total bool {
   match (p, p') {
     ((d, False), (d', False)) -> compare(d, d').is-gte
     ((d, True) , (d', True))  -> compare(d, d').is-eq
     ((d, False), (d', True))  -> compare(d, d').is-gte
     // Originally, Kuper checks if d' is top, which is necessary as top is an explicit element of the lattice type a,
     // even though such a state should never arise. Our implementation uses the type maybe<a> to represent the lattice,
     // where Nothing is top, and rule out the state where the lvar stores top via the type system, as it stores a value
     // of type a rather than maybe<a>.
     _ -> False // d' == top
   }
}

// Syntactic sugar to allow us to write `lvar.join(d, d')`
public fun join( lv : lvar<a>, d : a, d' : a ) : total maybe<a> {
  join(lv)(d, d')
}
