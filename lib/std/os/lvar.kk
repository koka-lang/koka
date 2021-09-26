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

abstract struct lvar<a> {
  lv : any;
  join : (a, a) -> maybe<a>;
  compare : (a, a) -> maybe<order>;
}

noinline extern unsafe-lvar( bottom : a ) : pure any {
  c "kk_lvar_alloc"
}

noinline extern unsafe-put( lv : any, update : a -> null<a>) : pure () {
  c "kk_lvar_put"
}

noinline extern unsafe-get( lv : any, in-threshold-set : (a, int32) -> null<a> ) : pure a {
  c "kk_lvar_get"
}

noinline extern unsafe-freeze( lv : any ) : pure a {
  c "kk_lvar_freeze"
}

public noinline fun lvar( bottom : a, join : (a, a) -> maybe<a>, compare : (a, a) -> maybe<order>) : pure lvar<a> {
  Lvar( unsafe-lvar(bottom), join, compare )
}

// Update functions must obey the following laws:
//
//   d <= update(d)                             -- monotonicity
//   update1(update2(d)) == update2(update1(d)) -- commutativity
//
public fun put( lvar : lvar<a>, update : a -> maybe<a> ) : pure () {
  lvar.lv.unsafe-put( fn (d) { d.update.null } )
}

public fun put( lvar : lvar<a>, d : a ) : pure () {
  lvar.put( fn (d') { lvar.join(d, d') } )
}

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

public fun freeze( lvar : lvar<a> ) : pure a {
  lvar.lv.unsafe-freeze
}

private fun is-eq( m : maybe<order> ) : total bool {
  m.maybe(False, fn (ord) { ord == Eq } ) // TODO: default doesn't typecheck !
}

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
