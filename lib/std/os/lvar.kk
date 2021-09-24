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

abstract struct lvar<a> {
  lv : any;
  join : (a, a) -> a;
  is-gte : (a, a) -> maybe<bool>
}

noinline extern unsafe-lvar( bottom : a ) : pure any {
  c "kk_lvar_alloc"
}

noinline extern unsafe-put( lv : any, update : a -> a) : pure () {
  c "kk_lvar_put"
}

noinline extern unsafe-get( lv : any, in-threshold-set : a -> null<a> ) : pure a {
  c "kk_lvar_get"
}

noinline extern unsafe-freeze( lv : any ) : pure () {
  c "kk_lvar_freeze"
}

public noinline fun lvar( bottom : a, join : (a,a) -> a, is-gte : (a, a) -> maybe<bool>) : pure lvar<a> {
  Lvar( unsafe-lvar(bottom), join, is-gte )
}

public fun put( lvar : lvar<a>, x : a ) : pure () {
  put( lvar, fn (y) { join(lvar)(x, y) } )
}

// the update function must obey the following laws:
// update(x) >= x                             -- ordering
// update1(update2(x)) == update2(update1(x)) -- commutativity
public fun put( lvar : lvar<a>, update : a -> a ) : pure () {
  unsafe-put( lvar.lv, update )
}

public fun get( lvar : lvar<a>, in-threshold-set : a -> maybe<a> ) : pure a {
  unsafe-get( lvar.lv, fn (x) { null(in-threshold-set(x)) } )
}

public fun get( lvar : lvar<a>, threshold-set : list<a> ) : pure a {
  get( lvar, fn (x) {
    threshold-set.find(fn (y) {
      is-gte(lvar)(x,y).default(False)
    })
  })
}

public fun freeze( lvar : lvar<a> ) : pure () {
  unsafe-freeze( lvar.lv )
}
