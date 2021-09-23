/*---------------------------------------------------------------------------
  Copyright 2021-2021, Microsoft Research, Daan Leijen.

    This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the LICENSE file at the root of this distribution.
---------------------------------------------------------------------------*/


/*
Experimental: Parallel tasks.

Note: very experimental and may not work as intended :-)
See ``test/bench/koka/binarytrees.kk`` for example usage.
*/
module std/os/task

import std/num/int32

// A `:promise<a>` can be `await`ed for a result.
abstract struct promise<a> {
  promise : any
}

noinline extern unsafe_task( work : () -> pure a ) : pure any {
  c "kk_task_schedule"
}

noinline extern unsafe_await( p : any ) : pure a {
  c "kk_promise_get"
}

// Spark a pure computation in a separate thread of control.
public noinline fun task( work : () -> pure a ) : pure promise<a> {
  Promise( unsafe_task( work ) )
}

// Await the result of a promise.
public fun await( p : promise<a> ) : pure a {
  unsafe_await( p.promise )
}

// Await the result of a list of promises.
public fun await( ps : list<promise<a>> ) : pure list<a> {
  ps.map(await)
}

// Run a list of pure computations in parallel.
public fun parallel( xs : list<() -> pure a> ) : pure list<a> {
  xs.map( task ).await
}


/*
noinline extern unsafe_task_n( count : ssize_t, stride : ssize_t, work : () -> pure a, combine : (a,a) -> a ) : pure any {
  c "kk_task_schedule_n"
}
public noinline fun taskn( count : int, stride : int, work : () -> pure a, combine : (a,a) -> total a ) : pure promise<a> {
  Promise( unsafe_task_n( count.ssize_t, stride.ssize_t, work, combine ) )
}
*/


// ---------------------------------------------------------
// LVar's
// Note: currently unsafe in the pure effect!! needs work

abstract struct lvar<a> {
  lv : any;
  bottom : a;
  join : (a, a) -> a;
  is-gte : (a, a) -> maybe<bool>
}

noinline extern unsafe-lvar( bottom : a ) : pure any {
  c "kk_lattice_var_alloc"
}

noinline extern unsafe-put( lv : any, join : (a,a) -> a, x : a) : pure () {
  c "kk_lattice_var_put"
}

noinline extern unsafe-get( lv : any, in-threshold-set : a -> null<a> ) : pure a {
  c "kk_lattice_var_get"
}

noinline extern unsafe-freeze( lv : any ) : pure () {
  c "kk_lattice_var_freeze"
}

public noinline fun lvar( bottom : a, join : (a,a) -> a, is-gte : (a, a) -> maybe<bool>) : pure lvar<a> {
  Lvar( unsafe-lvar(bottom), bottom, join, is-gte )
}

public fun put( lvar : lvar<a>, x : a ) : pure () {
  unsafe-put( lvar.lv, lvar.join, x )
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
