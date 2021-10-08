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

extern prim-task-set-default-concurrency( thread-count : ssize_t  ) : io () {
  c "kk_task_set_default_concurrency"
}

public fun task-set-default-concurrency( thread-count : int ) : io () {
  prim-task-set-default-concurrency( thread-count.ssize_t )
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
  lv : any
}

noinline extern unsafe-lvar( init : a ) : pure any {
  c "kk_lvar_alloc"
}

noinline extern unsafe-put( lvar : any, x : a, monotonic-combine : (a,a) -> a ) : pure () {
  c "kk_lvar_put"
}

noinline extern unsafe-get( lvar : any, bot : a, is-gte : (a,a) -> int32 ) : pure a {
  c "kk_lvar_get"
}

public noinline fun lvar( init : a ) : pure lvar<a> {
  Lvar( unsafe-lvar(init) )
}

public fun put( lvar : lvar<a>, x : a ,monotonic-combine : (a,a) -> a ) : pure () {
  unsafe-put( lvar.lv, x, monotonic-combine )
}

public fun get( lvar : lvar<a>, bot : a, is-gte: (a,a) -> bool ) : pure a {
  unsafe-get( lvar.lv, bot, fn(x,y){ if (is-gte(x,y)) then one else zero } )
}
