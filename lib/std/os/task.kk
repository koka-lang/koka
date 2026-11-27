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
In JavaScript, tasks run sequentially: `task` evaluates its argument right away.
*/
module std/os/task

import std/num/int32

extern import
  js file "task-inline.js"

// A `:promise<a>` can be `await`ed for a result.
abstract struct promise<a>
  promise : any

noinline extern unsafe_task( work : () -> a ) : any
  c  "kk_task_schedule"
  js inline "(#1)()"

noinline extern unsafe_await( p : any ) : a
  c  "kk_promise_get"
  js inline "(#1)"

extern prim-task-set-default-concurrency( thread-count : ssize_t  ) : ioc ()
  c  "kk_task_set_default_concurrency"
  js inline "$std_core_types.Unit"

pub fun task-set-default-concurrency( thread-count : int ) : ioc ()
  prim-task-set-default-concurrency( thread-count.ssize_t )


// Spark a pure computation in a separate thread of control.
pub noinline fun task( work : () -> a ) : promise<a>
  Promise( unsafe_task( work ) )

// Await the result of a promise.
pub fun await( p : promise<a> ) : a
  unsafe_await( p.promise )

// Await the result of a list of promises.
pub fun list/await( ps : list<promise<a>> ) : list<a>
  ps.map(await)

// Run a list of pure computations in parallel.
pub fun parallel( xs : list<() -> a> ) : list<a>
  xs.map( task ).await


/*
noinline extern unsafe_task_n( count : ssize_t, stride : ssize_t, work : () -> pure a, combine : (a,a) -> a ) : pure any
  c "kk_task_schedule_n"

pub noinline fun taskn( count : int, stride : int, work : () -> pure a, combine : (a,a) -> total a ) : pure promise<a>
  Promise( unsafe_task_n( count.ssize_t, stride.ssize_t, work, combine ) )

*/

// ---------------------------------------------------------
// LVar's
// Note: currently unsafe in the pure effect!! needs work

abstract struct lvar<a>
  lv : any

noinline extern unsafe-lvar( init : a ) : any
  c  "kk_lvar_alloc"
  js inline "({ value: (#1) })"

noinline extern unsafe-put( lvar : any, x : a, monotonic-combine : (a,a) -> a ) : ()
  c  "kk_lvar_put"
  js inline "((#1).value = (#3)((#1).value, #2), $std_core_types.Unit)"

noinline extern unsafe-get( lvar : any, bot : a, is-gte : (a,a) -> int32 ) : ndet a
  c  "kk_lvar_get"
  js "_lvar_get"

pub noinline fun lvar( init : a ) : lvar<a>
  Lvar( unsafe-lvar(init) )

pub fun put( lvar : lvar<a>, x : a, monotonic-combine : (a,a) -> a ) : ()
  unsafe-put( lvar.lv, x, monotonic-combine )

pub fun get( lvar : lvar<a>, bot : a, is-gte: (a,a) -> bool ) : ndet a
  unsafe-get( lvar.lv, bot, fn(x,y){ if (is-gte(x,y)) then one else zero } )
