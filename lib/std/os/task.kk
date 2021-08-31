/*---------------------------------------------------------------------------
  Copyright 2021-2021, Microsoft Research, Daan Leijen.

    This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the LICENSE file at the root of this distribution.
---------------------------------------------------------------------------*/


/*
Parallel tasks.
*/
module std/os/task

abstract struct promise<a> {
  promise : any
}

noinline extern unsafe_task( work : () -> pure a ) : pure any {
  c "kk_task_schedule"
}

noinline extern unsafe_await( p : any ) : pure a {
  c "kk_promise_get"
}

noinline extern unsafe_task_n( count : ssize_t, stride : ssize_t, work : () -> pure a, combine : (a,a) -> a ) : pure any {
  c "kk_task_schedule_n"
}

public noinline fun task( work : () -> pure a ) : pure promise<a> {
  Promise( unsafe_task( work ) )
}

public fun await( p : promise<a> ) : pure a {
  unsafe_await( p.promise )
}

public fun await( ps : list<promise<a>> ) : pure list<a> {
  ps.map(await)
}

public fun parallel( xs : list<() -> pure a> ) : pure list<a> {
  xs.map( task ).await
}

public noinline fun taskn( count : int, stride : int, work : () -> pure a, combine : (a,a) -> total a ) : pure promise<a> {
  Promise( unsafe_task_n( count.ssize_t, stride.ssize_t, work, combine ) )
}


/*
public fun preduce( count : int32, stride : int32, f : (int32,int32) -> pure a, combine : (a,a) -> total a ) : pure b {
  val c = atomic(zero)
  fun worker(init) {
    val i = atomic-add(stride)
    if (i > count) return acc
    val cnt = if (i+stride > count) then count - i else stride
    val y   = f(i,cnt)
    val next = match(init) {
                 Nothing -> y
                 Just(x) -> combine(x,y)
               }
    worker(Just(next))
  }
  val xs = list(1,get-cpu-count()).map(fn(_){ worker(Nothing) }).parallel()
  return xs.foldl1(combine)
}
*/