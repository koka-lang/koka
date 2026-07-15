/*---------------------------------------------------------------------------
  Copyright 2026, Tim Whiting, Microsoft Research, Daan Leijen.

  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the LICENSE file at the root of this distribution.
---------------------------------------------------------------------------*/

/* Threading for asynchronous programs.

This module offloads heavy, self-contained computations onto worker threads so
they do not block the async loop that requested them. On native (`libc`) hosts
the work runs on a libuv threadpool thread with its own Koka heap; the result is
marked _thread-shared_ (atomic refcounts) before it crosses back to the
requesting loop. On hosts without OS threads (web/wasi) `compute` degrades to a
cooperative strand with the exact same API, so applications stay portable.

# Data offload

    fun main()
      val n = compute fn() heavy-pure-work()   // runs off the async loop
      println("result: " ++ n.show)

While `compute` runs, the requesting loop keeps servicing other strands (timers,
subscriptions, animation), so a UI thread stays responsive during heavy work.
*/
module async/thread

import std/core/unsafe
import std/core/exn
import async/async

// Select the host threading backend. Native uses real OS worker threads (libuv
// threadpool + uv_thread); every JavaScript/Wasm host degrades to a cooperative
// strand. `pub import` re-exports the backend's portable surface -- `xchannel`,
// `xsender`, `spawn-thread`, `native-threads` -- through `std/async/thread`.
pub import async/api/thread
  [host=libc]                          async/api/uv/thread
  [host=jsnode|jsweb|wasmweb|wasi-web] async/api/coop/thread

// Offload the pure computation `work` onto a worker thread (native) or a
// cooperative strand (web/wasi), and await its result on the current async loop
// without blocking it. Other strands (timers, subscriptions, animations) keep
// running while `work` executes.
//
// `work` must be _self-contained_: it may only read thread-shared/immutable
// values captured in its closure and must not mutate shared state or perform
// I/O — on native hosts it runs on a separate thread with NO effect handlers in
// scope (a worker starts with an empty evidence vector). `work` is `pure`
// (`<exn,div>`), and `exn` is handler-based, so we must INSTALL a fresh `exn`
// handler on the worker itself: `error/try(work)` does that when the thunk runs
// there, catching any exception into an `error<a>` that crosses back and is
// rethrown here. (`unsafe-total(work)` would only cast the effect away and
// segfault — its own docs warn of this; here `unsafe-total` dismisses just the
// leftover `div`, which is handler-free and safe.) Its result is marked
// thread-shared before it crosses back.
pub fun compute( work : () -> pure a ) : async a
  match await-compute( fn() unsafe-total{ error/try(work) } )
    Ok(x)    -> x
    Error(e) -> impossible("async/compute: `work` must not raise, but it did: " ++ e.show)
