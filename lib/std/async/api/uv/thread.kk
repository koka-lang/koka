/*---------------------------------------------------------------------------
  Copyright 2026, Tim Whiting, Microsoft Research, Daan Leijen.

  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the LICENSE file at the root of this distribution.
---------------------------------------------------------------------------*/

// Native (libuv) threading backend for `std/async/thread`.
//
// Offloads a pure computation to a libuv threadpool worker thread and delivers
// the (thread-shared) result back on the owning async loop. See `thread.c` for
// the `mark_shared` discipline that keeps refcounts sound across threads.
module async/api/uv/thread

import async/api/uv/evloop  // transitively supplies the evloop.h declarations to thread.c
import async/internal
import async/async
import async/channel

extern import
  c[host=libc] file "thread.c"

// Primitive: queue `work` on the loop's threadpool, delivering its result to
// `resume` on the loop thread. Returns a dispose function (cancels the work).
extern compute-prim( evloop : any, work : () -> ioc a, resume : (a) -> ioc () ) : ioc error<() -> ioc ()>
  c "kk_uv_queue_work"

// Run `work` on a worker thread and await its result on the current loop.
pub fun await-compute( work : () -> ioc a ) : <async|ioc> a
  match setup/await-error("compute", fn(evloop, resume) compute-prim(evloop, work, resume))
    Ok(x)      -> x
    Error(exn) -> impossible("async/compute: failed to queue work: " ++ exn.show)

// ----------------------------------------------------------------
// xchannel: a THREAD-SAFE channel (native). Any thread may `xemit`; the owner
// loop is woken via uv_async to deliver values into a loop-local Koka channel
// that receivers await. See thread.c for the mutex-queue + mark_shared discipline.

// The C thread-safe channel handle + the owner-side Koka channel receivers await.
pub value struct xchannel<a>( xc : any, chan : channel<a> )

// The cross-thread SENDER half: JUST the C handle (no Koka channel), so a worker
// closure that captures it never holds -- and never races on -- the owner-side
// Koka `channel` (whose mutable state the owner concurrently mutates). This is
// the ONLY part that should cross a thread boundary.
pub value struct xsender<a>( xc : any )

pub fun xchannel/sender( x : xchannel<a> ) : xsender<a>
  Xsender(x.xc)

// create the C channel on `evloop` and hand its boxed handle back via `resume`
extern xchan-create-prim( evloop : any, deliver : (a) -> ioc (), resume : (any) -> ioc () ) : ioc error<() -> ioc ()>
  c "kk_xchan_create"

// enqueue+wake from any thread (the C side marks the value thread-shared).
// Typed `ioc` (not total) to match the web/coop fallback (a plain channel emit),
// so the portable `std/async/thread` signature is host-consistent.
extern xchan-emit-prim( xc : any, value : a ) : ioc ()
  c "kk_xchan_emit"

// close the channel from the OWNER thread (uv_close + free); no cross-thread free
extern xchan-close-prim( xc : any ) : ioc ()
  c "kk_xchan_close"

// True on hosts with real OS worker threads (native); False on web/wasi where
// `spawn-thread` has no real thread. Callers gate threaded topologies on this.
pub fun native-threads() : bool
  True

// Create a thread-safe channel bound to the CURRENT async loop.
pub fun xchannel() : async xchannel<a>
  val ch = channel()
  val xc = match setup/await-error("xchannel", fn(evloop, resume) xchan-create-prim(evloop, fn(v) ch.emit-io(v), resume))
    Ok(p)      -> p
    Error(exn) -> impossible("async/xchannel: could not create: " ++ exn.show)
  Xchannel(xc, ch)

// Emit `value` from ANY thread through the SENDER (thread-safe). Marks the value
// thread-shared and wakes the owner loop; a waiting `xreceive` resumes there. No
// observable Koka effect (the queue+wake is invisible to the caller). Give a
// worker `x.sender` -- never the whole `xchannel` (which holds the Koka channel).
pub fun xsender/xemit( s : xsender<a>, value : a ) : ioc ()
  xchan-emit-prim(s.xc, value)

// Convenience: emit from the owner side (also fine cross-thread, but a worker
// should capture `x.sender`, not `x`, to avoid holding the Koka channel).
pub fun xchannel/xemit( x : xchannel<a>, value : a ) : ioc ()
  xchan-emit-prim(x.xc, value)

// Receive on the OWNER loop (the loop that created the channel).
pub fun xchannel/xreceive( x : xchannel<a> ) : <async,ndet> a
  x.chan.receive

// Receive on the OWNER loop without waiting: `Just` if a value has already
// been delivered to this loop, `Nothing` otherwise.
pub fun xchannel/try-xreceive( x : xchannel<a> ) : <async,ndet> maybe<a>
  x.chan.try-receive

// Close the channel from the OWNER thread. The handle is unrefcounted (not Koka
// memory), so it does not free on drop -- call this to close the uv_async handle
// (letting a finite program's loop drain) once no thread will `xemit` again. An
// app that runs for its whole lifetime need not call it.
pub fun xchannel/close( x : xchannel<a> ) : ioc ()
  xchan-close-prim(x.xc)

// ----------------------------------------------------------------
// spawn-thread: a PERSISTENT worker thread running its OWN async loop.

extern spawn-thread-prim( body : () -> ioc () ) : ioc ()
  c "kk_spawn_thread"

// run a full async handler on THIS thread's own loop -- exactly what `async` does
// (event-loop-init + async/exn handlers + event-loop-run / uv_run). The spawned
// worker thread calls this so its body is a real async context.
fun thread-main( body : () -> <async,ioc> () ) : ioc ()
  async(body)

// Spawn a persistent OS worker thread that runs `body` under its OWN async loop.
// `body` is a real async context: it may `wait`, `spawn`, `xreceive`/`xemit`, etc.
// The thread runs until `body`'s loop drains (no active handles), then exits.
// Cross-thread communication is via `xchannel`/`xsender` (thread-safe channels).
pub fun spawn-thread( body : () -> <async,ioc> () ) : ioc ()
  spawn-thread-prim(fn() thread-main(body))



