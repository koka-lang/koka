/*---------------------------------------------------------------------------
  Copyright 2026, Tim Whiting, Microsoft Research, Daan Leijen.

  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the LICENSE file at the root of this distribution.
---------------------------------------------------------------------------*/

// Cooperative fallback threading backend for `std/async/thread`.
//
// Web and wasi hosts have no OS threads, so `compute` degrades to a cooperative
// strand: yield to the loop once (giving other strands a chance to run), then
// run `work` inline on the single thread. Same API as the native backend, so
// applications stay portable.
module async/api/coop/thread

import async/async
import async/channel

// Run `work` cooperatively: yield to other strands, then compute inline.
pub fun await-compute( work : () -> ioc a ) : <async|ioc> a
  yield()
  work()

// ----------------------------------------------------------------
// thread-safe channel + spawn-thread: on a single-threaded cooperative host
// there ARE no other threads, so an `xchannel` is just a plain channel and
// `native-threads()` is False (callers gate real threading on it). The API
// matches the native backend so `std/async/thread` stays host-consistent.

pub value struct xchannel<a>( chan : channel<a> )
pub value struct xsender<a>( chan : channel<a> )

pub fun native-threads() : bool
  False

pub fun xchannel() : async xchannel<a>
  Xchannel(channel())

pub fun xchannel/sender( x : xchannel<a> ) : xsender<a>
  Xsender(x.chan)

pub fun xsender/xemit( s : xsender<a>, value : a ) : ioc ()
  s.chan.emit-io(value)

pub fun xchannel/xemit( x : xchannel<a>, value : a ) : ioc ()
  x.chan.emit-io(value)

pub fun xchannel/xreceive( x : xchannel<a> ) : <async,ndet> a
  x.chan.receive

// No native handle to close on a cooperative host; a no-op for API parity.
pub fun xchannel/close( x : xchannel<a> ) : ioc ()
  ()

// No OS threads here: gated out by `native-threads()`. If a caller ignores the
// gate and calls this, it is a no-op (the body never runs) -- deliberately inert
// rather than throwing, since it is never reached on a correctly-gated path.
pub fun spawn-thread( body : () -> <async,ioc> () ) : ioc ()
  println("std/async: spawn-thread has no effect on this host (no OS threads)")
