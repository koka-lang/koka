/*---------------------------------------------------------------------------
  Copyright 2012-2016 Microsoft Corporation.
 
  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the file "license.txt" at the root of this distribution.
---------------------------------------------------------------------------*/

// Asynchronous primitives
module std/async

import std/num/int32
import std/time/duration

extern include {
  js file "async-inline.js"
}

type extend exception-info {
  Cancel
}

public fun cancelable( action : (cancel:() -> exn ()) -> <async,exn|e> a ) : <async,exn|e> maybe<a> { 
  val caction = create-cancelable-prim(action)
  catch { Just(caction()) } fun(exn) {
    match(exn.info) {
      Cancel -> {
        check-canceled() // will re-throw if action is not the one that was canceled
        Nothing
      }
      _ -> throw(exn)
    }
  }
}

extern create-cancelable-prim( action : (cancel:() -> exn ()) -> <async,exn|e> a ) : (() -> <async,exn|e> a) {
  js "_create_cancelable"
}

extern check-canceled() : exn () {
  js "_check_canceled"
}

public extern unsafe-register-cancel( cb : a -> <async|e> (), cancel : () -> exn () ) : exn () {
  js "_register_cancel"
}

public fun timeout( secs : duration, action : () -> <async,exn|e> a ) : <async,exn|e> maybe<a> {
  cancelable fun(cancel) {
    val h = set-timeout( fun(_){ cancel(); }, secs.milli-seconds.int32 )
    on-exit({ cancel-timeout(h) }, action)
  }
}

// ----------------------------------------------------------------------------
// Asynchronous operations are supported through the `:async` effect.
// ----------------------------------------------------------------------------

public fun async-fork( action1 : () -> <async|e> a, action2 : () -> <async|e> ()) : <async|e> a {
  val xs = async-all( [{ Just(action1()) }, { action2(); Nothing }] )
  unsafe-noexn{ xs.head.unjust.unjust }
}

// Execute `actions` asynchronously, i.e. interleaved around asynchronous operations.
// When all operations have terminated (or raised an exception) return the list of
// results (or raise the first raised exception).
public fun async-all( actions : list<() -> <async|e> a> ) : <async|e> list<a> {
  async-all-prim( actions.vector ).list;
}


extern async-all-prim( actions : vector<() -> <async|e> a> ) : <async|e> vector<a>  {
  js inline "$asyncAll(actions,_k)"
}

extern async-callback0( action: () -> e (), haserr : bool, nodup : bool ) : (() -> e ()) {
  js "$asyncCallback"
}

public fun unsafe-async-yield-err( action : (cb : a -> <async|e> ()) -> <async|e> () ) : <async|e> a {
  unsafe-async-yield( action, True ) 
}

public extern unsafe-async-yield( action : (cb : a -> <async|e> ()) -> <async|e> (), haserr : bool = False ) : <async|e> a {
  js inline "(#1)( $asyncCallback(_k,#2), $std_core.async_yield)"
}

// Wait (asynchronously) for `secs` seconds. 
// Use `wait(0)` to yield to other asynchronous operations.
public fun wait( secs : double ) : <async,exn> () {
  wait(secs.duration)
}

// Wait (asynchronously) for `secs` seconds duration. 
// Use `wait(0)` to yield to other asynchronous operations.
public fun wait( secs : duration ) : <async,exn> () {
  unsafe-async-yield( fun(cb) { 
    val msecs = secs.milli-seconds.int32
    val h = set-timeout( cb, msecs)
    if (msecs > 100.int32) { 
      unsafe-register-cancel( cb, fun(){ cancel-timeout(h) } )
    }
  })
}

type timeout-handle

extern set-timeout( action : (()) -> e (), ms : int32 ) : exn timeout-handle {
  js "setTimeout"
}

extern cancel-timeout( h : timeout-handle ) : exn () {
  js "clearTimeout"
}


// Execute any asynchronous actions inside `action` synchronously, ie. 
// no other actions are interleaved.
public fun synchronous( action : () -> <async|e> a ) : e a {
  start-synchronous()
  on-exit(end-synchronous){ unsafe-total(action) }
}

extern start-synchronous() : () {
  js "$asyncStartSynchronous"
}

extern end-synchronous() : () {
  js "$asyncEndSynchronous"
}

