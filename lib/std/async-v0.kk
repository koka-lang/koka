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
  js file "async-v0-inline.js"
}

// A type alias for asynchronous operations that can raise exceptions non-deterministically.
// This is common for almost all `:async` operations since `cancelable` and `timeout` can
// cancel operations non-deterministically which raises the `Cancel` exception and cancels
// outstanding asynchronous requests. 
public alias asyncx = <async,exn,ndet>

public effect async {
  fun async-yield() : ()
}

public val async-handle = handler {
  async-yield() -> ()  // exit the program
}

// ----------------------------------------------------------------------------
// Asynchronous timeout and waiting
// ----------------------------------------------------------------------------

// Do `action` but cancel it if it is not done within `secs` seconds. 
// Returns `Nothing` if canceled, and `Just(x)` if successful with a result `x`.
public fun timeout( secs : duration, action : () -> <asyncx|e> a ) : <asyncx|e> maybe<a> {
  cancelable fun(cancel) {
    val id = set-timeout( fun(_){ cancel(); }, secs.milli-seconds.int32 )
    on-exit({ clear-timeout(id); () }, action)
  }
}

// Wait (asynchronously) for `secs` seconds as a `:double`. 
// Use `wait()` to yield to other asynchronous operations.
public fun wait( secs : double ) : <asyncx> () {
  wait(secs.duration)
}

// Wait (asynchronously) for optional `secs` seconds `:duration` (`= 0s`). 
// Use `wait()` to yield generally to other asynchronous operations.
// If the wait time is longer than 200ms the wait can be canceled.
public fun wait( secs : duration = zero ) : <asyncx> () {
  if (secs > 200.milli-seconds) then cancelable-wait(secs) 
  elif (secs <= zero) then yield()
  else yield(secs)
}

// Wait for `secs` seconds duration
public fun cancelable-wait( secs : duration ) : <asyncx> () {
  if (secs <= zero) then yield() else {
    var mid := Nothing
    on-cancel {
      match(mid) {
        Nothing -> ()
        Just(id) -> clear-timeout(id)
      }
    }
    { unsafe-async-yield fun(cb) { 
        mid := Just(set-timeout(cb,secs.milli-seconds.int32))      
      }
    }
  }
}

fun yield( secs : duration = zero ) : <asyncx> () {
  unsafe-async-yield( fun(cb) { set-timeout(cb,max(zero:int32,secs.milli-seconds.int32)); () } )
}

// abstract id for timeout handlers
type timeout-id

extern set-timeout( action : (()) -> e (), ms : int32 ) : exn timeout-id {
  js "setTimeout"
}

extern clear-timeout( h : timeout-id ) : exn () {
  js "clearTimeout"
}

// ----------------------------------------------------------------------------
// Asynchronous cancelation 
// ----------------------------------------------------------------------------

// The `Cancel` exception.
public type extend exception-info {
  // The `Cancel` exception is thrown when an asynchronous operation is canceled.
  con Cancel
}

public fun on-cancel( oncancel : () -> <exn|e> (), action : () -> <exn|e> a ) : <exn|e> a {
  on-exn(fun(exn) {
    match(exn.info) {
      Cancel -> oncancel()
      _      -> ()
    }
  },action)
}

// Execute an asynchronous `action` that gets passed as `cancel` function. If `cancel` is
// called, it will either raise the `Cancel` exception on the next asynchronous operation,
// or, if currently in an asynchronous operation, it will cancel the outstanding operation
// and resume by throwing `Cancel` at that point. If canceled, `cancelable` returns `Nothing`,
// and on success `Just(x)` where `x` is the result of `action`.
public fun cancelable( action : (cancel:() -> exn ()) -> <asyncx|e> a ) : <asyncx|e> maybe<a> { 
  val (unreg,caction) = create-cancelable-prim(action)
  catch { 
    val x = caction()
    unreg()
    Just(x) 
  } 
  fun(exn) {
    unreg()
    match(exn.info) {
      Cancel -> {
        check-canceled() // will re-throw if action is not the one that was canceled
        Nothing
      }
      _ -> throw(exn)
    }
  }
}

extern create-cancelable-prim( action : (cancel:() -> exn ()) -> <asyncx|e> a ) : (() -> exn (), () -> <asyncx|e> a) {
  js "_create_cancelable"
}

extern check-canceled() : exn () {
  js "_check_canceled"
}

// ----------------------------------------------------------------------------
// Asynchronous operations are supported through the `:async` effect.
// ----------------------------------------------------------------------------

public fun async-fork( action1 : () -> <async|e> a, action2 : () -> <async|e> ()) : <async|e> a {
  val xs = interleaved( [{ Just(action1()) }, { action2(); Nothing }].vector )
  unsafe-noexn{ xs[0].unjust }
}

// Execute `actions` asynchronously, i.e. interleaved around asynchronous operations.
// When all operations have terminated (or raised an exception) return the list of
// results (or raise the first raised exception).
public fun interleaved( actions : list<() -> <async|e> a> ) : <async|e> list<a> {
  interleaved( actions.vector ).list;
}

extern interleaved( actions : vector<() -> <async|e> a> ) : <async|e> vector<a>  {
  js inline "_async_all(actions,_k)"
}


public fun unsafe-async-yield-err( action : (cb : a -> <asyncx|e> ()) -> <asyncx|e> () ) : <asyncx|e> a {
  unsafe-async-yield( action, True ) 
}

public extern unsafe-async-yield( action : (cb : a -> <asyncx|e> ()) -> <asyncx|e> (), haserr : bool = False ) : <asyncx|e> a {
  js inline "(#1)( _async_callback(_k,#2), $std_core.async_yield)"
}


// ----------------------------------------------------------------------------
// Synchronous blocks (uninterleaved sequences of async operations)
// ----------------------------------------------------------------------------

// Execute any asynchronous actions inside `action` synchronously, ie. 
// no other actions are interleaved.
public fun synchronous( action : () -> <async|e> a ) : e a {
  start-synchronous()
  on-exit(end-synchronous){ unsafe-total(action) }
}

extern start-synchronous() : () {
  js "_async_start_synchronous"
}

extern end-synchronous() : () {
  js "_async_end_synchronous"
}

