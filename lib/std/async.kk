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

extern create-cancelable-prim( action : (cancel:() -> exn ()) -> <async,exn|e> a ) : (() -> exn (), () -> <async,exn|e> a) {
  js "_create_cancelable"
}

extern check-canceled() : exn () {
  js "_check_canceled"
}

public fun timeout( secs : duration, action : () -> <async,exn|e> a ) : <async,exn|e> maybe<a> {
  cancelable fun(cancel) {
    val id = set-timeout( fun(_){ cancel(); }, secs.milli-seconds.int32 )
    on-exit({ clear-timeout(id); () }, action)
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

public fun on-cancel( oncancel : () -> <exn|e> (), action : () -> <exn|e> a ) : <exn|e> a {
  on-exn(fun(exn) {
    match(exn.info) {
      Cancel -> oncancel()
      _      -> ()
    }
  },action)
}

// Wait (asynchronously) for `secs` seconds duration. 
// Use `wait(0)` to yield to other asynchronous operations.
public fun wait( secs : duration ) : <async,exn> () {
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

type timeout-id

extern set-timeout( action : (()) -> e (), ms : int32 ) : exn timeout-id {
  js "setTimeout"
}

extern clear-timeout( h : timeout-id ) : exn () {
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

