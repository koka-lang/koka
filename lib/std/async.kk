/*---------------------------------------------------------------------------
  Copyright 2012-2016 Microsoft Corporation.
 
  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the file "license.txt" at the root of this distribution.
---------------------------------------------------------------------------*/

/* Asynchronous primitives

This module is based closely on [@Leijen:async].


## References {-}

~ Bibliography { caption:"0" }
~~ BibItem { #Leijen:async; bibitem-label:"[1]"; searchterm:"Leijen+Daan+Structured+Asynchrony+with+Algebraic+Effects" }
Daan Leijen.
_Structured Asynchrony with Algebraic Effects_.
Microsoft Research technical report MSR-TR-2017-21, May 2017.
[pdf](https://www.microsoft.com/en-us/research/wp-content/uploads/2017/05/asynceffects-msr-tr-2017-21.pdf)
~~
~
\/
*/
module std/async

import std/data/dict
import std/data/array
import std/num/int32
import std/time/duration

// A type alias for asynchronous operations that can raise exceptions non-deterministically.
// This is common for almost all `:async` operations since `cancelable` and `timeout` can
// cancel operations non-deterministically which raises the `Cancel` exception and cancels
// outstanding asynchronous requests. 
public alias asyncx = <async,exn,ndet>


abstract struct promise<a>(
  state : ref<global,either<list<(a -> io ())>,a>>
)

// ----------------------------------------------------------------------------
// Promises
// ----------------------------------------------------------------------------

public fun promise() : <async,exn> promise<a> {
  async-io{ Promise(ref(Left([]))) }
}

public fun await( p : promise<a> ) : <async,exn,ndet> a {
  val r = p.state
  await1 fun(cb) {
    match (!r) {
      Left(listeners) -> r := Left(Cons(cb,listeners))
      Right(value)    -> cb(value)
    }
  }
}

public fun resolve( p : promise<a>, value : a ) : <async,exn> () {
  val r = p.state
  await fun(cb) {
    match(!r) {
      Left(listeners) -> {
        r := Right(value)
        listeners.foreach fun(cbx) {
          cbx(value) // set-immediate1( cbx, value )
        }
        cb(Right(()))
      }
      Right -> cb(Left(exception("promise was already resolved.",Error)))
    }
  }
}

// ----------------------------------------------------------------------------
// Asynchronous timeout and waiting
// ----------------------------------------------------------------------------

// Wait (asynchronously) for `secs` seconds as a `:double`. 
// Use `yield()` to yield to other asynchronous operations.
public fun wait( secs : double ) : <async,exn> () {
  wait(secs.duration)
}

// Wait (asynchronously) for optional `secs` seconds `:duration` (`= 0s`). 
// Use `yield()` to yield generally to other asynchronous operations.
// If the wait time is longer than 200ms the wait can be canceled.
public fun wait( secs : duration = zero ) : <async,exn> () {
  if (secs > 200.milli-seconds) then cancelable-wait(secs) 
  elif (secs <= zero) then yield()
  else yield(secs)
}


// Wait for `secs` seconds duration and allow cancelation.
public fun cancelable-wait( secs : duration ) : <async,exn> () {
  if (secs <= zero) then yield() else {
    var vtid := Nothing 
    match(fork-after(secs)) {
      Left(exn) -> {
        match(vtid) {
          Nothing   -> ()
          Just(tid) -> clear-timeout(tid)
        }
        throw(exn)
      }
      Right(mbtid) -> {
        vtid := mbtid
        if (mbtid.just?) then exit() else ()          
      }
    }
  }
}


fun xtimeout( secs : duration, action : () -> <asyncx|e> a ) : <asyncx|e> maybe<a> {
  catch {
    val x = cancelable {
      match(fork-after(secs).untry) {
        Nothing -> {
          cancel()
          exit()
        }
        Just(tid) -> finally(action) { clear-timeout(tid) } // ouch: clear-timeout prevents resuming twice
      }
    }
    Just(x)
  }
  fun(exn) {
    match(exn.info) {
      Cancel -> Nothing
      _      -> throw(exn)
    }
  }
}

fun fork-after( secs : duration ) : <async,exn> either<exception,maybe<timeout-id>> {
  awaitx1( fun(resume) {
    val tid = set-timeout1( resume, max(0.int32,secs.milli-seconds.int32), Nothing )
    resume(Just(tid))
  }, 2 )
} 

// Execute `action` but if it is not finished within `secs` seconds duration
// `cancel` it.
public fun timeout( secs : duration, action )  {
  firstof 
    { wait(secs); Nothing }
    { Just(action()) }
}

// Execute `a` and `b` interleaved. As soon as one of them finishes,
// `cancel` the other one and return the result of the first.
public fun firstof( a, b ) {
  cancelable {
    val (ra,rb) = interleavedx { val x = a(); cancel(); x }
                               { val x = b(); cancel(); x }
    match(ra) {
      Left(exn) | exn.info.cancel? -> rb.untry 
      _ -> ra.untry
    }
  }                              
} 


// ----------------------------------------------------------------------------
// Yield
// ----------------------------------------------------------------------------

// Wait for optional `secs` duration (=`0`). Use `yield()` to generally yield
// to other asynchronous operations. Note that `yield` is not cancelable,
// use `wait` instead if waiting for more than short durations.
public fun yield( secs : duration = zero ) : <async,exn> () {
  await0 fun(cb){ 
    set-timeout( cb, max(zero:int32,secs.milli-seconds.int32) )
    ()
  }
}

// abstract wid for timeout handlers
type timeout-id

extern set-timeout1( cb : a -> io (), ms : int32, arg : a ) : io timeout-id {
  js "setTimeout"
}

extern set-timeout( cb : () -> io (), ms : int32 ) : io timeout-id {
  js "setTimeout"
}

extern clear-timeout( tid : timeout-id ) : exn () {
  js "clearTimeout"
}


// ----------------------------------------------------------------------------
// Asynchronous cancelation 
// ----------------------------------------------------------------------------



// ----------------------------------------------------------------------------
// Synchronous blocks (uninterleaved sequences of async operations)
// ----------------------------------------------------------------------------


// ----------------------------------------------------------------------------
// Asynchronous operations are supported through the `:async` effect.
// The basic effect is in core since it usually requires some support from the host system.
// ----------------------------------------------------------------------------

// Interleave a list of actions.
public fun interleaved( xs : list<() -> <asyncx|e> a> ) : <asyncx|e> list<a> {
  interleaved(xs.vector).list
}

fun interleaved( xs : vector<() -> <asyncx|e> a> ) : <asyncx|e> vector<a> {
  async-handle-interleaved {
    val n = xs.length
    //if (n==0) return vector()
    val res    = array(n,Left(exception("Interleaved operation was not completed",Internal("std/async/interleaved"))))
    var count := n;
    val i = primforkn(n)
    res[i] := try( inject-st(xs[i]) )
    count := count - 1
    if (count>0) { exit() }
    res.vector.map(untry)
  }
}

// Interleave two asynchronous actions.
public fun interleaved( a : () -> <asyncx|e> a, b : () -> <asyncx|e> b ) : <asyncx|e> (a,b) {
  val (ra,rb) = interleavedx(a,b)
  (ra.untry,rb.untry)
}

// Interleave two asynchronous actions and return either their result or their exceptional value.
public fun interleavedx( a : () -> <asyncx|e> a, b : () -> <asyncx|e> b ) : <asyncx|e> (either<exception,a>,either<exception,b>) {
  async-handle-interleaved {
    var ares := Nothing
    var bres := Nothing
    val forked = primfork()
    if (forked) then {
      val br = try( injectst(b) )
      match(ares) {
        Nothing  -> { bres := Just(br); exit() }
        Just(ar) -> (ar,br)
      }
    }
    else {
      val ar = try( injectst(a) )
      match(bres) {
        Nothing  -> { ares := Just(ar); exit() }
        Just(br) -> (ar,br)
      }
    }
  }
}

extern inline injectst(action : () -> <asyncx|e> a) : total (() -> <asyncx,st<h>|e> a) {
  js inline "(#1)"
}

// ----------------------------------------------------------------------------
// Forking
// ----------------------------------------------------------------------------

public fun fork( action )  {  
  val forked = primfork()
  if (forked) then {
    catch {
      action()
      exit()
    }
    fun(exn) { 
      trace("unhandled fork exception: " + exn.show)
      exit() 
    }
  }
  else ()
}

fun primfork( ) {
  val i = primforkn(2)
  (i==0)
} 

public fun primforkn( n : int ) : <asyncx> int {
  if (n<=1) then 0 else {
    await1( fun(cb) {
      for(1, n - 1) fun(i) {
        set-immediate1( cb, i )
      }
      cb(0)
    }, n)
  }
}

extern set-immediate( action : () -> e () ) : exn () {
  js inline "(typeof setImmediate !== 'undefined' ? setImmediate(#1) : setTimeout(#1,0))"
}

extern set-immediate1( action : (a) -> e (), arg : a ) : exn () {
  js inline "(typeof setImmediate !== 'undefined' ? setImmediate(#1,#2) : setTimeout(#1,0,#2))"
}


// ----------------------------------------------------------------------------
// awaitx wrappers
// ----------------------------------------------------------------------------

public fun await-exn0( setup : (cb : (null<exception>) -> io () ) -> io () ) : <async,exn> () {
  await( fun(resume) {
    setup( fun(nexn) { resume(nexn.unnull(())) }) 
  } )
}

public fun await-exn1( setup : (cb : (null<exception>,a) -> io () ) -> io () ) : <async,exn> a {
  await( fun(resume) {
    setup( fun(nexn,x) { resume(nexn.unnull(x)) })
  } )
}

fun unnull( nexn : null<exception>, x : a  ) : either<exception,a> {
  match(nexn.maybe) {
    Nothing -> Right(x)
    Just(exn) -> Left(exn)
  }
}

public fun await0( setup : (cb : () -> io () ) -> io () ) : <async,exn> () {
  await( fun(resume) { setup( fun() { resume(Right(())) } ) } )
}

public fun await1( setup : (cb : (a) -> io () ) -> io (), rcount : int = 1 ) : <async,exn> a {
  awaitx( fun(resume) { setup( fun(x) { resume(Right(x)) } ) }, rcount ).untry
}

public fun await( setup : (cb : either<exception,a> -> io () ) -> io () ) : <async,exn> a {
  awaitx(setup, 1 ).untry
}

public fun awaitx0( setup : (cb : () -> io () ) -> io (), rcount : int = 1 ) : <async> maybe<exception> {
  val res = awaitx( fun(resume){ setup( fun(){ resume(Right(())) } ) }, rcount ) 
  match(res) {
    Right -> Nothing
    Left(exn) -> Just(exn)
  }
}

public fun awaitx1( setup : (cb : a -> io () ) -> io (), rcount : int = 1 ) : <async> either<exception,a> {
  awaitx( fun(resume){ setup( fun(x){ resume(Right(x)) } ) }, rcount )
}

public fun awaitx( setup : (cb : (either<exception,a> -> io () ) -> io ()), resume-count : int ) : async either<exception,a> {
  var count := resume-count
  await-done( fun(cb) {
    setup( fun(x) {
      count := count - 1
      cb(x, count <= 0)
    })
  }).fst
}


// ----------------------------------------------------------------------------
// Callback widentifiers
// ----------------------------------------------------------------------------

  
private struct wid( :int )

val wid-exit = Wid(-1)

fun (==)(wid1 : wid, wid2 : wid ) : bool {
  match(wid1) {
    Wid(i1) -> match(wid2) {
      Wid(i2) -> i1==i2
    }
  }
}

fun (!=)(wid1 : wid, wid2 : wid ) : bool {
  !(wid1 == wid2)
}


// ----------------------------------------------------------------------------
// Async effect
// ----------------------------------------------------------------------------

alias await-result<a> = (either<exception,a>,bool,wid)

// Asynchronous operations have the `:async` effect.
abstract effect async {
  fun await-id() : wid  
  fun await( setup : (cb : await-result<a> -> io ()) -> io (), wid : wid ) : await-result<a>
  fun cancel( awaits : maybe<list<wid>> ) : ()
}

public fun await-done( setup : ((either<exception,a>,bool) -> io ()) -> io () ) : async (either<exception,a>,bool) {
  val wid = await-id()
  val (x,done?,_) = await( fun(cb){
                      setup(fun(x,done?){ cb((x,done? || x.left?, wid)) })
                    }, wid)
  (x,done?)
}

// The `cancel` operations cancels any outstanding asynchronous operation under the innermost
// `cancelable` handler by returning the `Cancel` exception. The `cancel` operation itself returns normally
// without raising a `Cancel` exception.
public fun cancel() : async (){
  cancel(Nothing)
}

// Exits the current asynchronous strand of execution without raising an exception.
public fun exit() : <async,exn> a {
  await( fun(cb) { }, wid-exit ).fst.untry
  //error("exit should never resume") 
  //()
}

fun async-io( action : () -> io a ) : <async,exn> a {
  val r = await-done( fun(cb) { cb( try(action), True ) } )
  r.fst.untry
}

// ----------------------------------------------------------------------------
// Async handlers: cancelable
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

val cancel-exn = exception("Cancel", Cancel)

// Execute `action` in a cancelable scope. If `cancel` is called within `action`,
// any outstanding asynchronous operations started in the cancelable scope are canceled but
// not outside it.
public fun cancelable( action : () -> <async|e> a ) : <async|e> a 
{
  var awaits := []

  handle(inject-st(action)) {
    await(setup,wid) -> {
      if (wid != wid-exit) awaits := Cons(wid,awaits)
      val res = await(setup,wid)
      if (res.snd) awaits := awaits.remove( fun(i) { i == res.thd } )
      resume(res)
    }    
    cancel(wids) -> {
      resume(cancel(if (wids.nothing?) then Just(awaits) else wids))
    }
    await-id() -> resume(await-id())
  }
}

// ----------------------------------------------------------------------------
// Async handlers: interleaved
// ----------------------------------------------------------------------------

public fun async-handle-interleaved( action : () -> <async,exn|e> a ) : <async,exn|e> a  
{
  var latest : some<e,a> await-result<() -> <async|e> a> -> io () := fun(_) { () }
  
  handle(inject-st(action)) {
    await(setup,wid) -> {
      val r : some<e,a> await-result<() -> <async|e> a>
        = await( fun(cb) {
            latest := cb
            setup( fun(x) {
              latest( (Right({resume(x)}), x.snd, x.thd) ) 
            })
          },wid)
      match(r.fst) {
        Left(exn) -> resume((Left(exn), r.snd, r.thd)) 
        Right(f)  -> f()
      }
    }
    cancel(wids) -> resume(cancel(wids))
    await-id()   -> resume(await-id())
  }
} 

// ----------------------------------------------------------------------------
// Async handle
// ----------------------------------------------------------------------------

// The outer `:async` effect handler
public fun async-handle(action : () -> <async,io> () ) : io () 
{
  var awaits := []
  
  fun outer-callback( resume, wid : wid) {
    val oresume = outer-resume(resume)
    fun cb(res) {
      val (_,done?,wid' : wid) = res
      if (awaits.contains(wid')) {
        if (done?) then awaits := awaits.remove(wid')
        oresume(res)
      }
    }
    if (wid!=wid-exit) awaits := Cons((wid,cb),awaits)
    cb
  }

  fun cancel-await( wid : wid ) {
    match( awaits.lookup(wid) ) {
      Nothing  -> ()
      Just(cb) -> {
        cb((Left(cancel-exn),True,wid))
      }
    }
  }

  handle(action) {   
    await( setup, wid ) -> setup( outer-callback(resume,wid) )
    cancel( wids )      -> resume(wids.default(awaits.map(fst)).foreach( cancel-await ))
    await-id()          -> resume(Wid(unique()))
  }
}

// Convenience functions for wid maps
fun remove( xs : list<(wid,a)>, wid : wid ) : list<(wid,a)> {
  xs.remove( fun(x:(wid,_)) { x.fst == wid })
}

fun lookup( xs : list<(wid,a)>, wid : wid ) : maybe<a> {
  xs.lookup( fun(x:wid) { x == wid })
}

fun contains( xs : list<(wid,a)>, wid : wid ) : bool {
  xs.lookup(wid).bool
}