/*---------------------------------------------------------------------------
  Copyright 2012-2016 Microsoft Corporation.
 
  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the file "license.txt" at the root of this distribution.
---------------------------------------------------------------------------*/

// Asynchronous primitives
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


// ----------------------------------------------------------------------------
// Asynchronous timeout and waiting
// ----------------------------------------------------------------------------


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
    var vtid := Nothing 
    match(fork-after(secs)) {
      Left(exn) -> {
        match(vtid) {
          Nothing   -> ()
          Just(tid) -> clear-timeout(tid)
        }
        throw(exn)
      }
      Right(mbtid) -> match(mbtid) {
        Nothing -> ()
        Just(tid) -> {
          vtid := Just(tid)
          exit(Nothing)
        }
      }
    }
  }
}


public fun timeout( secs : duration, action : () -> <asyncx|e> a ) : <asyncx|e> maybe<a> {
  cancelable {
    match(fork-after(secs).untry) {
      Nothing -> {
        cancel()
        throw(cancel-exn)
      }
      Just(tid) -> finally(action) { clear-timeout(tid) } 
    }
  }
}

fun fork-after( secs : duration ) : <async,exn> either<exception,maybe<timeout-id>> {
  await-err1( fun(resume) {
    val tid = set-timeout1( resume, max(0.int32,secs.milli-seconds.int32), Nothing )
    resume(Just(tid))
  }, 2 )
} 



public fun yield( secs : duration = zero ) : <async,exn> () {
  await0 fun(cb){ 
    set-timeout( cb, max(zero:int32,secs.milli-seconds.int32) )
    ()
  }
}

// abstract id for timeout handlers
type timeout-id

extern set-timeout1( cb : a -> io (), ms : int32, arg : a ) : io timeout-id {
  js "setTimeout"
}

extern set-timeout( action : () -> e (), ms : int32 ) : exn timeout-id {
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

val cancel-exn = exception("Cancel", Cancel)

public fun cancelable( action ) { //: () -> <asyncx|e> a ) : <asyncx|e> maybe<a> { 
  var canceled := False
  val idmap = idmap()
 
  catch { 
    handle(inject-st(action)) {
      return x -> Just(x)
      awaitx(setup,n) -> {
        val h = idmap.register(resume,n)
        val x = awaitx( setup, n )
        h.unregister
        if (canceled) {
          exit(Nothing) // callback came back but we canceled already
        }
        else resume(x)
      }
      exit(mbexn) -> exit(mbexn)
      cancel()    -> {
        canceled := True
        val elems = idmap.elements
        idmap.clear()
        if (elems.length > 0) {
          val i = primforkn(elems.length)
          try{ elems[i](Left(cancel-exn)) }
          if (i > 0) then exit(Nothing)
        }
        resume(True)
      }
      outer-async() -> resume( outer-async() )
    }
  }
  fun(exn) {
    match(exn.info) {
      Cancel -> Nothing
      _      -> throw(exn)
    }
  }
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




// ----------------------------------------------------------------------------
// Asynchronous operations are supported through the `:async` effect.
// The basic effect is in core since it usually requires some support from the host system.
// ----------------------------------------------------------------------------




public fun interleaved( xs : list<() -> <asyncx|e> a> ) : <asyncx|e> list<a> {
  interleaved(xs.vector).list
}


public fun interleaved( xs : vector<() -> <asyncx|e> a> ) : <asyncx|e> vector<a> {
  async-handle-interleaved {
    val n = xs.length
    if (n==0) return vector()
    val res    = array(n,Left(exception("Interleaved operation was not completed",Internal("std/async/interleaved"))))
    var count := n;
    val i = primforkn(n)
    res[i] := try( inject-st(xs[i]) )
    count := count - 1
    if (count>0) {
      exit(Nothing)
    }
    else {
      res.vector.map(untry)
    }
  }
}

public fun interleaved( a : () -> <asyncx|e> a, b : () -> <asyncx|e> b ) : <asyncx|e> (a,b) {
  async-handle-interleaved {
    var ares := Nothing
    var bres := Nothing
    val forked = primfork()
    if (forked) then {
      val br = try( inject-st(b) )
      match(ares) {
        Nothing  -> { bres := Just(br); exit(Nothing) }
        Just(ar) -> (ar.untry,br.untry)
      }
    }
    else {
      val ar = try( inject-st(a) )
      match(bres) {
        Nothing  -> { ares := Just(ar); exit(Nothing) }
        Just(br) -> (ar.untry,br.untry)
      }
    }
  }
}

// ----------------------------------------------------------------------------
// Interleaved async handler
// ----------------------------------------------------------------------------


public fun async-handle-interleaved(action : () -> <async,ndet|e> a ) : <async,ndet|e> a 
{
  fun capture() { capture-handler-context(outer-async()) }
  var ctx := capture()
  handle(inject-st(action)) {
    awaitx(setup, n ) -> {
      ctx := capture()
      val x = awaitx( setup, n )
      resume-in-handler-context( ctx, resume, x )
    }
    exit(mbexn) -> {
      ctx := capture()
      exit(mbexn)
    }
    cancel() -> {
      ctx := capture()
      resume( cancel() )
    }
    outer-async() -> resume( current-handler-top().inc )
  }
}


// ----------------------------------------------------------------------------
// Forking
// ----------------------------------------------------------------------------

public fun fork( action ) {  
  val forked = primfork()
  if (forked) then {
    catch {
      action()
      exit(Nothing)
    }
    fun(exn) { exit(Just(exn)) }
  }
  else ()
}

fun primfork( ) {
  await1( fun(resume) {
    set-immediate1( resume, True )
    resume(False)
  }, 2 )
} 

fun primforkn( n : int ) {
  if (n<=1) then 0 else {
    await1( fun(resume) {
      for(1, n - 1) fun(i) {
        set-immediate1( resume, i )
      }
      resume( 0 )
    }, n )
  }
}

extern set-immediate( action : () -> e () ) : exn () {
  js inline "(typeof setImmediate !== 'undefined' ? setImmediate(#1) : setTimeout(#1,0))"
}

extern set-immediate1( action : (a) -> e (), arg : a ) : exn () {
  js inline "(typeof setImmediate !== 'undefined' ? setImmediate(#1,#2) : setTimeout(#1,0,#2))"
}



// ----------------------------------------------------------------------------
// Awaitx wrappers
// ----------------------------------------------------------------------------

public fun await-exn0( setup : (resume-cb : (null<exception>) -> io () ) -> io (), resumes : int = 1 ) : <async,exn> () {
  await( fun(resume) {
    setup( fun(nexn) { 
      resume( match(nexn.maybe) {
        Nothing   -> Right(())
        Just(exn) -> Left(exn)
      })
    })
  }, resumes )
}

public fun await-exn1( setup : (resume-cb : (null<exception>,a) -> io () ) -> io (), resumes : int = 1 ) : <async,exn> a {
  await( fun(resume) {
    setup( fun(nexn,x) { 
      resume( match(nexn.maybe) {
        Nothing   -> Right(x)
        Just(exn) -> Left(exn)
      })
    })
  }, resumes )
}


public fun await0( setup : (resume-cb : () -> io () ) -> io (), resumes : int = 1 ) : <async,exn> () {
  await fun(resume) { setup( fun() { resume(Right(())) } ), resumes }
}

public fun await1( setup : (resume-cb : (a) -> io () ) -> io (), resumes : int = 1 ) : <async,exn> a {
  await( fun(resume) { setup( fun(x) { resume(Right(x)) } ) }, resumes )
}


public fun await-err0( setup : (resume-cb : () -> io () ) -> io (), resumes : int = 1 ) : <async> maybe<exception> {
  val res = awaitx( fun(resume){ setup( fun(){ resume(Right(())) } ) }, resumes ) 
  match(res) {
    Right -> Nothing
    Left(exn) -> Just(exn)
  }
}

public fun await-err1( setup : (resume-cb : a -> io () ) -> io (), resumes : int = 1 ) : <async> either<exception,a> {
  awaitx( fun(resume){ setup( fun(x){ resume(Right(x)) } ) }, resumes )
}

public fun await( setup : (resume-cb : either<exception,a> -> io () ) -> io (), resumes : int = 1 ) : <async,exn> a {
  match(awaitx(setup, resumes )) {
    Left(exn) -> throw(exn)
    Right(x)  -> x
  }
}


// ----------------------------------------------------------------------------
// Async effect
// ----------------------------------------------------------------------------


// Asynchronous operations have the `:async` effect.
public effect async {
  fun awaitx( setup : (resume-cb : either<exception,a> -> io () ) -> io (), resumes : int ) : either<exception,a>
  fun exit( mbexn : maybe<exception> ) : a
  fun cancel() : bool
  fun outer-async() : int32
}




// ----------------------------------------------------------------------------
// Async handle
// ----------------------------------------------------------------------------

public val async-handle  = handler {
  awaitx( setup, n ) -> setup( outer-resume(resume) )  
  cancel()           -> resume(False)  
  outer-async()      -> resume( current-handler-top().inc )
  exit( mbexn ) -> {
    match(mbexn) {
      Nothing   -> ()
      Just(exn) -> throw(exn)
    }
  }
}




// ----------------------------------------------------------------------------
//  Id maps 
// ----------------------------------------------------------------------------

abstract struct id<h,a>( 
  idmap : idmap<h,a>, 
  key : string
)

abstract type idmap<h,a> {
  Idmap( top : ref<h,int>, values : mdict<h,(int,a)> )
}

public fun idmap() : alloc<h> idmap<h,a> {
  Idmap( ref(0), mdict() )
}

public fun register( idmap : idmap<h,a>, value : a, count : int = 1 ) : st<h> id<h,a> {
  val t = idmap.top
  val idx = !t
  t := idx + 1
  val key = idx.show
  idmap.values[key] := (count,value)
  Id(idmap,key)
}

public fun unregister( id : id<h,a> ) : st<h> () {
  match(id.idmap.values[id.key]) {
    Nothing -> ()
    Just((c,x)) -> {
      if (c > 0) {
        id.idmap.values[id.key] := (c.dec, x)
      }
      elif (c==0) {          
        id.idmap.values.remove(id.key)
        if (id.idmap.values.count==0) {
          val t = id.idmap.top
          t := 0
        } 
      }
      // if negative, never unregister...
    }
  }
}

public fun elements( idmap : idmap<h,a> ) : st<h> vector<a> {
  idmap.values.vector.map(fun(x) { x.snd.snd })
}

public fun clear( idmap : idmap<h,a> ) : st<h> () {
  idmap.values.clear()
  val t = idmap.top
  t := 0
}