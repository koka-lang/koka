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
public fun wait( secs : double ) : <async,exn> () {
  wait(secs.duration)
}

// Wait (asynchronously) for optional `secs` seconds `:duration` (`= 0s`). 
// Use `wait()` to yield generally to other asynchronous operations.
// If the wait time is longer than 200ms the wait can be canceled.
public fun wait( secs : duration = zero ) : <async,exn> () {
  if (secs > 200.milli-seconds) then cancelable-wait(secs) 
  elif (secs <= zero) then yield()
  else yield(secs)
}


// Wait for `secs` seconds duration
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
      Right(mbtid) -> match(mbtid) {
        Nothing -> ()
        Just(tid) -> {
          vtid := Just(tid)
          exit()
        }
      }
    }
  }
}


public fun timeout( secs : duration, action : () -> <asyncx|e> a ) : <asyncx|e> a {
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
  awaitx1( fun(resume) {
    val tid = set-timeout1( resume, max(0.int32,secs.milli-seconds.int32), Nothing )
    resume(Just(tid))
  }, 2 )
} 


// ----------------------------------------------------------------------------
// Yield
// ----------------------------------------------------------------------------


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

public fun interleaved( xs : list<() -> <asyncx|e> a> ) : <asyncx|e> list<a> {
  interleaved(xs.vector).list
}


public fun interleaved( xs : vector<() -> <asyncx|e> a> ) : <asyncx|e> vector<a> {
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


public fun interleaved( a : () -> <asyncx|e> a, b : () -> <asyncx|e> b ) : <asyncx|e> (a,b) {
  async-handle-interleaved {
    var ares := Nothing
    var bres := Nothing
    val forked = primfork()
    if (forked) then {
      val br = try( inject-st(b) )
      match(ares) {
        Nothing  -> { bres := Just(br); exit() }
        Just(ar) -> (ar.untry,br.untry)
      }
    }
    else {
      val ar = try( inject-st(a) )
      match(bres) {
        Nothing  -> { ares := Just(ar); exit() }
        Just(br) -> (ar.untry,br.untry)
      }
    }
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

public fun primforkn( n : int ) {
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

public fun await-exn0( setup : (cb : (null<exception>) -> io () ) -> io (), rcount : int = 1 ) : <async,exn> () {
  await( fun(resume) {
    setup( fun(nexn) { resume(nexn.unnull(())) }) 
  }, rcount )
}

public fun await-exn1( setup : (cb : (null<exception>,a) -> io () ) -> io (), rcount : int = 1 ) : <async,exn> a {
  await( fun(resume) {
    setup( fun(nexn,x) { resume(nexn.unnull(x)) })
  }, rcount )
}

fun unnull( nexn : null<exception>, x : a  ) : either<exception,a> {
  match(nexn.maybe) {
    Nothing -> Right(x)
    Just(exn) -> Left(exn)
  }
}

public fun await0( setup : (cb : () -> io () ) -> io (), rcount : int = 1 ) : <async,exn> () {
  await( fun(resume) { setup( fun() { resume(Right(())) } ) }, rcount )
}

public fun await1( setup : (cb : (a) -> io () ) -> io (), rcount : int = 1 ) : <async,exn> a {
  await( fun(resume) { setup( fun(x) { resume(Right(x)) } ) }, rcount )
}

public fun await( setup : (cb : either<exception,a> -> io () ) -> io (), rcount : int = 1 ) : <async,exn> a {
  awaitx(setup, rcount ).untry
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
// Async effect
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
  
struct id( :int )

val id-exit = Id(-1)

fun show(id : id ) : string {
  match(id) {
    Id(i) -> i.show
  }
}

fun (==)(id1 : id, id2 : id ) : bool {
  match(id1) {
    Id(i1) -> match(id2) {
      Id(i2) -> i1==i2
    }
  }
}
fun (!=)(id1 : id, id2 : id ) : bool {
  !(id1 == id2)
}

alias ids = list<id>


alias callback<a> = (either<exception,a>,bool,id) -> io ()

// Asynchronous operations have the `:async` effect.
public effect async {
  fun await-id( setup : (cb : callback<a>) -> io (), id : id ) : (either<exception,a>,bool,id)
  fun cancel-ids( mids : maybe<ids> ) : ()
  fun fresh-id() : id  
}

public fun await-done( setup : ((either<exception,a>,bool) -> io ()) -> io () ) : async (either<exception,a>,bool) {
  val id = fresh-id()
  val (x,done?,_) = await-id( fun(cb){
                      setup(fun(x,done?){ cb(x,done?,id) })
                    }, id)
  (x,done?)
}

public fun cancel() {
  cancel-ids(Nothing)
}

public fun exit() : <async,exn> a {
  await-id( fun(cb) { }, id-exit ) 
  error("exit should never resume") 
  //()
}


// ----------------------------------------------------------------------------
// Async handle
// ----------------------------------------------------------------------------


public fun cancelable( action : () -> <async|e> a ) : <async|e> a 
{
  var cids := []

  handle(inject-st(action)) {
    await-id(setup,id) -> {
      if (id != id-exit) cids := Cons(id,cids)
      val x = await-id(setup,id)
      if (x.snd) cids := cids.remove( fun(i) { i == x.thd })
      resume(x)
    }    
    cancel-ids(mids) -> {
      resume(cancel-ids(if (mids.nothing?) then Just(cids) else mids))
    }
    fresh-id() -> resume(fresh-id())
  }
}

public fun async-handle-interleaved( action : () -> <async,exn|e> a ) : <async,exn|e> a  
{
  var latest : some<e,a> (either<exception,() -> <async|e> a>, bool,id) -> io () := fun(_,_,_) { () }
  
  handle(inject-st(action)) {
    await-id(setup,id) -> {
      val r : some<e,a> (either<exception,() -> <async|e> a>,bool,id) 
        = await-id( fun(cb) {
            latest := cb
            setup( fun(x : either<exception,_a>,  done?:bool, id' : id) {
              latest( Right( { resume((x,done?,id')) }), done?, id' )
            })
          },id)
      (r.fst.untry)()
    }
    cancel-ids(ids) -> resume(cancel-ids(ids))
    fresh-id()      -> resume(fresh-id())
  }
} 

public fun async-handle(action : () -> <async,io> () ) : io () 
{
  val waiting = idmap()
  
  fun outer-callback( resume, id : id) {
    fun cb(x,done?,id' : id) {
      if (waiting.contains(id')) {
        if (done?) then waiting.remove(id')
        outer-resume(resume)((x,done?,id'))
      }
    }
    if (id!=id-exit) waiting[id] := cb
    cb
  }

  fun cancel-id( id ) {
    match( waiting[id] ) {
      Nothing  -> ()
      Just(cb) -> {
        waiting.remove(id)
        cb(Left(cancel-exn),True,id)
      }
    }
  }

  handle(action) {   
    await-id( setup, id ) -> setup( outer-callback(resume,id) )
    cancel-ids( mids )    -> resume(mids.default(waiting.ids).foreach( cancel-id ))
    fresh-id()            -> resume(Id(unique()))
  }
}


//

struct idmap<h,a>( m : mdict<h, callback<a>> ) 

fun idmap() {
  Idmap(mdict())
}

fun []( idmap : idmap<h,a>, id : id, cb : callback<a>  ) : st<h> () {
  idmap.m[id.show] := cb
}

fun contains( idmap : idmap<h,a>, id : id ) : read<h> bool {
  idmap.m.contains-key(id.show)
}

fun remove( idmap : idmap<h,a>, id : id ) : st<h> () {
  idmap.m.remove(id.show)
}

fun [](idmap : idmap<h,a>, id : id ) : st<h> maybe<callback<a>> {
  (idmap.m)[id.show]
}

fun keys(idmap : idmap<h,a> ) : st<h> list<string> {
  idmap.m.keys.list
}

fun ids( idmap: idmap<h,a> ) : st<h> ids {
  idmap.keys.map( fun(s) { Id(parse-int(s).default(-1)) })
}