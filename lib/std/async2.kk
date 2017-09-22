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
module std/async2

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
/*
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
*/

// ----------------------------------------------------------------------------
// Channels
// ----------------------------------------------------------------------------


abstract struct channel<a>(
	state : ref<global,either<list<(a -> io ())>,list<a>>>
)

public fun channel() : <async,exn> channel<a> {
  async-io{ Channel(ref(Left([]))) }
}

public fun await( ch : channel<a> ) : <async,exn,ndet> a {
	fun setup( cb : _ -> io () ) : io () {
		val r = ch.state
		match (!r) {
			Left(listeners) -> r := Left(listeners + [cb])
			Right([]) -> r := Left([cb])
			Right(Cons(v,vs))-> {  // this case should not happen due to `peek?`
				r := Right(vs)
				cb(v)
			}
		}
	}
	match(ch.peek?) {
		Just(v) -> v
		Nothing -> {
			await1(setup)
		}
	}
}

public fun peek?( ch : channel<a> ) : asyncx maybe<a> {
	async-io {
		val r = ch.state
		match (!r) {
			Left	-> Nothing
			Right(values)	-> values.head
		}
	}
}

fun emit-io( ch : channel<a>, value : a ) : io () {
	val r = ch.state
	match(!r) {
		Right(values) -> r := Right(values + [value])
		Left([]) -> r := Right([value])
		Left(Cons(l,ls)) -> {
            r := Left(ls)
            l(value)
        }
	}
}

public fun emit( ch : channel<a>, value : a ) : <async,exn> () {
	async-io {
		emit-io(ch,value)
	}
}


// ----------------------------------------------------------------------------
// Asynchronous timeout and waiting
// ----------------------------------------------------------------------------
/*
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
*/

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

public fun interleaved( action1 : () -> <asyncx|e> a, action2 : () -> <asyncx|e> b ) : <asyncx|e> (a,b) {
	val (ra,rb) = interleavedx( action1, action2 )
	(ra.untry,rb.untry)
}

public fun interleaved( xs : list<() -> <asyncx|e> a> ) : <asyncx|e> list<a> {
	interleavedx(xs).map(untry)
}

public fun interleavedx( action1 : () -> <asyncx|e> a, action2 : () -> <asyncx|e> b ) : <asyncx|e> (either<exception,a>,either<exception,b>) {
    fun act1() { Left(action1()) }
    fun act2() { Right(action2()) }
    match( interleavedx([act1,act2])) {
        Cons(x,Cons(y)) -> (x.map(left),y.map(right))
        _ -> error("xinterleaved")
    }
}

fun left( e ) {
	match(e) {
		Left(x) -> x
	}
}

fun right( e ) {
	match(e) {
		Right(x) -> x
	}
}

public fun interleavedx( xs : list<() -> <asyncx|e> a> ) : <asyncx|e> list<either<exception,a>> {
	(unsafe-no-div { interleaved-div(xs) })()
}

// Add the state and div effect to a function effect.
extern inline inject-st-div : forall<a,h,e> (() -> e a) -> total (() -> <st<h>,div|e> a)   { inline "#1" }

extern inline unsafe-no-cps : forall<a,e> (() -> <cps|e> a) -> (() -> e a) { inline "#1" }
extern inline unsafe-no-div : forall<a,e> (() -> <div|e> a) -> (() -> e a) { inline "#1" }

fun from-values(vs) {
	match(vs) {
		Nil	-> Left([])
		Cons	-> Right(vs)
	}
}

fun interleaved-div( xs : list<() -> <asyncx|e> a> ) : <asyncx,div|e> list<either<exception,a>> {
	val n	= xs.length
	var count	:= n
	val results	= array(n,Left(exception("Interleaved operation was not completed",Internal("std/async/interleaved"))))
	val actions	= xs.map-indexed fun(i,action) {
			return fun() {
				results[i] := try( inject-st-div(action) )
				count := count - 1
			}
		}
	val ch : some<a,e> channel<() -> <asyncx|e> a> = channel()
	val handle-action = handler {
		do-await(ev) -> {
			no-await(ev, fun(res) {
				ch.emit-io( unsafe-no-cps{ resume(res) } )
			})
			()  // stop the computation
		}
		async-io(f)	-> resume(async-io(f))
		register(setup,scope)	-> resume(register(setup,scope))
		cancel(scope)	-> resume(cancel(scope))
	}
	actions.foreach( fun(action) { handle-action(action) } )
	while { count > 0 } {
		val schedule = ch.await
		//trace-any("schedule next: ", schedule )
		schedule()
	}
	results.list
}


// ----------------------------------------------------------------------------
// Asynchronous operations are supported through the `:async` effect.
// The basic effect is in core since it usually requires some support from the host system.
// ----------------------------------------------------------------------------


extern set-immediate( action : () -> e () ) : exn () {
  js inline "(typeof setImmediate !== 'undefined' ? setImmediate(#1) : setTimeout(#1,0))"
}

extern set-immediate1( action : (a) -> e (), arg : a ) : exn () {
  js inline "(typeof setImmediate !== 'undefined' ? setImmediate(#1,#2) : setTimeout(#1,0,#2))"
}


// ----------------------------------------------------------------------------
// do-await wrappers
// ----------------------------------------------------------------------------

public fun await-exn0( setup : (cb : (null<exception>) -> io () ) -> io () ) : <async,exn> () {
  awaite( fun(resume) {
    setup( fun(nexn) { resume(nexn.unnull(())) })
  } )
}

public fun await-exn1( setup : (cb : (null<exception>,a) -> io () ) -> io () ) : <async,exn> a {
  awaite( fun(resume) {
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
	awaite( fun(resume) { setup( fun() { resume(Right(())) } ) } )
}

public fun await1( setup : (cb : (a) -> io () ) -> io () ) : <async,exn> a {
	awaite( fun(resume) { setup( fun(x) { resume(Right(x)) } ) } )
}

public fun awaite( setup : (cb : either<exception,a> -> io () ) -> io () ) : <async,exn> a {
	do-await(register(setup,empty-scope)).untry
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

private struct sid( : list<int> )

val empty-scope = Sid([])

fun parent-scope( cid : int, sid : sid ) : sid {
	match(sid) {
		Sid(cids) -> Sid(Cons(cid,cids))
	}
}

fun child-scope( id : int, sid : sid ) : sid {
	match(sid) {
		Sid(cids) -> Sid(cids + [id])
	}
}

fun in-scope-of?( child : list<int>, parent : list<int> ) : bool {
	match(parent) {
		Nil -> True
		Cons(p,ps) -> match(child) {
			Cons(c,cs) -> (c == p && in-scope-of?(cs,ps))
			Nil -> False
		}
	}
}

fun in-scope-of?( child : sid, parent : sid ) : bool {
	match(parent) {
		Sid(pids) -> match(child) {
			Sid(cids) -> in-scope-of?(cids,pids)
		}
	}
}

fun (==)( ids1 : list<int>, ids2 : list<int> ) : bool {
	match(ids1) {
		Nil -> ids2.nil?
		Cons(i1,is1) -> match(ids2) {
			Cons(i2,is2) -> (i1 == i2 && is1 == is2)
			Nil -> False
		}
	}
}

fun (==)(sid1 : sid, sid2 : sid ) : bool {
	match(sid1) {
		Sid(ids1) -> match(sid2) {
			Sid(ids2) -> ids1==ids2
		}
	}
}

fun (!=)(sid1 : sid, sid2 : sid ) : bool {
	!(sid1 == sid2)
}


// ----------------------------------------------------------------------------
// Async effect
// ----------------------------------------------------------------------------
alias await-result<a> = either<exception,a>

type await-state<a> {
	Empty
	Done( result : either<exception,a> )
	Waiting( listener : either<exception,a> -> io () )
}

abstract struct await-event<a> (
	state : ref<global,await-state<a>>
)


// Asynchronous operations have the `:async` effect.
abstract effect async {
  fun register( setup : (cb : either<exception,a> -> io ()) -> io (), scope : sid ) : await-event<a>
  fun do-await( ev : await-event<a> ) : await-result<a>
  fun async-io( action : () -> io a ) : a
  fun cancel( scope : sid ) : ()
}


// The `cancel` operations cancels any outstanding asynchronous operation under the innermost
// `cancelable` handler by returning the `Cancel` exception. The `cancel` operation itself returns normally
// without raising a `Cancel` exception.
public fun cancel() : async (){
	cancel(empty-scope)
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
	val cid = async-io{ unique() }

	handle(inject-st(action)) {
		register(setup,sid) -> {
			resume(register(setup,parent-scope(cid,sid)))
		}
		do-await(ev) -> {
			resume(do-await(ev))
		}
		cancel(sid) -> {
			resume(cancel(parent-scope(cid,sid)))
		}
		async-io(f)	-> resume(async-io(f))
	}
}

// ----------------------------------------------------------------------------
// Async handle
// ----------------------------------------------------------------------------


fun emit-io( ev : await-event<a>, res : await-result<a> ) : io () {
	val r = ev.state
	match(!r) {
		Empty -> r := Done(res)
		Done -> error("callback called more than once")
		Waiting(listener) -> {
			r := Done(res) // or an exception?
			listener(res)
		}
	}
}

fun on-event( ev : await-event<_>, f : await-result<_> -> io ()) : io () {
	val r = ev.state
	match(!r) {
		Empty -> r := Waiting(f)
		Done(res) -> f(res)  // and set Done exception?
		Waiting -> error("callback can only have one awaiter")
	}
}

fun on-event-resume(ev : await-event<_>, resume : await-result<_> -> <io,cps> ()) : <io,cps> () {
	val r = ev.state
	match(!r) {
		Empty -> r := Waiting(outer-resume(resume))
		Done(res) -> resume(res)  // and set Done exception?
		Waiting -> error("callback can only have one awaiter")
	}
}

// The outer `:async` effect handler
public fun async-handle(action : () -> <async,io> () ) : io ()
{
	var callbacks := ( [] : list<(sid,_)> )

	fun handle-register( setup, scope ) {
		val ev	= Await-event(ref(Empty))
		val sid	= child-scope(unique(),scope)
		fun cb(res) {
			if (callbacks.contains(sid)) {  // if not contained, the callback was already invoked (with a cancel exception?)
				callbacks := callbacks.remove(sid)
				ev.emit-io(res)
			}
		}
		callbacks := Cons((sid,cb),callbacks)
		setup(cb)
		ev
	}

	fun handle-cancel( scope : sid )  {
		callbacks.foreach fun(entry) {
			val (sid,cb) = entry
			if (sid.in-scope-of?(scope)) then 	cb(Left(cancel-exn))
		}
	}

	handle(action) {
		do-await( ev )	-> on-event-resume(ev, resume)	// returns to outer event loop
		register( setup, scope )	-> handle-register(setup,scope).resume
		cancel( scope ) 	-> handle-cancel(scope).resume
		async-io( f )	-> resume(f())
	}
}

fun no-await(ev : await-event<a>, f : await-result<a> -> io () ) : async () {
	async-io {
		on-event(ev, f)
	}
}


// Convenience functions for sid maps
fun remove( xs : list<(sid,a)>, sid : sid ) : list<(sid,a)> {
  xs.remove( fun(x:(sid,_)) { x.fst == sid })
}

fun lookup( xs : list<(sid,a)>, sid : sid ) : maybe<a> {
  xs.lookup( fun(x:sid) { x == sid })
}

fun contains( xs : list<(sid,a)>, sid : sid ) : bool {
  xs.lookup(sid).bool
}


/*

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

*/
