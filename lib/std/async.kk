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
// This is common for almost all `:async` operations since `cancel` and `timeout` can
// cancel operations non-deterministically which raises the `Cancel` exception and cancels
// outstanding asynchronous requests.
public alias asyncx = <async,exn,ndet>


// ----------------------------------------------------------------------------
// Promises
// ----------------------------------------------------------------------------

abstract struct promise<a>(
	state : ref<global,promise-state<a>>
)

type promise-state<a> {
	Resolved( value : a )
	Awaiting( listeners : list<a -> io ()> )
}

public fun promise() : <async,exn> promise<a> {
	async-io{ Promise(ref(Awaiting([]))) }
}

public fun await( p : promise<a> ) : asyncx a {
	fun setup(cb) {
		val r = p.state
		match (!r) {
			Awaiting(listeners)	-> r := Awaiting(Cons(cb,listeners))
			Resolved(value)	-> cb(value) // resume right away; should not happen due to try-await
		}
	}
	match(p.try-await) {
		Just(v)	-> v
		Nothing	-> await1(setup)
	}
}

public fun try-await( p : promise<a> ) : async maybe<a> {
	async-io {
		val r = p.state
		match(!r) {
			Resolved(value)	-> Just(value)
			_	-> Nothing
		}
	}
}

public fun resolve( p : promise<a>, value : a ) : <async,exn> () {
	async-io {
		val r = p.state
		match(!r) {
			Awaiting(listeners) -> {
				r := Resolved(value)
				listeners.foreach fun(cbx) {  // todo: through set-immediate?
					cbx(value) // set-immediate1( cbx, value )
				}
			}
			_ -> error("promise was already resolved.")
		}
	}
}

// ----------------------------------------------------------------------------
// Channels
// ----------------------------------------------------------------------------


abstract struct channel<a>(
	state : ref<global,channel-state<a>>
)

type channel-state<a> {
	Empty
	Values( value : a, values : list<a> = [] )
	Waiting( listener : a -> io (), listeners : list<a -> io ()> = [] )
}

fun from-values(values : list<a> ) : channel-state<a> {
	match(values) {
		Nil	-> Empty
	 	Cons(v,vs)	-> Values(v,vs)
	}
}

fun from-waiting(listeners : list<a -> io ()>) : channel-state<a> {
	match(listeners) {
		Nil	-> Empty
	 	Cons(l,ls)	-> Waiting(l,ls)
	}
}
public fun channel() : async channel<a> {
  async-io{ Channel(ref(Empty)) }
}

public fun receive( ch : channel<a> ) : asyncx a {
	fun setup( cb : _ -> io () ) : io () {
		val r = ch.state
		match (!r) {
			Empty -> r := Waiting(cb)
			Waiting(l,ls) -> r := Waiting(l,ls + [cb])
			Values(v,vs)-> {  // this case should not happen due to `peek?`
				r := from-values(vs)
				cb(v)
			}
		}
	}
	match(ch.try-receive?) {
		Just(v) -> v
		Nothing -> await1(setup)
	}
}

public fun try-receive?( ch : channel<a> ) : <async,ndet> maybe<a> {
	async-io {
		val r = ch.state
		match (!r) {
			Values(v, vs) -> {
				r := from-values(vs)
				Just(v)
			}
			_ -> Nothing
		}
	}
}

fun emit-io( ch : channel<a>, value : a ) : io () {
	val r = ch.state
	match(!r) {
		Empty	-> r := Values(value)
		Values(v,vs)	-> r := Values(v,vs + [value])
		Waiting(l,ls) -> {
			r := from-waiting(ls)
			l(value)
		}
	}
}

public fun emit( ch : channel<a>, value : a ) : async () {
	async-io {
		emit-io(ch,value)
	}
}


// ----------------------------------------------------------------------------
// Asynchronous timeout and waiting
// ----------------------------------------------------------------------------

// Execute `action` but if it is not finished within `secs` seconds duration
// `cancel` it.
public fun timeout( secs : duration, action )  {
	firstof	{ wait(secs); Nothing }
		{ Just(action()) }
}

// Execute `a` and `b` interleaved. As soon as one of them finishes,
// `cancel` the other one and return the result of the first.
public fun firstof( a, b ) {
	cancelable {
		val (ra,rb) = interleavedx	{ val x = a(); cancel(); x }
			{ val x = b(); cancel(); x }
		match(ra) {
			Left(exn) | exn.info.cancel? -> rb.untry
			_ -> ra.untry
		}
	}
}

// Wait (asynchronously) for `secs` seconds as a `:double`.
// Use `yield()` to yield to other asynchronous operations.
public fun wait( secs : double ) : <async,exn> () {
  wait(secs.duration)
}

// Wait (asynchronously) for optional `secs` seconds `:duration` (`= 0.seconds`).
// Use `yield()` to yield generally to other asynchronous operations.
// If the wait time is longer than 200ms the wait can be canceled.
public fun wait( secs : duration = zero ) : <async,exn> () {
  if (secs <= zero) then return yield()
  val msecs = max(zero:int32,secs.milli-seconds.int32)
  await fun(cb) {
	  val tid = set-timeout( fun(){ cb(Right(())) }, msecs )
	  Just { clear-timeout(tid) }
  }
}

// Yield to other asynchronous operations. Same as `wait(0)`.
public fun yield() : <async,exn> () {
  await0 fun(cb) {
    set-timeout( cb, zero )
    ()
  }
}

// abstract wid for timeout handlers
type timeout-id

extern set-timeout( cb : () -> io (), ms : int32 ) : io timeout-id {
  js "setTimeout"
}

extern set-timeout1( cb : a -> io (), ms : int32, arg : a ) : io timeout-id {
  js "setTimeout"
}

extern clear-timeout( tid : timeout-id ) : exn () {
  js "clearTimeout"
}

extern set-immediate( action : () -> e () ) : exn () {
  js inline "(typeof setImmediate !== 'undefined' ? setImmediate(#1) : setTimeout(#1,0))"
}

extern set-immediate1( action : (a) -> e (), arg : a ) : exn () {
  js inline "(typeof setImmediate !== 'undefined' ? setImmediate(#1,#2) : setTimeout(#1,0,#2))"
}



// ----------------------------------------------------------------------------
// Interleaved strands of execution
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

fun interleaved-div( xs : list<() -> <asyncx|e> a> ) : <asyncx,div|e> list<either<exception,a>> {
	val n	= xs.length
	var count	:= n
	val results	= array(n,Left(exception("Interleaved operation was not completed",Internal("std/async/interleaved"))))
	val strands	= xs.map-indexed fun(i,action) {
			return fun() {
				 val res = try( inject-st-div(action) )
				results[i] := res
				count := count - 1
			}
		}
	val ch : some<a,e> channel<() -> <asyncx|e> a> = channel()
	val handle-strand = handler {
		do-await(setup,scope) -> {
			no-await( setup, scope ) fun(res) {
				//trace-any("emit in interleaved channel: ", res )
				// emit a resumption of this strand into the channel
				ch.emit-io( unsafe-no-cps{ resume(res) }  )
			}
			()  // stop the strand at this point
		}
		// redirect all other operations
		no-await(setup,scope,f)	-> resume(no-await(setup,scope,f))
		async-io(f)	-> resume(async-io(f))
		cancel(scope)	-> resume(cancel(scope))
	}
	strands.foreach( fun(strand) { handle-strand(strand) } )
	while { count > 0 } {  // while there are resumptions on the strands..
		val strand-resume = ch.receive
		//trace-any("schedule next: ", schedule )
		strand-resume()
	}
	results.list
}



// ----------------------------------------------------------------------------
// Await wrappers
// ----------------------------------------------------------------------------

public fun await-exn0( setup : (cb : (null<exception>) -> io () ) -> io maybe<() -> io ()> ) : <async,exn> () {
  await( fun(cb) {
    setup( fun(nexn) { cb(nexn.unnull(())) })
  } )
}

public fun await-exn1( setup : (cb : (null<exception>,a) -> io () ) -> io maybe<() -> io ()> ) : <async,exn> a {
  await( fun(cb) {
    setup( fun(nexn,x) { cb(nexn.unnull(x)) })
  } )
}

fun unnull( nexn : null<exception>, x : a  ) : either<exception,a> {
  match(nexn.maybe) {
    Nothing -> Right(x)
    Just(exn) -> Left(exn)
  }
}

public fun await0( setup : (cb : () -> io () ) -> io () ) : <async,exn> () {
	await( fun(cb) { setup( fun() { cb(Right(())) } ); Nothing } )
}

public fun await1( setup : (cb : (a) -> io () ) -> io () ) : <async,exn> a {
	await( fun(cb) { setup( fun(x) { cb(Right(x)) } ); Nothing } )
}

public fun await( setup : (cb : either<exception,a> -> io () ) -> io maybe<() -> io ()> ) : <async,exn> a {
	await-exn(setup).untry
}

public fun await-exn( setup : (cb : either<exception,a> -> io () ) -> io maybe<() -> io ()> ) : async either<exception,a> {
	do-await(fun(cb) { setup(fun(res) { cb(res,True) }) }, empty-scope )
}




// ----------------------------------------------------------------------------
// Async effect
// ----------------------------------------------------------------------------
alias await-result<a> = either<exception,a>
alias await-setup<a> = (cb : (either<exception,a>,bool) -> io ()) -> io maybe<() -> io ()>

// Asynchronous operations have the `:async` effect.
abstract effect async {
  fun do-await( setup : await-setup<a>, scope : scope ) : either<exception,a>
  fun no-await( setup : await-setup<a>, scope : scope, f : either<exception,a> -> io ()  ) : ()
  fun async-io( action : () -> io a ) : a
  fun cancel( scope : scope ) : ()
}


// The `cancel` operations cancels any outstanding asynchronous operation under the innermost
// `cancelable` handler by returning the `Cancel` exception. The `cancel` operation itself returns normally
// without raising a `Cancel` exception.
public fun cancel() : async (){
	cancel(empty-scope)
}

public fun do-await( setup : await-setup<a> ) : async either<exception,a> {
	do-await(setup,empty-scope)
}
public fun no-await( setup : await-setup<a>, f : either<exception,a> -> io ()  ) : async () {
	no-await(setup,empty-scope,f)
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

	fun extend(scope : scope ) {
		parent-scope(cid,scope)
	}

	handle(action) {
		return x -> {
			// cancel any outstanding operations still in our scope.
			// this might be needed for `no-await` operations.
			cancel(empty-scope.extend)
			x
		}
		do-await(setup,scope)	-> resume(do-await(setup,scope.extend))
		no-await(setup,scope,f)	-> resume(no-await(setup,scope.extend,f))
		cancel(scope)	-> resume(cancel(scope.extend))
		async-io(f)	-> resume(async-io(f))
	}
}

// ----------------------------------------------------------------------------
// Async handle
// ----------------------------------------------------------------------------

fun nodispose() : io () {
	()
}

// The outer `:async` effect handler
public fun async-handle(action : () -> <async,io> () ) : io ()
{
	var callbacks : list<(scope,() -> io ())>  := []

	fun handle-await( setup : await-setup<a>, scope : scope, f  : either<exception,a> -> io ()) : <io,cps> () {
		val cscope = child-scope(unique(),scope)
		var dispose := nodispose
		fun cb( res : either<exception,_>, done? : bool ) {
			if (callbacks.contains(cscope)) {
				if (done?) then {
					callbacks := callbacks.remove(cscope)
					if (res.left?) then dispose()
				}
				f(res)
			}
		}
		callbacks := Cons((cscope, { cb(Left(cancel-exn),True) } ), callbacks)
		match(setup(cb)) {
			Nothing	-> ()
			Just(d)	-> dispose := d
		}
	}

	fun handle-cancel( scope : scope ) {
		callbacks.foreach fun(entry) {
			val (cscope,cb) = entry
			if (cscope.in-scope-of?(scope)) then cb()
		}
	}

	handle(action) {
		do-await( setup, scope )	-> handle-await(setup,scope,outer-resume(resume))	// returns to outer event loop
		no-await( setup, scope, f )	-> resume(handle-await(setup,scope,f))
		cancel( scope ) 	-> resume(handle-cancel(scope))
		async-io( f )	-> resume(f())
	}
}


// ----------------------------------------------------------------------------
// Scope identifiers
// ----------------------------------------------------------------------------

private struct scope( : list<int> )

val empty-scope = Scope([])

fun parent-scope( cid : int, scope : scope ) : scope {
	match(scope) {
		Scope(cids) -> Scope(Cons(cid,cids))
	}
}

fun child-scope( id : int, scope : scope ) : scope {
	match(scope) {
		Scope(cids) -> Scope(cids + [id])
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

fun in-scope-of?( child : scope, parent : scope ) : bool {
	match(parent) {
		Scope(pids) -> match(child) {
			Scope(cids) -> in-scope-of?(cids,pids)
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

fun (==)(scope1 : scope, scope2 : scope ) : bool {
	match(scope1) {
		Scope(ids1) -> match(scope2) {
			Scope(ids2) -> ids1==ids2
		}
	}
}

fun (!=)(scope1 : scope, scope2 : scope ) : bool {
	!(scope1 == scope2)
}


// Convenience functions for scope maps
fun remove( xs : list<(scope,a)>, scope : scope ) : list<(scope,a)> {
  xs.remove( fun(x:(scope,_)) { x.fst == scope })
}

fun lookup( xs : list<(scope,a)>, scope : scope ) : maybe<a> {
  xs.lookup( fun(x:scope) { x == scope })
}

fun contains( xs : list<(scope,a)>, scope : scope ) : bool {
  xs.lookup(scope).bool
}
