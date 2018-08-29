/*---------------------------------------------------------------------------
	Copyright 2012-2016 Microsoft Corporation.

	This is free software; you can redistribute it and/or modify it under the
	terms of the Apache License, Version 2.0. A copy of the License can be
	found in the file "license.txt" at the root of this distribution.
---------------------------------------------------------------------------*/

/* Asynchronous primitives

This module is based closely on [@Leijen:async] and aims to have robust and composable asynchronous
primitives. In particular, any outstanding asynchronous operation can be canceled (through `cancel`)
within a certain scope which allows for composable primitives like `timeout` and `firstof`.


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
import std/num/ddouble    // for C# backend
import std/time/duration
import std/debug

extern include {
	cs file "async-inline.cs"
}

// A type alias for asynchronous operations that can raise exceptions non-deterministically.
// This is common for almost all `:async` operations since `cancel` and `timeout` can
// cancel operations non-deterministically which raises the `Cancel` exception and cancels
// outstanding asynchronous requests.
public alias asyncx = <async,exn,ndet>


// ----------------------------------------------------------------------------
// Promises
// ----------------------------------------------------------------------------

// A _promise_ that carries a value of type `:a`. A promise is initially empty
// but can be `await`ed  asynchronously until it gets `resolve`d unblocking any
// `await` operations. After that a promise stays resolved and any `await` will
// return immediately. It is an error to try to resolve a promise more than once.
abstract struct promise<a>(
	state : ref<global,promise-state<a>>
)

abstract type promise-state<a> {
	Resolved( value : a )
	Awaiting( listeners : list<a -> io ()> )
}

// Create a new promise.
public fun promise() : async promise<a> {
	async-iox { Promise(ref(Awaiting([]))) }
}

// Await a promise; returns immediately if the promise was already resolved and otherwise
// waits asynchronously.
public fun await( p : promise<a> ) : asyncx a {
	fun setup(cb) {
		val r = p.state
		match (!r) {
			Awaiting(listeners) -> r := Awaiting(Cons(cb,listeners))
			Resolved(value) -> cb(value) // resume right away; should not happen due to try-await
		}
	}
	match(p.try-await) {
		Just(v) -> v
		Nothing -> await1(setup)
	}
}

// Returns immediately if the promise was already resolved and otherwise return `Nothing`.
public fun try-await( p : promise<a> ) : <async,ndet> maybe<a> {
	async-io-noexn {
		val r = p.state
		match(!r) {
			Resolved(value) -> Just(value)
			_ -> Nothing
		}
	}
}

// Resolve a promise to `value`. Raises an exception if the promise was already resolved.
public fun resolve( p : promise<a>, value : a ) : asyncx () {
	async-io {
		val r = p.state
		match(!r) {
			Awaiting(listeners) -> {
				r := Resolved(value)
				listeners.foreach fun(cbx) {  // todo: through set-immediate?
					cbx(value) // set-immediate1( cbx, value )
				}
			}
			_ -> error("Promise was already resolved")
		}
	}
}

// ----------------------------------------------------------------------------
// Channels
// ----------------------------------------------------------------------------

// A _channel_ of values of type `:a`. Values can be asynchronously `emit`ed into
// a channel, and asynchronously `receive`d.
abstract struct channel<a>(
	chid : int,
	state : ref<global,channel-state<a>>
)

// todo: use queue data type for the values and listeners for better complexity
abstract type channel-state<a> {
	Empty
	Values( value : a, values : list<a> = [] )
	Waiting( listener : a -> io (), listeners : list<a -> io ()> = [] )
}

fun from-values(values : list<a> ) : channel-state<a> {
	match(values) {
		Nil -> Empty
		Cons(v,vs)  -> Values(v,vs)
	}
}

fun from-waiting(listeners : list<a -> io ()>) : channel-state<a> {
	match(listeners) {
		Nil -> Empty
		Cons(l,ls)  -> Waiting(l,ls)
	}
}

// Create a new asynchronous channel.
public fun channel() : async channel<a> {
	async-iox {
		Channel(unique(), ref(Empty))
	}
}

// Receive (and remove) a value from the channel: returns immediately if a value is available and otherwise
// waits asynchronously until a value becomes available.
public fun receive( ch : channel<a> ) : asyncx a {
	ch.receivex.untry
}

fun receivex( ch : channel<a>, cancelable? : bool = True ) : <async,ndet> try<a> {
	fun setup( cb : (_,_) -> io () )  {
		fun cbr(x) { cb(Ok(x),True) }
		val r = ch.state
		match (!r) {
			Empty -> r := Waiting(cbr,[])
			Waiting(l,ls) -> r := Waiting(l,ls + [cbr])
			Values(v,vs)-> {  // this case should not happen due to `try-receive?`
				r := from-values(vs)
				cbr(v)
			}
		}
		Nothing
	}
	match(ch.try-receive?) {
		Just(v) -> Ok(v)
		Nothing -> do-await(setup,empty-scope,cancelable?)
	}
}

// Return immediately if a value is available on the channel and otherwise returns `Nothing`.
public fun try-receive?( ch : channel<a> ) : <async,ndet> maybe<a> {
	async-io-noexn {
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
		Empty -> r := Values(value, [])
		Values(v,vs)  -> r := Values(v,vs + [value])
		Waiting(l,ls) -> {
			r := from-waiting(ls)
			l(value)
		}
	}
}

// Emit a value asynchronously into a channel.
public fun emit( ch : channel<a>, value : a ) : asyncx () {
	async-io {
		emit-io(ch,value)
	}
}

fun trace-channel( msg : string, ch : channel<a> ) : <async,ndet> () {
	async-io-noexn {
		trace-channel-io( msg, ch )
	}
}

fun trace-channel-io( msg : string, ch : channel<a> ) : io-noexn () {
	val msgx = msg + ": id=" + ch.chid.show
	val r = ch.state
	match(!r) {
		Empty -> trace(msgx + ", empty")
		Values(v,vs) -> trace-any(msgx + ", full: " + (1 + vs.length).show + ": ", v )
		Waiting(_,ls) -> trace(msgx + ", listeners: " + (1 + ls.length).show)
	}
}

fun trace-anyx( s : string, x : a ) : async () {
	trace-any(s,x)
}

// ----------------------------------------------------------------------------
// Asynchronous timeout and waiting
// ----------------------------------------------------------------------------

// Execute `action` but if it is not finished within `secs` seconds duration
// `cancel` it (and return `Nothing`). Due to the generality of `cancel`, this `timeout`
// abstraction can reliably time out over any composition of asynchronous operations
// and is therefore quite expressive.
public fun timeout( secs : duration, action : () -> <asyncx|e> a ) : <asyncx|e> maybe<a> {
	firstof	{ wait(secs); Nothing }
		{ Just(action()) }
}

// Execute `a` and `b` interleaved. As soon as one of them finishes,
// `cancel` the other one and return the result of the first.
public fun firstof( a : () -> <asyncx|e> a, b : () -> <asyncx|e> a  ) : <asyncx|e> a  {
	cancelable {
		val (ra,rb) = interleavedx	{ val x = a(); cancel(); x }
			{ val x = b(); cancel(); x }
		match(ra) {
			Exn(exn) | exn.cancel? -> rb.untry
			_  -> ra.untry
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
public fun wait( secs : duration = zero ) : <async,exn> () {
	if (secs <= zero) then return yield()
	val msecs = max(zero:int32,secs.milli-seconds.int32)
	await fun(cb) {
		val tid = set-timeout( fun(){ cb(Ok(())) }, msecs )
		Just( { clear-timeout(tid) } )
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
abstract struct timeout-id(
	timer : any
)

fun set-timeout( cb : () -> io (), ms : int32 ) : io timeout-id {
	Timeout-id(set-timeoutx(cb,max(ms,zero)))
}

extern set-timeoutx( cb : () -> io (), ms : int32 ) : io any {
	cs "_Async.SetTimeout"
	js "setTimeout"
}

fun clear-timeout( tid : timeout-id ) : io () {
	clear-timeoutx(tid.timer)
}

extern clear-timeoutx( tid : any) : io () {
	cs "_Async.ClearTimeout"
	js "clearTimeout"
}


// ----------------------------------------------------------------------------
// Interleaved strands of execution
// ----------------------------------------------------------------------------

// Interleave two actions around their asynchronous operations.
public fun interleaved( action1 : () -> <async,exn|e> a, action2 : () -> <async,exn|e> b ) : <async,exn|e> (a,b) {
	val (ra,rb) = interleavedx( action1, action2 )
	[ra.maybe,rb.maybe].ordered_throw
	(ra.untry,rb.untry)
}

// Interleave a list of actions around their asynchronous operations.
public fun interleaved( xs : list<() -> <async,exn|e> a> ) : <async,exn|e> list<a> {
	val ress = interleavedx(xs)
	ress.map(maybe).ordered_throw
	ress.map(untry)
}

fun ordered_throw( xs : list<maybe<exception>> ) : exn () {
	var mexn := Nothing
	xs.foreach fun(x) {
		match(x) {
			Nothing -> ()
			Just(exn) -> match(mexn) {
				Nothing -> mexn := x
				Just(exnx) ->
					if ((exn.finalize? && !exnx.finalize?) || (exnx.cancel? && !exn.cancel?))
						then mexn := x
			}
		}
	}
	match(mexn) {
		Just(exn)	-> throw(exn)
		Nothing	-> ()
	}
}

// Interleave two actions around their asynchronous operations and explicitly returning either
// their result or their exception.
public fun interleavedx( action1 : () -> <async,exn|e> a, action2 : () -> <async,exn|e> b ) : <async|e> (try<a>,try<b>) {
	fun act1() { Left(action1()) }
	fun act2() { Right(action2()) }
	match( interleavedx([act1,act2])) {
		Cons(x,Cons(y)) -> (x.unleft,y.unright)
		_ -> {
			// this will never happen..
			val exn = exception("invalid interleaved result",Internal("std/async/interleavedx(action1,action2)"))
			(Exn(exn),Exn(exn))
		}
	}
}

fun unleft( x : try<either<a,b>> ) : try<a> {
	match(x) {
		Ok(Left(l)) -> Ok(l)
		Exn(exn)  -> Exn(exn)
		_ -> Exn(exception("invalid left interleaved result",Internal("std/async/interleavedx(action1,action2)")))
	}
}
fun unright( x : try<either<a,b>> ) : try<b> {
	match(x) {
		Ok(Right(r)) -> Ok(r)
		Exn(exn)     -> Exn(exn)
		_ -> Exn(exception("invalid right interleaved result",Internal("std/async/interleavedx(action1,action2)")))
	}
}

// Private effect to keep track of when a strand in an interleaving is done.
// Preferred over using built-in state as this works well if there is an outer handler
// over the state that resumes more than once -- redoing part of the interleaving.
// See `test/algeff/async5.js`
effect strands<a> {
	// Are there still strands that need to be resumed?
	fun strands-busy?() : bool
	// Call this when a strand is done.
	fun strand-done(idx : int, result : try<a>) : ()
}

// Insert in order with an accumulating list.
fun insert-acc( xs : list<(int,a)>, idx : int, value : a, acc : list<(int,a)> ) : list<(int,a)> {
	match(xs) {
		Cons(x,xx) | x.fst < idx -> insert-acc(xx, idx, value, Cons(x,acc))
		_ -> reverse-append( acc, Cons((idx,value),xs) )
	}
}

// Insert in order
fun insert( xs : list<(int,a)>, idx : int, value : a, n : int  = 0 ) : list<(int,a)> {
	if (n > 100)
		then insert-acc( xs, idx, value, [] )
		else match(xs) {
			Cons(x,xx) | x.fst < idx -> Cons(x, insert(xx, idx, value, n + 1))
			_ -> Cons((idx,value),xs)
		}
}


// Interleave a list actions around their asynchronous operations and explicitly returning either
// either their result or their exception.
public fun interleavedx( xs : list<() -> <async,exn|e> a> ) : <async|e> list<try<a>> {
	val n = xs.length
	if (n==0) then []
	elif (n==1) then xs.map(unsafe-try-all)
	else {
		val handle-strands = handler(cr : some<a>(int,list<(int,try<a>)>)) {
			return x	-> (cr.snd : list<(_,_)>).map( snd )
			strands-busy?()	-> resume(cr.fst > 0, cr)
			strand-done(idx,res)	-> resume((), (cr.fst - 1, cr.snd.insert(idx,res)))
		}
		handle-strands( (n, []), unsafe-no-ndet-div{ interleaved-div(xs) } )
	}
}

extern inline unsafe-no-ndet-div : forall<a,e> (() -> <ndet,div|e> a) -> (() -> e a) { inline "#1" }
extern inline inject-effects : forall<a,h,e> (() -> e a) -> total (() -> <strands<a>,ndet,div|e> a)   { inline "#1" }

fun finalize?( t : try<a> ) : bool {
	match(t) {
		Exn(exn) -> exn.finalize?
		_ -> False
	}
}

fun cancel?( t : try<a> ) : bool {
	match(t) {
		Exn(exn) -> exn.cancel?
		_ -> False
	}
}

fun interleaved-div( xs : list<() -> <async,exn|e> a> ) : <async,ndet,div,strands<a>|e> () {
	val strands =	xs.map-indexed fun(i,action) {
			return fun() {
				val res = unsafe-try-all(inject-effects(action))
				strand-done(i,res)
				if (res.finalize?) then cancel()  // cancel others if finalization happens
			}
		}
	val ch : some<a,e> channel<() -> <async|e> a> = channel()
	val handle-strand = handler {
		fun raw do-await(setup,scope,c) -> {
			no-await( setup, scope, c) fun(res) {
				// emit a resumption of this strand into the channel
				ch.emit-io( /* no-cps */ { context.resume(res) }  )
			}
			()  // stop the strand at this point
		}

		// redirect all other operations
		fun no-await(setup,scope,c,f)	-> resume(no-await(setup,scope,c,f))
		fun async-iox(f)	-> resume(async-iox(f))
		fun cancel(scope)	-> resume(cancel(scope))
	}
	strands.foreach(handle-strand)

	while { strands-busy?() } {  // while there are resumptions on the strands..
		// the only way receive can throw is through a cancelation -- but in that case
		// we should not cancel but instead await all canceled strands; so keep listening on the channel.
		match(ch.receivex(False)) {
			Exn(_exn) -> () // ignore cancelation on receive
			Ok(strand-resume) -> strand-resume()
		}
	}
	()
}



// ----------------------------------------------------------------------------
// Await wrappers
// ----------------------------------------------------------------------------

// Convenience function for awaiting a NodeJS style callback where the first argument is a possible exception.
public fun await-exn0( setup : (cb : (null<exception>) -> io () ) -> io maybe<() -> io ()> ) : <async,exn> () {
	await( fun(cb) {
		setup( fun(nexn) { cb(nexn.unnull(())) })
	} )
}

// Convenience function for awaiting a NodeJS style callback where the first argument is a possible exception
// and the second argument the possible result value.
public fun await-exn1( setup : (cb : (null<exception>,a) -> io () ) -> io maybe<() -> io ()> ) : <async,exn> a {
	await( fun(cb) {
		setup( fun(nexn,x) { cb(nexn.unnull(x)) })
	} )
}

fun unnull( nexn : null<exception>, x : a  ) : try<a> {
	match(nexn.maybe) {
		Nothing -> Ok(x)
		Just(exn) -> Exn(exn)
	}
}

// Convenience function for awaiting a zero argument callback.
public fun await0( setup : (cb : () -> io () ) -> io () ) : <async,exn> () {
	await( fun(cb) { setup( fun() { cb(Ok(())) } ); Nothing } )
}

// Convenience function for awaiting a single argument callback.
public fun await1( setup : (cb : (a) -> io () ) -> io () ) : <async,exn> a {
	await( fun(cb) { setup( fun(x) { cb(Ok(x)) } ); Nothing } )
}

// Execute `setup` to set up an asynchronous callback with the host platform. Invoke `cb` as the callback:
// it takes either an exception or a result `a`. Usually `setup` returns `Nothing` but you can return a `Just(cleanup)`
// value where the `cleanup` functions is invoked on cancellation to dispose of any resources (see the implementation of `wait`).
// The callback should be invoked exactly once -- when that happens `await` is resumed with the result using `untry`
// either raise an exception or return the plain result.
public fun await( setup : (cb : try<a> -> io () ) -> io maybe<() -> io ()> ) : <async,exn> a {
	await-exn(setup).untry
}




// ----------------------------------------------------------------------------
// Async effect
// ----------------------------------------------------------------------------
alias await-result<a> = try<a>
alias await-setup<a> = (cb : (try<a>,bool) -> io ()) -> io maybe<() -> io ()>

// Asynchronous operations have the `:async` effect.
abstract effect async {
	fun do-await( setup : await-setup<a>, scope : scope, cancelable? : bool ) : try<a>
	fun no-await( setup : await-setup<a>, scope : scope, cancelable? : bool, f : try<a> -> io ()  ) : ()
	fun async-iox( action : () -> io-noexn a ) : a
	fun cancel( scope : scope ) : ()
}


// The `cancel` operations cancels any outstanding asynchronous operation under the innermost
// `cancelable` handler by returning the `Cancel` exception. The `cancel` operation itself returns normally
// without raising a `Cancel` exception.
public fun cancel() : async (){
	cancel(empty-scope)
}

// Primitive: Execute `setup` to set up an asynchronous callback with the host platform. Invoke `cb` as the callback:
// it takes either an exception or a result `a`. Usually `setup` returns `Nothing` but you can return a `Just(cleanup)`
// value where the `cleanup` functions is invoked on cancellation to dispose of any resources (see the implementation of `wait`).
// The callback should be invoked exactly once -- when that happens `await-exn` is resumed with the result.
public fun await-exn( setup : (cb : (try<a>) -> io ()) -> io maybe<() -> io ()> ) : async try<a> {
	do-await(fun(cb) { setup(fun(res) { cb(res,True) }) }, empty-scope, True )
}

// Primitive: Execute `setup` to set up an asynchronous callback with the host platform. Invoke `cb` as the callback: it takes either
// an exception or a result value, together with boolean parameter whether the callback is done.
// The callback `cb` will eventually emit the result into the given channel `ch` after applying the transformation `f` to the result.\
// Note: once you exit the `cancelable` scope where `await-to-channel` was called, the callback is invoked with a `Cancel` exception.
// The channel should always be awaited within the same `cancelable` scope as the `await-to-channel` invokation.
public fun await-to-channel( setup : (cb : (try<a>,bool) -> io ()) -> io maybe<() -> io ()>, ch : channel<b>, f : try<a> -> b  ) : async channel<b> {
	no-await(setup,empty-scope,True) fun(res) {
		ch.emit-io( f(res) )
	}
	ch
}

fun async-io-noexn( f : () -> io-noexn a ) : <async,ndet> a {
	async-iox(f)
}

// Perform an I/O operation at the outer level; exceptions are propagated back.
fun async-io( f : () -> io a ) : asyncx a {
	async-io-noexn( { try(f) } ).untry
}


// ----------------------------------------------------------------------------
// Async handlers: cancelable
// ----------------------------------------------------------------------------

// Execute `action` in a cancelable scope. If `cancel` is called within `action`,
// any outstanding asynchronous operations started in the cancelable scope are canceled.
// (Outstanding operations outside the cancelable scope are not canceled).
public fun cancelable( action : () -> <async|e> a ) : <async|e> a
{
	val cid = async-iox{ unique() }

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
		do-await(setup,scope,c)	-> resume(do-await(setup,scope.extend,c))
		no-await(setup,scope,c,f)	-> resume(no-await(setup,scope.extend,c,f))
		cancel(scope)	-> resume(cancel(scope.extend))
		async-iox(f)	-> resume(async-iox(f))
	}
}

// ----------------------------------------------------------------------------
// Async handle
// ----------------------------------------------------------------------------

fun nodispose() : io () {   () }

// The outer `:async` effect handler. This is automatically applied by the compiler
// around the `main` function if it has an `:async` effect.
public fun async-handle(action : () -> <async,io> () ) : io ()
{
	var callbacks : list<(scope,() -> io ())>  := []

	fun handle-await( setup : await-setup<a>, scope : scope, f : try<a> -> io (), cancelable? : bool) : io ()
	{
		val cscope = child-scope(unique(),scope)
		var dispose := nodispose

		fun cb( res : try<_>, done? : bool ) : io () {
			if (callbacks.contains(cscope)) {
				if (done?) then {
					callbacks := callbacks.remove(cscope)
					if (res.exn?) then try-default((),dispose)
				}
				f(res)
			}
		}

		// trace("register: " + cscope.show)
		callbacks := Cons((cscope, if (cancelable?) then fun(){ cb(Exn(unsafe-cancel-exn()),True) } else nodispose), callbacks)
		try {
			// setup the callback which returns a possible dipose function
			match(setup(cb)) {
				Just(d) -> dispose := d
				Nothing -> ()
			}
		}
		fun(exn) {
			// if setup fails, immediately resume with the exception
			cb(Exn(exn),True)
		}
	}

	fun handle-cancel( scope : scope ) : io () {
		callbacks.foreach fun(entry) {
			val (cscope,cb) = entry
			if (cscope.in-scope-of?(scope)) then cb()
		}
	}

	handle(action) {
		fun raw do-await( setup, scope, c )	-> handle-await(setup,scope, fun(x){ context.resume(x) }, c)  // returns to outer event loop
		fun no-await( setup, scope, c, f )	-> resume(handle-await(setup,scope,f,c))
		fun cancel( scope )	-> resume(handle-cancel(scope))
		fun async-iox( f )	-> resume(f())
	}
}


// ----------------------------------------------------------------------------
// Scope identifiers
// ----------------------------------------------------------------------------

abstract struct scope( : list<int> )

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


fun show( s : scope ) : string {
	match(s) {
		Scope(ids) -> ids.map(show).join("-")
	}
}
