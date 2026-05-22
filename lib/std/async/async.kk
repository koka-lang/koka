/*---------------------------------------------------------------------------
  Copyright 2012-2021, Microsoft Research, Daan Leijen.

  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the LICENSE file at the root of this distribution.
---------------------------------------------------------------------------*/

/* Asynchronous primitives

This module is based closely on [@Leijen:async] and aims to have robust and composable asynchronous
primitives. In particular, any outstanding asynchronous operation can be canceled (through `cancel`)
within a certain scope which allows for composable primitives like `timeout` and `firstof`.


## References {-
~ Bibliography { caption:"0"~~ BibItem { #Leijen:async; bibitem-label:"[1]"; searchterm:"Leijen+Daan+Structured+Asynchrony+with+Algebraic+Effects"Daan Leijen.
_Structured Asynchrony with Algebraic Effects_.
Microsoft Research technical report MSR-TR-2017-21, May 2017.
[pdf](https://www.microsoft.com/en-us/research/wp-content/uploads/2017/05/asynceffects-msr-tr-2017-21.pdf)
~~
~
\/
*/
module std/async/async

import std/data/dict
import std/data/array
import std/num/int64
import std/num/ddouble    // for C# backend
import std/time/duration
import std/async/null
import std/core/unsafe
import std/core/undiv
pub import std/num/ddouble
pub import std/num/float64
import std/time/timestamp

import uv/core
import uv/timer

/*
# Cancellation scopes

Each scope has a unique ID (int).

When we await any async action, we associate it with its full scope chain. If
scope a spawns scope b which spawns scope c which spawns an action, that
action's scope is Scope([c, b, a]) - a scope is just a list of scope IDs,
childmost first.

When we decide to cancel a scope, we cancel it by ID, e.g. `b`. This will
cancel the following scopes:

`[b, _]` -> clearly in b `[_, b, _])` -> a child scope of b

It will not cancel something in `[a, d]` (i.e. within `a` but not `b`)

# Cancelation: up and down

When cancelation is triggered, it is generally triggered from _within_ the
scope to be canceled, and affects all transitive child scopes. There are many
reasons for cancelation:
 - returning normally, cancelation prevents dangling callbacks
 - returning abnormally (e.g. exn), we cancel any pending callbacks
 - within a `firstof`, we have decided to cancel all remaining strands after
   the first is complete

After cancelation is triggered, the canceler's execution continues as normal -
calling `cancel-scope` simply returns `()`.

Triggering affects actions within the scope - so when observing cancelation, it
being triggered from somewhere "up" the stack. It's like a reverse of an
exception (which is coming from "down" the stack and observed "up" the stack).

The way that cancelation is observed is that all outstanding callbacks beneath
the canceled scope receive a `Cancel` instead of a `Result`. Aside from
low-level code, you won't observe this - the higher level utilities for
awaiting a result will automatically invoke `discontinue()` on the execution
which is awaiting the result.

It's not intended to be possible to "catch" a `discontinue` - cancelation can
be deferred to protect some critical section (using `defer-cancelation`), but
that only means cancelation will take effect at the end of that critical
section - it cannot be stopped.

(Internally `interleave` does "catch" cancelation, but it's a special case and
is careful to maintain these semantics)

# Strands (interleave)

Multiple strands can be cooperatively executed, by running each in a custom
`async` handler. Instead of awaiting a result, the handler returns immediately
and the callback triggers an enqueue onto an internal channel.

Continuations are consumed from this channel until all strands have completed.

When one strand returns abnormally (e.g. exception), the entire scope is
canceled. This will cause any pending callbacks to be resolved with a Cancel
result.

# Interleave and cancelation

In order to control cancelation explicitly, `interleave-strands` creates a root
scope instead of a child scope to spawn operations in. No actions spawned
within this scope will be seen as children of the parent scope, preventing
automatic cancelation when a parent scope is canceled.

Instead, `interleave-strands` _observes_ when the parent scope is canceled, and
triggers cancelation on the child strands. Crucially this doesn't cancel the
interleaving bookkeeping itself, which ensures that:
 - `interleave-raw` always observes the final states of all strands, even if
   all of them are `Cancel`
 - We drive each strand to completion, including finalizers. If we simply
   abandoned the bookeeping, we'd potentially leave finalize actions sitting in
   an unconsumed channel instead of running them.

*/

// Asynchronous operations have the `:async` effect.
// **Note**: technically, async is capable of embedding arbitrary `ioc`
// actions, as this is typically required to interface with the event loop.
//
// Code which makes use of this low-level capability should take care to
// represent the appropriate semantic effects in addition to `async`.
pub effect async
  val async-scope: scope

  // setup a callback and await its completion
  ctl do-await( setup : await-setup<a>, scope : scope) : await-result<a>
  
  // setup a callback to execute `f` on completion, returning control to the caller immediately
  // after the setup is performed
  ctl no-await( setup : await-setup<a>, scope : scope, f : await-result<a> -> ioc ()  ) : ()

  // Internal: evaluate io in async
  ctl async-iox( action : () -> ioc a ) : a

  // trigger cancelation of a specific scope (and all its children)
  ctl cancel-scope( scope : scope ) : ()

  // React to cancelation of this expression
  final ctl discontinue(): a

// Backwards compatibility, no longer needed
// pub alias asyncx = <async,exn,ndet>
pub alias asyncx = <async>

pub alias async-exn = <async,exn>

pub fun cancel()
  cancel-scope(async-scope)

/*
pub fun on-cancel(fin: () -> <async|e> (), action: () -> <async|e> a): <async|e> a
  handle<async>({ mask behind<async>(action) })
    final ctl discontinue()
      fin()
      discontinue()

    // passthrough others
    val async-scope = async-scope
    fun do-await(setup, scope) do-await(setup,scope)
    fun no-await(setup, scope, f) no-await(setup,scope, f)
    fun async-iox(x) async-iox(x)
    fun cancel-scope(scope) cancel-scope(scope)
*/

fun async-st(action: () -> st<global> a): async a
  async-iox(action)

// ----------------------------------------------------------------------------
// Async effect
// ---------------------------------------------------------------------------- 
// "I/O as in C": a linear `:io` effect without exceptions (`:exn`). 
pub alias ioc = io-noexn

pub alias dispose-fn = () -> ioc ()

pub alias await-setup<a> = (evloop : event-loop, cb : (result:await-result<a>, isdone:bool) -> ioc ()) -> ioc error<dispose-fn>

pub type await-result<a>
  Cancel
  Exception( exn: exception )
  Result( value: a )

fun await-result/show(r: await-result<a>) : string
  match r
    Cancel -> "Cancel"
    Exception(exn) -> "Exception(" ++ show(exn) ++ ")"
    Result(_) -> "Result(_)"

fun continue-await(v: await-result<a>): async error<a>
  match v
    Result(value)  -> Ok(value)
    Exception(exn) -> Error(exn)
    Cancel         -> discontinue()

fun continue-await-exn(v: await-result<a>): async-exn a
  continue-await(v).untry

fun unsafe/continue-await-no-exn(v: await-result<a>): async a
  match continue-await(v) 
    Ok(res)    -> res
    Error(exn) -> impossible("unexpected exception from async operation: " ++ show(exn))



/*
// Prevents cancelation, but invokes a hook immediately for notification
fun unsafe-override-cancel(cancel: dispose-fn, action: () -> <async|e> a): <async|e> a
  val dangling-cb: ref<global,_> = async-st { ref(Nothing) }
  with finally {
    // ensure dangling-cb gets invoked so we don't leak uninvoked and uncancelled callbacks
    with async-iox
    match !dangling-cb
      Just(cb) -> cb(Result(()), True)
      Nothing -> ()
  }

  // within the outer scope, spawn a canary action
  // to capture cancelation triggered from above.
  fun setup-cb(evloop : event-loop, cb: ((await-result<()>, bool) -> ioc ())): ioc error<dispose-fn>
    dangling-cb.set(Just(cb))
    return Ok(cancel) // cleanup fn
  no-await(setup-cb, async-scope, fn(_) ())

  // create a new scope root, so that any callbacks registered
  // underneath this scope will not be seen as children of
  // any scopes above us (and therefore not canceled if an outer
  // scope is canceled)
  val cid = async-iox { unique() }
  val new-scope = empty-scope.child(cid)

  with finally {
    cancel-scope(cid) // don't leave anything dangling in case of exn etc
  }

  with handler<async>
    val async-scope = new-scope
    fun do-await(setup,scope) do-await(setup,scope)
    fun no-await(setup,scope,f) no-await(setup,scope,f)

    // We notice cancelation from a parent via the canary. We also
    // need to modify the `cancel-scope` hook so that we notice (and defer)
    // cancelation triggered from within this scope, but only if it's
    // targeted at exactly this scope.
    fun cancel-scope(scope-id)
      if scope-id == cid then
        async-iox(cancel)
      else
        // pass it along, it's presumably a child scope
        // nested under this uncancelable one
        cancel-scope(scope-id)

    fun async-iox(f) async-iox(f)
    fun discontinue() discontinue()

  with mask behind<async>
  action()

// defer cancelation of the child-scoped `action` until the end of `action`. Note that
// cancelation cannot be ignored, only deferred.
pub fun defer-cancelation(action: () -> <async|e> a): <async|e> a
  val canceled: ref<global,_> = async-st { ref(False) }
  fun on-cancel()
    canceled := True
  val result = unsafe-override-cancel(on-cancel, action)
  if (async-st { !canceled }) then
    discontinue()
  result
*/

// ----------------------------------------------------------------------------
// Promises
// ----------------------------------------------------------------------------

// A _promise_ that carries a value of type `:a`. A promise is initially empty
// but can be `await`ed  asynchronously until it gets `resolve`d unblocking any
// `await` operations. After that a promise stays resolved and any `await` will
// return immediately. It is an error to try to resolve a promise more than once.
abstract struct promise<a>
  state : ref<global,promise-state<a>>

abstract value type promise-state<a>
  Resolved( value : a )
  Awaiting( listeners : list<a -> io ()> )

// Create a new promise.
pub fun promise() : async promise<a>
  async-st { Promise(ref(Awaiting([]))) }

// Await a promise; returns immediately if the promise was already resolved and otherwise
// waits asynchronously.
pub fun promise/await( p : promise<a> ) : async a
  fun setup(cb : _ -> ioc ()): ioc ()
    val r = p.state
    match (!r)
      Awaiting(listeners) -> r := Awaiting(Cons(cb,listeners))
      Resolved(value) -> cb(value) // resume right away; should not happen due to try-await
  match p.try-await
    Just(v) -> v
    Nothing -> await1-noexn(setup)

// Returns immediately if the promise was already resolved and otherwise return `Nothing`.
pub fun try-await( p : promise<a> ) : <async> maybe<a>
  async-ioc
    val r = p.state
    match !r
      Resolved(value) -> Just(value)
      _ -> Nothing

// Resolve a promise to `value`. Raises an exception if the promise was already resolved.
pub fun resolve( p : promise<a>, value : a ) : <async,exn> ()
  async-io
    if !try-resolve-io(p, value) then
      throw("Promise was already resolved")

// Resolve a promise to `value`. Returns False if the promise was already resolved.
// TODO: make this ioc
pub fun try-resolve-io( p : promise<a>, value : a ) : io bool
  val r = p.state
  match !r
    Awaiting(listeners) ->
      r := Resolved(value)
      listeners.foreach fn(cbx)  // todo: through set-immediate?
        cbx(value) // set-immediate1( cbx, value )
      True
    _ -> False

// Perform an I/O operation at the outer level; exceptions are propagated back.
fun async-io( f : () -> io a ) : <async,exn> a
  async-ioc( { try(f) } ).untry

// ----------------------------------------------------------------------------
// Channels
// ----------------------------------------------------------------------------

// A _channel_ of values of type `:a`. Values can be asynchronously `emit`ed into
// a channel, and asynchronously `receive`d.
abstract value struct channel<a>(
  chid : int,
  state : ref<global,channel-state<a>>
)

// todo: use queue data type for the values and listeners for better complexity
abstract type channel-state<a>
  Empty
  Values( value : a, values : list<a> = [] )
  Waiting( listener : a -> ioc (), listeners : list<a -> ioc ()> = [] )

fun from-values(values : list<a> ) : channel-state<a>
  match values
    Nil -> Empty
    Cons(v,vs)  -> Values(v,vs)

fun from-waiting(listeners : list<a -> ioc ()>) : channel-state<a>
  match listeners
    Nil -> Empty
    Cons(l,ls)  -> Waiting(l,ls)

// Create a new asynchronous channel.
pub fun channel() : async channel<a>
  async-iox
    Channel(unique(), ref(Empty))

val no-dispose = Ok({})    

// Receive (and remove) a value from the channel: returns immediately if a value is available and otherwise
// waits asynchronously until a value becomes available.
fun receive( ch : channel<a>) : <async,div,ndet> a
  fun setup( cb : _ -> ioc () ) : ioc ()
    val r = ch.state
    match !r
      Empty -> r := Waiting(cb,[])
      Waiting(l,ls) -> r := Waiting(l,ls ++ [cb])
      Values(v,vs)->   // this case should not happen due to `try-receive`
        r := from-values(vs)
        cb(v)        

  match ch.try-receive
    Just(v) -> v
    Nothing -> await1-noexn(setup)

// Return immediately if a value is available on the channel and otherwise returns `Nothing`.
pub fun try-receive( ch : channel<a> ) : <async,ndet> maybe<a>
  async-ioc
    val r = ch.state
    match (!r)
      Values(v, vs) ->
        r := from-values(vs)
        Just(v)
      _ -> Nothing

fun emit-io( ch : channel<a>, value : a ) : ioc ()
  val r = ch.state
  match !r
    Empty -> r := Values(value, [])
    Values(v,vs)  -> r := Values(v,vs ++ [value])
    Waiting(l,ls) ->
      r := from-waiting(ls)
      l(value)

// Emit a value asynchronously into a channel.
pub fun emit( ch : channel<a>, value : a ) : <async,div> ()
  async-ioc
    emit-io(ch,value)

fun trace-channel( msg : string, ch : channel<a> ) : <async,ndet> ()
  async-ioc
    trace-channel-io( msg, ch )

fun trace-channel-io( msg : string, ch : channel<a> ) : ioc ()
  val msgx = msg ++ ": id=" ++ ch.chid.show
  val r = ch.state
  match !r
    Empty -> trace(msgx ++ ", empty")
    Values(v,vs) -> trace-any(msgx ++ ", full: " ++ (1 + vs.length).show ++ ": ", v )
    Waiting(_,ls) -> trace(msgx ++ ", listeners: " ++ (1 + ls.length).show)

fun trace-anyx( s : string, x : a ) : async ()
  trace-any(s,x)

// ----------------------------------------------------------------------------
// Asynchronous timeout and waiting
// ----------------------------------------------------------------------------

// Execute `action` but if it is not finished within `secs` seconds duration
// `cancel` it (and return `Nothing`). Due to the generality of `cancel`, this `timeout`
// abstraction can reliably time out over any composition of asynchronous operations
// and is therefore quite expressive.
pub fun timeout( secs : duration, action : () -> <async,ioc|e> a ) : <async,ioc|e> maybe<a>
  firstof { duration/wait(secs); Nothing} { Just(action()) }

// Wait (asynchronously) for `secs` seconds as a `:double`.
// Use `yield()` to yield to other asynchronous operations.
pub fun float/wait( secs : float64 ) : async ()
  wait(secs.duration)

// Wait (asynchronously) for optional `secs` seconds `:duration` (`= 0.seconds` or `100.milli-seconds`).
// Use `yield()` to yield generally to other asynchronous operations.
pub fun duration/wait( secs : duration = zero ) : async ()
  val msecs = max(zero:int64,secs.milli-seconds.int64)
  await-error fn(evloop,cb) timer-setup(evloop,msecs,fn() cb(()))  // ignore errors; we assume timeouts always work
  ()

// Yield to other asynchronous operations. Same as `wait(0)`.
pub fun yield() : async ()
  duration/wait()


// ----------------------------------------------------------------------------
// Interleaved strands of execution
// ----------------------------------------------------------------------------

// Execute `a` and `b` interleaved. As soon as one of them finishes,
// `cancel` the other one and return the result of finished action.
pub fun two/firstof( action1 : () -> <async,ndet|e> a, action2 : () -> <async,ndet|e> a) : <async,ndet|e> a
  list/firstof(action1,[action2])

// Interleave a list of actions `[x]++xs` and return the result of the first action
// that completes (and cancel all others).
pub fun list/firstof( x : () -> <async,ndet|e> a, xs : list<() -> <async,ndet|e> a>) : <async,ndet|e> a
  fun action(f)
    fn()
      val res = f()
      cancel()     // cancel other strands
      res
  Cons(x,xs).map(action).interleaved-raw(False).first-completed-strand
  
// Interleave two actions
pub fun two/interleaved( action1 : () -> <async,ioc|e> a, action2 : () -> <async,ioc|e> b ) : <async,ioc|e> (a,b)
  fun actionL() Left(action1())
  fun actionR() Right(action2())
  match list/interleaved([actionL, actionR])
    [Left(l),Right(r)] -> (l,r)
    other -> impossible("two/interleaved returned " ++ other.length.show ++ " values")

// Interleave a list of actions. If any raises an exception, cancels, or finalizes,
// propagate that and cancel all others.
pub fun list/interleaved(xs : list<() -> <async|e> a> ) : <async|e> list<a>
  xs.interleaved-raw(True).all-completed-strands


// A strand result is either a finalizing yield (exception, cancel, etc), or a completed result value.
type strand-result<a>  
  Canceled
  Finalized(yld : yield-info)     // exception etc.
  Completed(result : a)

// Ensure we prefer Finalizing over Canceled
fun collapse( xs : list<strand-result<a>> ) : strand-result<()>
  match xs
    Cons(Canceled,xx as Cons(Canceled,_)) -> collapse(xx)
    Cons(Canceled,Cons(Finalized(yld),_)) -> Finalized(yld)
    Cons(Canceled,_)                      -> Canceled
    _                                     -> Completed(())

fun partition-strands( strands : list<strand-result<a>> ) : async (list<a>, strand-result<()>)
  val (xs,wrong) = strands.partition-map fn(res) match(res)
                      Completed(x) -> Left(x)
                      _            -> Right(res)
  (xs,wrong.collapse)

// Return strands that completed normally (and ignore the others)
fun any-completed-strands( strands : list<strand-result<a>> ) : <async,ndet> list<a>
  strands.partition-strands.fst

// Check that all strands completed normally or reyield the first finalized one (prefering non canceled ones)
fun all-completed-strands( strands : list<strand-result<a>> ) : async list<a>
  val (xs,wrong) = strands.partition-strands
  match wrong
    Finalized(yld) -> unsafe-reyield(yld)
    Canceled       -> discontinue()
    _              -> xs

// Return the first strand result that is not finalized
fun first-completed-strand( strands : list<strand-result<a>> ) : <async,ndet> a
  val (xs,wrong) = strands.partition-strands
  match xs
    Cons(x,_)  -> x
    _          -> match wrong
                    Finalized(yld) -> unsafe-reyield(yld)
                    Canceled       -> discontinue()
                    _              -> impossible("strands should be a non-empty list")


// Private effect to keep track of when a strand in an interleaving is done.
// Preferred over using built-in state as this works well if there is an outer handler
// over the state that resumes more than once -- redoing part of the interleaving.
// See `test/algeff/async5.js`
effect strands<a>
  // Are any strands canceled?
  fun strands-are-canceled() : bool
  // Are there still strands that need to be resumed?
  fun strands-are-busy() : bool
  // Call this when a strand is done.
  fun strand-done(idx : int, result : strand-result<a>) : ()
  

// Interleave a list of actions by tracking through a local state 
// when each action is done (or finalized)
fun list/interleaved-raw(xs : list<() -> <async|e> a>, needall : bool ) : <async|e> list<strand-result<a>>
  val total = xs.length
  var canceled := False
  var results : some<a> list<(int,strand-result<a>)> := []
  with handler<strands<_>>    
    fun strands-are-canceled() 
      canceled
    fun strands-are-busy()    
      results.length < total      
    fun strand-done(idx,res)
      if res.is-canceled then canceled := True
      results := results.insert(idx,res)    
  mask<local>
    interleave-strands(xs,needall)
  results.map(snd)
  
// Insert in order
tail fun insert( xs : list<(int,a)>, idx : int, value : a, n : int  = 0 ) : list<(int,a)>
  match xs
    Cons(x,xx) | x.fst < idx -> Cons(x, insert(xx, idx, value, n + 1))
    _ -> Cons((idx,value),xs)

inline extern unsafe-no-ndet-cast: forall<a,e> (() -> <ndet|e> a) -> (() -> e a) 
  inline "#1" 

fun unsafe-no-ndet( action : () -> <ndet|e> a ) : e a 
  unsafe-no-ndet-cast(action)()

// Catch a discontinue as a `:maybe`
fun unsafe/uncancel( action : () -> <async|e> a): <async|e> maybe<a>
  with override handler
    final ctl discontinue() Nothing
    return(x)               Just(x)
    // passthrough others
    val async-scope = async-scope
    fun do-await(setup, scope) do-await(setup,scope)
    fun no-await(setup, scope, f) no-await(setup,scope, f)
    fun async-iox(x) async-iox(x)
    fun cancel-scope(scope) cancel-scope(scope)  
  action()

// Returns on an await call and emits the resumption eventually in the given channel
fun emit-on-await(ch : channel<() -> <async|e> ()>, action : () -> <async|e> () ) : <async|e> ()
  with override handler
    raw ctl do-await(setup,scope)
      // turn awaits into no-awaits, allowing other strands to execute
      no-await(setup, scope) fn(res)
        ch.emit-io({ rcontext.resume(res) })
      ()
    // passthrough others
    val async-scope = async-scope
    fun no-await(setup,scope,f) no-await(setup,scope,f)
    fun async-iox(f) async-iox(f)
    fun cancel-scope(scope) cancel-scope(scope)
    fun discontinue() discontinue()
  action()
 
fun interleave-strands(xs : list<() -> <async|e> a>, needall : bool ) : <async,strands<a> |e> ()
  with cancelation-scope             // new cancelation scope
  val ch : some<a,e> channel<() -> <async|e> a> = channel()
  val strands = xs.foreach-indexed fn(i, action)
      if strands-are-canceled() then
        // immediately cancel if a previous strand already called cancel       
        strand-done(i,Canceled) 
      else      
        // otherwise run the action (up to the first async call)
        with emit-on-await(ch)       // redirect async calls to emit resumptions on the channel         
        val res = 
          with unsafe/unfinalize     // catches any final ctl (exceptions)
          with unsafe/uncancel       // catches any discontinue()
          with mask<strands>         
          action()
        match res
          Left(yld)      -> { strand-done(i,Finalized(yld)); if needall then cancel() }
          Right(Nothing) -> strand-done(i,Canceled)
          Right(Just(x)) -> strand-done(i,Completed(x))

  // while we receive resumptions in the channel, invoke them
  with pretend-no-div
  while { strands-are-busy() } // while there are resumptions on the strands..
    val strand-resume = unsafe-no-ndet { ch.receive }   // receive yields back to the outer event loop
    with mask<div> // don't interfere if generic `e` includes `div`
    strand-resume()
  ()  

/*
// Warning: Cancelation should not be caught unless you're carefully maintaining
// cancelation semantics yourself.
pub fun unsafe-catch-cancel(a: () -> <async|e> a): <async|e> await-result<a>
  handle({ mask behind<async>(a) })
    final ctl discontinue()
      Cancel
    return(x)
      Result(x)

    // passthrough others
    val async-scope = async-scope
    fun do-await(setup, scope) do-await(setup,scope)
    fun no-await(setup, scope, f) no-await(setup,scope, f)
    fun async-iox(x) async-iox(x)
    fun cancel-scope(scope) cancel-scope(scope)

fun interleave-strands(xs : list<(scope) -> <async|e> a> ) : <async,strands<a> |e> ()
  val inner-scope: ref<global,maybe<scope-id>> = async-st { ref(Nothing) }
  val keep-spawning: ref<global,bool> = async-st { ref(True) }
  val ch : some<e> channel<() -> <async|e> ()> = channel()

  // interleave has custom cancelation handling. When the scope
  // is canceled, we don't discontinue() but instead
  // perform explicit cancelation of the child scope, and
  // let that flow through into registering all outstanding scopes
  // with a `Cancel` result.
  with unsafe-override-cancel {
    // Note we can't cancel() directly in this hook because cancel is async,
    // but we can add it to the queue of actions to run
    keep-spawning := False
    fun cancel-action(): <async> ()
      async-st { !inner-scope }.foreach(cancel-scope)
    ch.emit-io(cancel-action)
  }

  val sentinel-scope = async-scope

  child-scope-without-auto-cancel fn(cid): <async,strands<_>|_> () // introduce a child scope which doesn't cancel-on-finalize
    async-st { inner-scope.set(Just(cid)) }
    fun yield-on-await(action)
      (handler<async> {
        raw ctl do-await(setup,scope)
          // turn awaits into no-awaits, allowing other strands to execute
          val unique-id = unsafe-total(unique)
          no-await(setup, scope) fn(res)
            ch.emit-io({ rcontext.resume(res) })
          ()

        // passthrough others
        val async-scope = async-scope
        fun no-await(setup,scope,f) no-await(setup,scope,f)
        fun async-iox(f) async-iox(f)
        fun cancel-scope(scope) cancel-scope(scope)
        fun discontinue() discontinue()
      }) {
        with mask behind<async>
        action()
      }

    xs.foreach-indexed fn(i, action)
      if (async-st { !(!keep-spawning) }) then
        // Finalization has occurred synchronously, don't
        // keep spawning actions.
        strand-done(i, Complete(Cancel))
      else
        with yield-on-await
        val res = strand-exec 
          with mask<strands>
          action(sentinel-scope)
        strand-done(i, res)
        if res.is-finalize then
          async-st { keep-spawning := False }
          cancel-scope(cid)
      ()

  // this runs outside of the `child-scope` (unaffected by `cancel-scope(cid)`)
  with pretend-no-div
  while { strands-are-busy() } // while there are resumptions on the strands..
    val strand-resume = unsafe-no-ndet { ch.receive }
    with mask<div> // don't interfere if generic `e` includes `div`
    strand-resume()
  ()
*/

// ----------------------------------------------------------------------------
// Await wrappers
// ----------------------------------------------------------------------------

/*
// Convenience function for awaiting a NodeJS style callback where the first argument is a possible exception.
pub fun null/await-exn0( setup : (cb : (null<exception>) -> ioc () ) -> ioc error<dispose-fn> ) : async-exn ()
  setup/await-exn fn(evloop,cb)
    setup( fn(nexn) cb(nexn.unnull(())) )

// Convenience function for awaiting a NodeJS style callback where the first argument is a possible exception
// and the second argument the possible result value.
pub fun null/await-exn1( setup : (cb : (null<exception>,a) -> ioc () ) -> ioc error<dispose-fn> ) : async-exn a
  setup/await-exn fn(evloop,cb)
    setup( fn(nexn,x) cb(Result(nexn.unnull(x))) )

fun unnull( nexn : null<exception>, x : a  ) : error<a>
  match nexn.maybe
    Nothing -> Ok(x)
    Just(exn) -> Error(exn)
*/

// Convenience function for awaiting a zero argument callback that never fails
fun await0-noexn( setup : (cb : () -> ioc () ) -> ioc () ) : async ()
  await1-noexn( fn(cb) setup(fn() cb(())) )

// Convenience function for awaiting a single argument callback that never fails
fun await1-noexn( setup : (cb : (a) -> ioc () ) -> ioc () ) : async a
  await-noexn fn(_evloop,cb){ setup(cb); (fn() {}) }
  
// Convenience function for async functions where the setup cannot fail. In that case
// the result will never be an error.
fun await-noexn( setup : (evloop : event-loop, cb : (a) -> ioc ()) -> ioc dispose-fn ) : async a  
  val res = setup/await-error( fn(evloop,cb) setup(evloop,cb).Ok )   
  match res
    Ok(x)      -> x
    Error(exn) -> impossible("unexpected exception returned from async await: " ++ show(exn))


// Convenience function for awaiting a zero argument callback.
pub fun await0( setup : (cb : () -> ioc () ) -> ioc error<dispose-fn> ) : async-exn ()
  await1( fn(cb) setup(fn() cb(())) )

// Convenience function for awaiting a single argument callback.
pub fun await1( setup : (cb : (a) -> ioc () ) -> ioc error<dispose-fn> ) : async-exn a
  setup/await(fn(_evloop,cb) setup(cb))

// Convenience function for awaiting with the exception effect
pub fun setup/await( setup : (evloop : event-loop, cb : (a) -> ioc ()) -> ioc error<dispose-fn> ) : async-exn a  
  await-error(setup).untry

// Primitive: Execute `setup` to set up an asynchronous callback with the host platform. Invoke `cb` as the callback:
// it takes either an exception or a result `a`. 
// The `setup` returns `dispose` funcion that is invoked on cancellation to dispose of any 
// resources (see the implementation of `wait`).
// The callback should be invoked exactly once -- when that happens `await-exn` is resumed with the result.
pub fun setup/await-error( setup : (evloop : event-loop, cb : (a) -> ioc ()) -> ioc error<dispose-fn> ) : async error<a>
  val result = do-await(fn(evloop,cb) { setup(evloop, fn(res) cb(Result(res),True)) }, async-scope)
  result.continue-await

// // Primitive: Execute `setup` to set up an asynchronous callback with the host platform. Invoke `cb` as the callback: it takes either
// // an exception or a result value, together with boolean parameter whether the callback is done.
// // The callback `cb` will eventually emit the result into the given channel `ch` after applying the transformation `f` to the result.\
// // Note: once you exit the `child-scope` scope where `await-to-channel` was called, the callback is invoked with a `Cancel` exception.
// // The channel should always be awaited within the same `child-scope` scope as the `await-to-channel` invokation.
// pub fun await-to-channel( setup : (cb : (a,bool) -> ioc ()) -> ioc (maybe<() -> ioc ()>), ch : channel<b>, f : a -> b) : async channel<b>
//   fun wrapped-setup(cb: (await-result<_>,bool) -> ioc ())
//     setup fn (value, b)
//       cb(Result(value), b)

//   no-await(wrapped-setup, async-scope) fn(res)
//     ch.emit-io(f(res.continue-await))
//   ch

fun async-ioc( f : () -> ioc a ) : async a
  async-iox(f)

// ----------------------------------------------------------------------------
// Async handlers: cancelation scope
// ----------------------------------------------------------------------------

// Set up a new cancelation scope
fun cancelation-scope( action : () -> <async|e> a) : <async|e> a
  val cid = async-iox{ unique() }
  with override handler
    val async-scope = async-scope.child(cid)
    // passthrough
    fun do-await(setup,scope)   do-await(setup,scope)
    fun no-await(setup,scope,f) no-await(setup,scope,f)
    fun cancel-scope(scope)     cancel-scope(scope)
    fun async-iox(f)            async-iox(f)
    fun discontinue()           discontinue()
  with finally
    cancel-scope(async-scope)   // ensure there are no more outstanding async operations when leaving the scope
  action()

/*
// Execute `action` in a child-scope scope. Once `action` is complete
// (successfully or otherwise), all outstanding actions
// within this scope are canceled.
pub fun child-scope( action : () -> <async|e> a ) : <async|e> a
  child-scope-without-auto-cancel fn(cid)
    with finally
      // cancel any outstanding operations still in our scope.
      // this might be needed for `no-await` operations.
      cancel-scope(cid)
    action()

// like child-scope, but the caller is responsible
// for canceling the created scope
fun child-scope-without-auto-cancel(action : (int) -> <async|e> a ) : <async|e> a
  val cid = async-iox{ unique() }
  handle ({mask behind<async> { action(cid) }})
    val async-scope = async-scope.child(cid)
    fun do-await(setup,scope) do-await(setup,scope)
    fun no-await(setup,scope,f) no-await(setup,scope,f)
    fun cancel-scope(scope) cancel-scope(scope)
    fun async-iox(f) async-iox(f)
    fun discontinue() discontinue()
*/

// // ----------------------------------------------------------------------------
// // Async handle
// // ----------------------------------------------------------------------------

fun noop(): ()
   ()

alias event-loop = any

// The outer `:async` effect handler. This is automatically applied by the compiler
// around the `main` function if it has an `:async` effect.
pub fun async/handle(evloop : event-loop, action : () -> <async,ioc> a ) : ioc ()
  val cancel-functions : ref<global,list<(scope,() -> <ioc|_> ())>> = ref([])
  fun handle-await( setup : await-setup<a>, scope : scope, f : (await-result<a>) -> ioc ()) : ioc ()
    // Each callback gets its own scope so we can tell when a given
    // callback can be removed from `cancel-functions`
    val cscope = scope.child(unique())

    // `cancel-hook` is the event-system action which should happen
    // on cancellation, e.g. clearTimeout(...)
    val cancel-hook: ref<global,dispose-fn> = ref(noop)

    // TODO is there any code path where is-done is False?
    fun cb( res : await-result<_>, is-done : bool ): ioc ()
      fun cleanup()
        if is-done && ((!cancel-functions).contains(cscope)) then
          cancel-functions := (!cancel-functions).remove(cscope)
          // Take ownership of the disposer and reset the ref to noop so
          // that, after we're done with `hook`, its captured resources
          // (e.g. a timer's `tid`) can drop. On natural completion we
          // just drop the disposer; on cancellation we invoke it first to
          // actively release the underlying operation, then drop it.
          val hook = !cancel-hook
          cancel-hook := noop
          if res.is-cancel then hook()
      cleanup()
      f(res)

    val trigger-cancel: () -> ioc () = fn() cb(Cancel, True)
    cancel-functions := Cons((cscope, trigger-cancel), !cancel-functions)
    match(setup(evloop,cb))
      Ok(d)      -> cancel-hook := d
      Error(exn) -> cb(Exception(exn),True)  // if setup fails, we call the callback immediately with an exception result

  fun handle-cancel( sid : scope-id ) : ioc ()
    // TODO what happens if we try to cancel more than once?
    (!cancel-functions).foreach fn((scope,trigger-cancel))
      if sid.in-scope-of(scope) then
        trigger-cancel()

  with handler<async>
    val async-scope = empty-scope
    raw ctl do-await(setup, scope)
      handle-await(setup, scope, fn(x) { rcontext.resume(x); () })
    fun no-await(setup, scope, f)
      handle-await(setup, scope, f)
    fun cancel-scope(scope)
      handle-cancel(scope.scope-id)
    final ctl discontinue()
      impossible("discontinue occurred at the toplevel scope")
    fun async-iox( f )
      f()

  action()
  ()

pub fun handle-evloop( action : () -> <async,ioc> () ) : ioc () 
  val eres = ref(Ok(()))
  val evloop = event-loop-init()
  async/handle(evloop)
    eres := try(action)
  event-loop-run(evloop)
  // (!eres).untry

pub fun @default-async(action)
  handle-evloop(action)


// ----------------------------------------------------------------------------
// Scope identifiers
// ----------------------------------------------------------------------------

// A scope has an ID, plus a list of child IDs.
// Every created scope has a unique ID
alias scope-id = int

// An abstract cancelation scope
abstract value struct scope( scopes : list<scope-id> )

// A scope is the chain starting with the current scope, including parent scopes up to the root scope-id
val empty-scope = Scope([])

fun child(parent : scope, child-id : scope-id) : scope
  match parent
    Scope(ids) -> Scope(Cons(child-id, ids))

fun scope-id( Scope(ids) ) : scope-id
  match ids
    (Cons(id,_)) -> id
    Nil          -> 0
    

fun in-scope-of(childid : scope-id, parent : scope ) : bool
  match parent
    Scope(ids) -> ids.find(fn(x) x == childid).is-just

// every scope with the same id must implicitly contain the same
// parent chain, so a simple comparison on the head is sufficient
pub fun scope/(==)(s1 : scope, s2 : scope ) : bool
  s1.scope-id == s2.scope-id

// Convenience functions for scope maps
fun remove( xs : list<(scope,a)>, scope : scope ) : list<(scope,a)>
  xs.remove( fn(x:(scope,_)) { x.fst == scope })

fun lookup( xs : list<(scope,a)>, scope : scope ) : maybe<a>
  xs.lookup( fn(x:scope) { x == scope })

fun contains( xs : list<(scope,a)>, scope : scope ) : bool
  xs.lookup(scope).bool

pub fun scope/show( s : scope ) : string
  match s
    Scope(ids) -> " " ++ ids.map(fn(id) "{" ++ id.show ++ "}").join("<") ++ "<"


