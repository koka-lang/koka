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
import std/num/int32
import std/num/ddouble    // for C# backend
import std/time/duration
import std/async/null
import std/core/unsafe
import std/core/undiv
pub import std/num/ddouble
pub import std/num/float64
import std/time/timestamp

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
// **Note**: technically, async is capable of embedding arbitrary `io-noexn`
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
  ctl no-await( setup : await-setup<a>, scope : scope, f : await-result<a> -> io-noexn ()  ) : ()

  // Internal: evaluate io in async
  ctl async-iox( action : () -> io-noexn a ) : a

  // trigger cancelation of a specific scope (and all its children)
  ctl cancel-scope( scope : scope-id ) : ()

  // React to cancelation of this expression
  final ctl discontinue(): a

// Backwards compatibility, no longer needed
// pub alias asyncx = <async,exn,ndet>
pub alias asyncx = <async>

pub alias async-exn = <async,exn>

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

fun async-st(action: () -> st<global> a): async a
  async-iox(action)

// ----------------------------------------------------------------------------
// Async effect
// ----------------------------------------------------------------------------
pub type await-result<a>
  Cancel
  Result(value: a)

fun await-result/show(r: await-result<a>)
  match r
    Cancel -> "Cancel"
    Result(_) -> "Result(_)"

type strand-result<a>
  Finalize(y: yield-info)
  Complete(value: a)
  
fun strand-result/show(s: strand-result<a>)
  match s
    Finalize(_) -> "Finalize(...)"
    Complete(_) -> "Complete(_)"

fun continue-await(v: await-result<a>): async a
  match v
    Result(value) -> value
    Cancel -> discontinue()

fun continue-strand(v: strand-result<a>): total a
  match v
    Complete(value) -> value
    Finalize(yield-info) -> unsafe-reyield(yield-info)

fun finalize-strand(v: strand-result<a>): total ()
  match v
    Complete(_) -> ()
    Finalize(yield-info) -> unsafe-reyield(yield-info)

alias dispose-fn = () -> io-noexn ()

alias maybe-dispose-fn = (maybe<dispose-fn>)

alias await-setup<a> = (cb : (await-result<a>, bool) -> io-noexn ()) -> io-noexn maybe-dispose-fn

// Prevents cancelation, but invokes a hook immediately for notification
fun unsafe-override-cancel(on-cancel-hook: () -> <io-noexn> (), action: () -> <async|e> a): <async|e> a
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
  fun setup-cb(cb: ((await-result<()>, bool) -> io-noexn ())): io-noexn maybe-dispose-fn
    dangling-cb.set(Just(cb))
    return Just(on-cancel-hook) // cleanup fn
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
        async-iox(on-cancel-hook)
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
  fun setup(cb : _ -> io-noexn ()): io-noexn ()
    val r = p.state
    match (!r)
      Awaiting(listeners) -> r := Awaiting(Cons(cb,listeners))
      Resolved(value) -> cb(value) // resume right away; should not happen due to try-await
  match p.try-await
    Just(v) -> v
    Nothing -> await1(setup)

// Returns immediately if the promise was already resolved and otherwise return `Nothing`.
pub fun try-await( p : promise<a> ) : <async> maybe<a>
  async-io-noexn
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
// TODO: make this io-noexn
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
  async-io-noexn( { try(f) } ).untry

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
  Waiting( listener : a -> io-noexn (), listeners : list<a -> io-noexn ()> = [] )

fun from-values(values : list<a> ) : channel-state<a>
  match values
    Nil -> Empty
    Cons(v,vs)  -> Values(v,vs)

fun from-waiting(listeners : list<a -> io-noexn ()>) : channel-state<a>
  match listeners
    Nil -> Empty
    Cons(l,ls)  -> Waiting(l,ls)

// Create a new asynchronous channel.
pub fun channel() : async channel<a>
  async-iox
    Channel(unique(), ref(Empty))

// Receive (and remove) a value from the channel: returns immediately if a value is available and otherwise
// waits asynchronously until a value becomes available.
fun receive( ch : channel<a>) : <async,div,ndet> a
  val setup: await-setup<_> = fn(cb : (_,_) -> io-noexn () )
    fun cbr(x) cb(Result(x),True)
    val r = ch.state
    match !r
      Empty -> r := Waiting(cbr,[])
      Waiting(l,ls) -> r := Waiting(l,ls ++ [cbr])
      Values(v,vs)->   // this case should not happen due to `try-receive`
        r := from-values(vs)
        cbr(v)
    Nothing

  match ch.try-receive
    Just(v) -> v
    Nothing -> do-await(setup,async-scope).continue-await

// Return immediately if a value is available on the channel and otherwise returns `Nothing`.
pub fun try-receive( ch : channel<a> ) : <async,ndet> maybe<a>
  async-io-noexn
    val r = ch.state
    match (!r)
      Values(v, vs) ->
        r := from-values(vs)
        Just(v)
      _ -> Nothing

fun emit-io( ch : channel<a>, value : a ) : io-noexn ()
  val r = ch.state
  match !r
    Empty -> r := Values(value, [])
    Values(v,vs)  -> r := Values(v,vs ++ [value])
    Waiting(l,ls) ->
      r := from-waiting(ls)
      l(value)

// Emit a value asynchronously into a channel.
pub fun emit( ch : channel<a>, value : a ) : <async,div> ()
  async-io-noexn
    emit-io(ch,value)

fun trace-channel( msg : string, ch : channel<a> ) : <async,ndet> ()
  async-io-noexn
    trace-channel-io( msg, ch )

fun trace-channel-io( msg : string, ch : channel<a> ) : io-noexn ()
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
pub fun timeout( secs : duration, action : () -> <async,io-noexn|e> a, ?set-timeout: (unit-cb, int32) -> io-noexn any, ?clear-timeout: (any) -> io-noexn () ) : <async,io-noexn|e> maybe<a>
  firstof { duration/wait(secs); Nothing} { Just(action()) }

// Execute `a` and `b` interleaved. As soon as one of them finishes,
// `cancel` the other one and return the result of the first.
pub fun firstof( a : () -> <async,ndet|e> a, b : () -> <async,ndet|e> a) : <async,ndet|e> a
  fun runner(f)
    fn(scope)
      with finally { scope.cancel() } // cancel regardless of exn or result
      mask<local> { f() }

  // note: when we cancel `scope`, interleaved-raw returns Cancel items,
  // rather than actually canceling the entire call
  val results = interleaved-raw([runner(a), runner(b)])
  val first = results.find-maybe fn(result)
    match result
      Result(x) -> Just(x)
      Cancel -> Nothing
  match first
    Just(x) -> x
    Nothing -> discontinue()

// Wait (asynchronously) for `secs` seconds as a `:double`.
// Use `yield()` to yield to other asynchronous operations.
pub fun float/wait( secs : float64, ?set-timeout: (unit-cb, int32) -> io-noexn any, ?clear-timeout: (any) -> io-noexn () ) : async ()
  wait(secs.duration)

// Wait (asynchronously) for optional `secs` seconds `:duration` (`= 0.seconds`).
// Use `yield()` to yield generally to other asynchronous operations.
pub fun duration/wait( secs : duration = zero, ?set-timeout: (unit-cb, int32) -> io-noexn any, ?clear-timeout: (any) -> io-noexn () ) : async ()
  if secs <= duration/zero then return yield()
  val msecs = max(zero:int32,secs.milli-seconds.int32)
  await fn(cb)
    val tid = async/set-timeout( fn(){ cb(()) }, msecs )
    Just( { async/clear-timeout(tid) } )

// Yield to other asynchronous operations. Same as `wait(0)`.
pub fun yield(?set-timeout: (unit-cb, int32) -> io-noexn any) : async ()
  await0 fn(cb)
    async/set-timeout( cb, int32/zero )
    ()

// abstract wid for timeout handlers
abstract struct timeout-id(
  timer : any
)

alias unit-cb = () -> io-noexn ()

fun async/set-timeout( cb : unit-cb, ms : int32, ?set-timeout: (unit-cb, int32) -> io-noexn any) : io-noexn timeout-id
  Timeout-id(?set-timeout(cb,max(ms,zero)))


fun async/clear-timeout( tid : timeout-id , ?clear-timeout: (any) -> io-noexn ()) : io-noexn ()
  ?clear-timeout(tid.timer)

// ----------------------------------------------------------------------------
// Interleaved strands of execution
// ----------------------------------------------------------------------------

// Interleave two actions around their asynchronous operations.
pub fun two/interleaved( action1 : () -> <async,io-noexn|e> a, action2 : () -> <async,io-noexn|e> b ) : <async,io-noexn|e> (a,b)
  fun act1() Left(action1())
  fun act2() Right(action2())
  match interleaved([act1, act2])
    [Left(l), Right(r)] -> (l, r)
    other -> impossible("two/interleaved returned " ++ other.length.show ++ " values")

value struct interleaving<a>
  incomplete: int
  results: list<(int, strand-result<await-result<a>>)>

fun interleaving/show(i: interleaving<_>)
  "Interleaving(incomplete=" ++ i.incomplete.show ++ ", " ++ i.results.show ++ ")"

pub fun list/interleaved(xs : list<() -> <async|e> a> ) : <async|e> list<a>
  fun runner(f)
    fn(_)
      f()
  xs.map(runner).interleaved-raw.map fn(result)
    result.continue-await

fun list/interleaved-raw(xs : list<(scope) -> <async|e> a> ) : <async|e> list<await-result<a>>
  var state : some<a> interleaving<a> := Interleaving(xs.length, [])
  with handler<strands<_>>
    finally()
      state.results.foreach fn(pair)
        pair.snd.finalize-strand

    return(_)
      // Treat finalized as cancels, since we're already returning await-result.
      state.results.map fn(pair)
        match pair.snd
          Finalize(_) -> Cancel
          Complete(x) -> x

    fun strands-are-busy()
      state.incomplete > 0

    fun strand-done(idx,res)
      state := Interleaving(state.incomplete - 1, state.results.insert(idx,res))

  // with unsafe-pretend-stateless
  with mask<local>
  interleave-strands(xs)

// Private effect to keep track of when a strand in an interleaving is done.
// Preferred over using built-in state as this works well if there is an outer handler
// over the state that resumes more than once -- redoing part of the interleaving.
// See `test/algeff/async5.js`
effect strands<a>
  // Are there still strands that need to be resumed?
  fun strands-are-busy() : bool

  // Call this when a strand is done.
  fun strand-done(idx : int, result : strand-result<await-result<a>>) : ()

// Insert in order with an accumulating list.
fun insert-acc( xs : list<(int,a)>, idx : int, value : a, acc : list<(int,a)> ) : list<(int,a)>
  match xs
    Cons(x,xx) | x.fst < idx -> insert-acc(xx, idx, value, Cons(x,acc))
    _ -> reverse-append( acc, Cons((idx,value),xs) )

// Insert in order
fun insert( xs : list<(int,a)>, idx : int, value : a, n : int  = 0 ) : list<(int,a)>
  if n > 100
    then insert-acc( xs, idx, value, [] )
    else match xs
      Cons(x,xx) | x.fst < idx -> Cons(x, insert(xx, idx, value, n + 1))
      _ -> Cons((idx,value),xs)

inline extern unsafe-no-ndet-div-cast : forall<a,e> (() -> <ndet,div|e> a) -> (() -> e a) 
  inline "#1" 

fun unsafe-no-ndet-div( action : () -> <ndet,div|e> a ) : e a 
  unsafe-no-ndet-div-cast(action)()

inline extern unsafe-no-ndet-cast: forall<a,e> (() -> <ndet|e> a) -> (() -> e a) 
  inline "#1" 

fun unsafe-no-ndet( action : () -> <ndet|e> a ) : e a 
  unsafe-no-ndet-cast(action)()

inline extern inject-strand-effect : forall<a,h,e> (() -> e a) -> total (() -> <strands<a>|e> a)
  inline "#1"

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

// TODO is there a better way to inform the caller of scope than having every item in the list accept it?
fun interleave-strands(xs : list<(scope) -> <async|e> a> ) : <async,strands<a>|e> ()
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
        val res = strand-exec(inject-strand-effect { action(sentinel-scope) })
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

// ----------------------------------------------------------------------------
// Await wrappers
// ----------------------------------------------------------------------------

// Convenience function for awaiting a NodeJS style callback where the first argument is a possible exception.
pub fun await-exn0( setup : (cb : (null<exception>) -> io-noexn () ) -> io-noexn maybe<() -> io-noexn ()> ) : async-exn ()
  setup/await fn(cb)
    setup( fn(nexn) cb(Result(nexn.unnull(()))) )
  .continue-await.untry

// Convenience function for awaiting a NodeJS style callback where the first argument is a possible exception
// and the second argument the possible result value.
pub fun await-exn1( setup : (cb : (null<exception>,a) -> io-noexn () ) -> io-noexn maybe<() -> io-noexn ()> ) : async-exn a
  setup/await fn(cb)
    setup( fn(nexn,x) cb(Result(nexn.unnull(x))) )
  .continue-await.untry

fun unnull( nexn : null<exception>, x : a  ) : error<a>
  match nexn.maybe
    Nothing -> Ok(x)
    Just(exn) -> Error(exn)

// Convenience function for awaiting a zero argument callback.
pub fun await0( setup : (cb : () -> io-noexn () ) -> io-noexn () ) : async ()
  setup/await fn(cb)
    setup( fn() cb(Result(())) )
    Nothing
  .continue-await

// // Convenience function for awaiting a single argument callback.
pub fun await1( setup : (cb : (a) -> io-noexn () ) -> io-noexn () ) : async a
  setup/await fn(cb)
    setup( fn(x) cb(Result(x)) )
    Nothing
  .continue-await


// Primitive: Execute `setup` to set up an asynchronous callback with the host platform. Invoke `cb` as the callback:
// it takes either an exception or a result `a`. Usually `setup` returns `Nothing` but you can return a `Just(cleanup)`
// value where the `cleanup` functions is invoked on cancellation to dispose of any resources (see the implementation of `wait`).
// The callback should be invoked exactly once -- when that happens `await-exn` is resumed with the result.
pub fun setup/await( setup : (cb : (a) -> io-noexn ()) -> io-noexn (maybe<() -> io-noexn ()>) ) : async a
  val result = do-await(fn(cb) { setup(fn(res) cb(Result(res),True)) }, async-scope)
  result.continue-await

// // Primitive: Execute `setup` to set up an asynchronous callback with the host platform. Invoke `cb` as the callback: it takes either
// // an exception or a result value, together with boolean parameter whether the callback is done.
// // The callback `cb` will eventually emit the result into the given channel `ch` after applying the transformation `f` to the result.\
// // Note: once you exit the `child-scope` scope where `await-to-channel` was called, the callback is invoked with a `Cancel` exception.
// // The channel should always be awaited within the same `child-scope` scope as the `await-to-channel` invokation.
// pub fun await-to-channel( setup : (cb : (a,bool) -> io-noexn ()) -> io-noexn (maybe<() -> io-noexn ()>), ch : channel<b>, f : a -> b) : async channel<b>
//   fun wrapped-setup(cb: (await-result<_>,bool) -> io-noexn ())
//     setup fn (value, b)
//       cb(Result(value), b)

//   no-await(wrapped-setup, async-scope) fn(res)
//     ch.emit-io(f(res.continue-await))
//   ch

fun async-io-noexn( f : () -> io-noexn a ) : <async> a
  async-iox(f)

// // ----------------------------------------------------------------------------
// // Async handlers: child-scope
// // ----------------------------------------------------------------------------

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


// // ----------------------------------------------------------------------------
// // Async handle
// // ----------------------------------------------------------------------------

pub fun @default-async(action)
  async/handle(action)

fun noop(): ()
   ()

// The outer `:async` effect handler. This is automatically applied by the compiler
// around the `main` function if it has an `:async` effect.
pub fun async/handle(action : () -> <async,io-noexn> a ) : io-noexn ()
  val cancel-functions : ref<global,list<(scope,() -> <io-noexn|_> ())>> = ref([])
  fun handle-await( setup : await-setup<a>, scope : scope, f : await-result<a> -> io-noexn ()) : io-noexn ()
    // Each callback gets its own scope so we can tell when a given
    // callback can be removed from `cancel-functions`
    val cscope = scope.child(unique())
    
    // `cancel-hook` is the event-system action which should happen
    // on cancellation, e.g. clearTimeout(...)
    val cancel-hook: ref<global, dispose-fn> = ref(noop)

    // TODO is there any code path where is-done is False?
    fun cb( res : await-result<_>, is-done : bool ): io-noexn ()
      fun cleanup()
        if is-done && ((!cancel-functions).contains(cscope)) then
          cancel-functions := (!cancel-functions).remove(cscope)
          if res.is-cancel then
            (!cancel-hook)()
      cleanup()
      f(res)

    val trigger-cancel: () -> io-noexn () = fn() cb(Cancel, True)
    cancel-functions := Cons((cscope, trigger-cancel), !cancel-functions)
    match(setup(cb))
      Just(d) -> cancel-hook := d
      Nothing -> ()

  fun handle-cancel( scope-id) : io-noexn ()
    // TODO what happens if we try to cancel more than once?
    (!cancel-functions).foreach fn(entry)
      val (scope,trigger-cancel) = entry
      if (scope.is-in-scope(scope-id)) then
        trigger-cancel()

  with handler<async>
    val async-scope = empty-scope
    raw ctl do-await(setup, scope)
      handle-await(setup, scope, fn(x) { rcontext.resume(x); () })
    fun no-await(setup, scope, f)
      handle-await(setup, scope, f)
    fun cancel-scope(scope)
      handle-cancel(scope)
    final ctl discontinue()
      impossible("discontinue occurred at the toplevel scope")

    fun async-iox( f )
      f()

  action()
  ()

// ----------------------------------------------------------------------------
// Scope identifiers
// ----------------------------------------------------------------------------

// A scope has an ID, plus a list of child IDs.
// Every created scope has a unique ID
alias scope-id = int
abstract value struct scope( : list<scope-id> )

// A scope is the chain starting with the current scope, including parent scopes up to the root scope ID
// A scope is "in" a given scope-id if that scope-id appears in its id list
val empty-scope = Scope([])

fun child(parent : scope, child-id: scope-id) : scope
  match parent
    Scope(ids) -> Scope(Cons(child-id, ids))

fun is-in-scope(child : scope, parent : scope-id ) : bool
  match child
    Scope(ids) -> ids.find(fn(x) x == parent).is-just

// every scope with the same ID must implicitly contain the same
// parent chain, so a simple comparison on the head is sufficient
pub fun scope/(==)(scope1 : scope, scope2 : scope ) : bool
  match scope1
    Scope(ids1) -> match scope2
      Scope(ids2) -> maybe/(==)(ids1.head, ids2.head, ?(==) = int/(==))

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

// TODO make private?
pub fun scope/cancel(s: scope)
  match s
    Scope(Cons(scope-id, _)) -> cancel-scope(scope-id)

    // TODO: make it impossible by types to have an empty scope?
    _ -> ()

// execute an action, but observe whether it completed normally or
// needs to instead run a finalizer (due to cancellation, exception, etc)
fun strand-exec( action : () -> <async|e> a ): <async|e> strand-result<await-result<a>>
  match unsafe-try-finalize({ unsafe-catch-cancel(action) })
    Left(fin) -> Finalize(fin)
    Right(v) -> Complete(v)