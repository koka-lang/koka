/*---------------------------------------------------------------------------
  Copyright 2012-2021, Microsoft Research, Daan Leijen.

  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the LICENSE file at the root of this distribution.
---------------------------------------------------------------------------*/

/* Asynchronous primitives

This module is based closely on [@Leijen:async] and aims to have robust and composable asynchronous
primitives. In particular, any outstanding asynchronous operation can be canceled (through `cancel-outstanding`)
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

import std/num/int64
import std/core/undiv
import std/core/unsafe
import std/num/float64
import std/time/duration

import uv/core
import uv/timer

/*
# Cancellation scopes

Each scope has a unique id.
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


// ----------------------------------------------------------------------------
// Async effect
// ---------------------------------------------------------------------------- 

// Asynchronous operations have the `:async` effect.
// Usually there is only one handler for asynchronous operations at
// the `main` function using the default `handle-evloop` (inserted
// automatically if `main` has the `:async` effect).
pub effect async
  // The current cancelations scope.
  val async-scope: scope

  // Setup a callback and await its completion. The setup is performed in the handler scope.
  ctl do-await( setup : await-setup<a>, scope : scope) : await-result<a>
  
  // Internal: Setup a callback to execute `f` on completion, but return control to the caller immediately
  // after the setup is performed. `f` is performed in the handler scope.
  ctl no-await( setup : await-setup<a>, scope : scope, f : await-result<a> -> ioc ()  ) : ()

  // Internal: perform an I/O operation in the outer async handler
  ctl async-ioc( action : () -> ioc a ) : a

  // Cancel any outstanding async operations of a specific cancelation scope (and all its children)
  ctl cancel-scope( scope : scope ) : ()

  // The current strand of execution is canceled
  final ctl discontinue(): a

// Cancel any _other_ outstanding operations that are interleaved and
// part of the same cancelation scope (see `cancelation-scope`).
pub fun cancel-outstanding()
  cancel-scope(async-scope)

// Perform a stateful operation in the outer async handler.
pub fun async-st(action: () -> st<global> a): async a
  async-ioc(action)

// Function that disposes any resources for an await call (i.e. `clearTimeout`)
pub alias dispose-fn = () -> ioc ()

// A function that will setup a primitive await. It gets passed the current event loop
// and a resumption function, and returns either an error or a dispose function.
// The resumption gets the `:await-result` (cancel, exception, or regular result) and
// whether this is the last time the resumption is called (`isdone`).
pub alias await-setup<a> = (evloop : event-loop, resume : (result:await-result<a>, isdone:bool) -> ioc ()) -> ioc error<dispose-fn>

// An await result is either canceled, an exception, or a regular return value.
pub type await-result<a>
  Cancel
  Exception( exn: exception )
  Result( value: a )

pub fun await-result/show(r: await-result<a>) : string
  match r
    Cancel -> "Cancel"
    Exception(exn) -> "Exception(" ++ show(exn) ++ ")"
    Result(_) -> "Result(_)"


// ----------------------------------------------------------------------------
// Asynchronous timeout and waiting
// ----------------------------------------------------------------------------

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
// Await wrappers
// ----------------------------------------------------------------------------

// Convenience function for awaiting a zero argument callback that never fails
fun await0-noexn( setup : (resume : () -> ioc () ) -> ioc () ) : async ()
  await1-noexn( fn(resume) setup(fn() resume(())) )

// Convenience function for awaiting a single argument callback that never fails
pub fun await1-noexn( setup : (resume : (a) -> ioc () ) -> ioc () ) : async a
  await-noexn fn(_evloop,resume){ setup(resume); (fn() {}) }
  
// Convenience function for async functions where the setup cannot fail. In that case
// the result will never be an error.
fun await-noexn( setup : (evloop : event-loop, resume : (a) -> ioc ()) -> ioc dispose-fn ) : async a  
  val res = setup/await-error( fn(evloop,resume) setup(evloop,resume).Ok )   
  match res
    Ok(x)      -> x
    Error(exn) -> impossible("unexpected exception returned from async await: " ++ show(exn))


// Convenience function for awaiting a zero argument callback.
pub fun await0( setup : (resume : () -> ioc () ) -> ioc error<dispose-fn> ) : <async,exn> ()
  await1( fn(resume) setup(fn() resume(())) )

// Convenience function for awaiting a single argument callback.
pub fun await1( setup : (resume : (a) -> ioc () ) -> ioc error<dispose-fn> ) : <async,exn> a
  setup/await(fn(_evloop,resume) setup(resume))

// Convenience function for awaiting with the exception effect
pub fun setup/await( setup : (evloop : event-loop, resume : (a) -> ioc ()) -> ioc error<dispose-fn> ) : <async,exn> a  
  await-error(setup).untry

// Primitive: Execute `setup` to set up an asynchronous callback with the host platform. Invoke `cb` as the callback:
// it takes either an exception or a result `a`. 
// The `setup` returns `dispose` funcion that is invoked on cancellation to dispose of any 
// resources (see the implementation of `wait`).
// The callback should be invoked exactly once -- when that happens `await-exn` is resumed with the result.
pub fun setup/await-error( setup : (evloop : event-loop, resume : (a) -> ioc ()) -> ioc error<dispose-fn> ) : async error<a>
  val res = do-await(fn(evloop,resume) { setup(evloop, fn(res) resume(Result(res),True)) }, async-scope)
  match res
    Result(value)  -> Ok(value)
    Exception(exn) -> Error(exn)
    Cancel         -> discontinue()


// ----------------------------------------------------------------------------
// Async handlers: cancelation scope
// ----------------------------------------------------------------------------

// Set up a new cancelation scope
pub fun cancelation-scope( action : () -> <async|e> a) : <async|e> a
  val cid = async-ioc{ unique() }
  with override handler
    val async-scope = async-scope.child(cid)
    // passthrough
    fun do-await(setup,scope)   do-await(setup,scope)
    fun no-await(setup,scope,f) no-await(setup,scope,f)
    fun cancel-scope(scope)     cancel-scope(scope)
    fun async-ioc(f)            async-ioc(f)
    fun discontinue()           discontinue()
  with finally
    cancel-scope(async-scope)   // ensure there are no more outstanding async operations when leaving the scope
  action()


// // ----------------------------------------------------------------------------
// // Async handle
// // ----------------------------------------------------------------------------

fun noop(): ()
   ()

pub alias event-loop = any

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
    fun async-ioc( f )
      f()

  action()
  ()

pub fun handle-evloop( action : () -> <async,ioc> () ) : ioc () 
  val evloop = event-loop-init()
  async/handle(evloop,action)
  event-loop-run(evloop)
  
pub fun @default-async( action : () -> <async,ioc> () ) : ioc () 
  handle-evloop(action)


// ----------------------------------------------------------------------------
// Scope identifiers
// ----------------------------------------------------------------------------

// A scope has an ID, plus a list of child IDs.
// Every created scope has a unique ID
alias scope-id = int

// An abstract cancelation scope
abstract value struct scope( scopes : list<scope-id> )

pub fun scope/show( s : scope ) : string
  match s
    Scope(ids) -> " " ++ ids.map(fn(id) "{" ++ id.show ++ "}").join("<") ++ "<"

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



/*
// Convenience function for awaiting a NodeJS style callback where the first argument is a possible exception.
pub fun null/await-exn0( setup : (cb : (null<exception>) -> ioc () ) -> ioc error<dispose-fn> ) : <async,exn> ()
  setup/await-exn fn(evloop,cb)
    setup( fn(nexn) cb(nexn.unnull(())) )

// Convenience function for awaiting a NodeJS style callback where the first argument is a possible exception
// and the second argument the possible result value.
pub fun null/await-exn1( setup : (cb : (null<exception>,a) -> ioc () ) -> ioc error<dispose-fn> ) : <async,exn> a
  setup/await-exn fn(evloop,cb)
    setup( fn(nexn,x) cb(Result(nexn.unnull(x))) )

fun unnull( nexn : null<exception>, x : a  ) : error<a>
  match nexn.maybe
    Nothing -> Ok(x)
    Just(exn) -> Error(exn)

// Primitive: Execute `setup` to set up an asynchronous callback with the host platform. Invoke `cb` as the callback: it takes either
// an exception or a result value, together with boolean parameter whether the callback is done.
// The callback `cb` will eventually emit the result into the given channel `ch` after applying the transformation `f` to the result.\
// Note: once you exit the `child-scope` scope where `await-to-channel` was called, the callback is invoked with a `Cancel` exception.
// The channel should always be awaited within the same `child-scope` scope as the `await-to-channel` invokation.
pub fun await-to-channel( setup : (cb : (a,bool) -> ioc ()) -> ioc (maybe<() -> ioc ()>), ch : channel<b>, f : a -> b) : async channel<b>
  fun wrapped-setup(cb: (await-result<_>,bool) -> ioc ())
    setup fn (value, b)
      cb(Result(value), b)

  no-await(wrapped-setup, async-scope) fn(res)
    ch.emit-io(f(res.continue-await))
  ch
*/


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
