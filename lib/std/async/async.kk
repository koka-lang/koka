/*---------------------------------------------------------------------------
  Copyright 2012-2021, Microsoft Research, Daan Leijen.

  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the LICENSE file at the root of this distribution.
---------------------------------------------------------------------------*/

/* Asynchronous primitives.

This module is based closely on [@Leijen:async] and aims to have robust and composable asynchronous
primitives. In particular, any outstanding asynchronous operation can be canceled (through `cancel-outstanding`)
within a certain async-scope which allows for composable primitives like `timeout` and `firstof`.

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
module async/async

import std/num/int64
import std/core/undiv
import std/core/unsafe
import std/num/float64
import std/time/duration

import async/uv/core
import async/uv/timer
import async/internal

/*
# Cancellation scopes

Each async-scope has a unique id.
When we await any async action, we associate it with its full async-scope chain. If
async-scope a spawns async-scope b which spawns async-scope c which spawns an action, that
action's async-scope is Scope([c, b, a]) - a async-scope is just a list of async-scope IDs,
childmost first.

When we decide to cancel a async-scope, we cancel it by ID, e.g. `b`. This will
cancel the following scopes:

`[b, _]` -> clearly in b `[_, b, _])` -> a child async-scope of b

It will not cancel something in `[a, d]` (i.e. within `a` but not `b`)

# Cancelation: up and down

When cancelation is triggered, it is generally triggered from _within_ the
async-scope to be canceled, and affects all transitive child scopes. There are many
reasons for cancelation:
 - returning normally, cancelation prevents dangling callbacks
 - returning abnormally (e.g. exn), we cancel any pending callbacks
 - within a `firstof`, we have decided to cancel all remaining strands after
   the first is complete

After cancelation is triggered, the canceler's execution continues as normal -
calling `cancel-scope` simply returns `()`.

Triggering affects actions within the async-scope - so when observing cancelation, it
being triggered from somewhere "up" the stack. It's like a reverse of an
exception (which is coming from "down" the stack and observed "up" the stack).

The way that cancelation is observed is that all outstanding callbacks beneath
the canceled async-scope receive a `Cancel` instead of a `Result`. Aside from
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

When one strand returns abnormally (e.g. exception), the entire async-scope is
canceled. This will cause any pending callbacks to be resolved with a Cancel
result.

# Interleave and cancelation

In order to control cancelation explicitly, `interleave-strands` creates a root
async-scope instead of a child async-scope to spawn operations in. No actions spawned
within this async-scope will be seen as children of the parent async-scope, preventing
automatic cancelation when a parent async-scope is canceled.

Instead, `interleave-strands` _observes_ when the parent async-scope is canceled, and
triggers cancelation on the child strands. Crucially this doesn't cancel the
interleaving bookkeeping itself, which ensures that:
 - `interleave-raw` always observes the final states of all strands, even if
   all of them are `Cancel`
 - We drive each strand to completion, including finalizers. If we simply
   abandoned the bookeeping, we'd potentially leave finalize actions sitting in
   an unconsumed channel instead of running them.

*/

pub linear effect async-ioc
  // Internal: perform an I/O operation in the outer async handler
  fun async-ioc( action : () -> ioc a ) : a

  // Internal: perform an I/O operation in the outer async handler in the next event loop tick
  fun async-schedule-ioc( action : () -> ioc () ) : ()
  
pub effect discontinue  
  // Internal: The current strand of execution is canceled
  final ctl discontinue() : a

// The asynchronous effect is composed of four primitive effects.
pub alias async = <async-await,async-ioc,async-cancel,discontinue>


// Cancel any _other_ outstanding operations that are interleaved and
// part of the same cancelation async-scope (see `cancelation-async-scope`).
pub fun cancel-outstanding()
  cancel-scope(async-scope)

pub fun is-canceled()
  is-scope-canceled(async-scope)

// Perform a stateful operation in the outer async handler.
pub fun async-st(action: () -> st<global> a): async a
  async-ioc(action)

pub fun await-result/show(r: await-result<a>) : string
  match r
    Cancel -> "Cancel"
    Exception(exn) -> "Exception(" ++ exn/show(exn) ++ ")"
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
  await-error("wait " ++ secs.show) fn(evloop,resume)                   // ignore errors; we assume timeouts always work
    timer-setup(evloop,msecs,fn() resume(()))     
  ()

// Yield to other asynchronous operations. Same as `wait(0)`.
pub fun yield() : async ()
  duration/wait()


// ----------------------------------------------------------------------------
// Await wrappers
// ----------------------------------------------------------------------------

// Convenience function for awaiting a zero argument callback that never fails
fun await0-noexn( label : string = "", setup : (resume : () -> ioc () ) -> ioc () ) : async ()
  await1-noexn(label) fn(resume) setup(fn() resume(())) 

// Convenience function for awaiting a single argument callback that never fails
pub fun await1-noexn( label : string = "", setup : (resume : (a) -> ioc () ) -> ioc () ) : async a
  await-noexn(label) fn(_evloop,resume){ setup(resume); (fn() {}) }
  
// Convenience function for async functions where the setup cannot fail. In that case
// the result will never be an error.
fun await-noexn( label : string = "", setup : (evloop : event-loop, resume : (a) -> ioc ()) -> ioc dispose-fn ) : async a  
  val res = setup/await-error( label, fn(evloop,resume) setup(evloop,resume).Ok )   
  match res
    Ok(x)      -> x
    Error(exn) -> impossible("unexpected exception returned from async await: " ++ show(exn))


// Await a zero argument callback.
pub fun await0( label : string = "", setup : (resume : () -> ioc () ) -> ioc error<dispose-fn> ) : <async,exn> ()
  await1(label) fn(resume) setup(fn() resume(())) 

// Await a single argument callback.
pub fun await1( label : string = "", setup : (resume : (a) -> ioc () ) -> ioc error<dispose-fn> ) : <async,exn> a
  setup/await(label) fn(_evloop,resume) setup(resume)

// Await with the exception effect
pub fun setup/await( label : string = "", setup : (evloop : event-loop, resume : (a) -> ioc ()) -> ioc error<dispose-fn> ) : <async,exn> a  
  await-error(label,setup).untry

// Primitive await: call `setup` to set up an asynchronous callback with the host platform. 
// The setup is passed the eventloop of the async handler, and a `resume` function
// that takes an `:await-result`: a cancelation, exception, or regular result.
// The `setup` function returns either an error, or a `dispose` funcion that is invoked on cancellation 
// to dispose of any resources (see the implementation of `wait`).
// The `resume` is invoked exactly once (either on the async completion callback, or on cancelation)
pub fun setup/await-error( label : string, setup : (evloop : event-loop, resume : (a) -> ioc ()) -> ioc error<dispose-fn> ) : async error<a>
  val res = do-await(fn(evloop,resume) { setup(evloop, fn(res) resume(Result(res),True)) }, async-scope, label)
  match res
    Result(value)  -> Ok(value)
    Exception(exn) -> Error(exn)
    Cancel         -> { trace("resumed with cancel: " ++ label); discontinue() }


// ----------------------------------------------------------------------------
// Default async handler
// ----------------------------------------------------------------------------

fun noop(): ()
   ()

type await-resumption
  AwaitResumption<a>( scope : async-scope, label : string, resume : (await-result<a>,bool) -> ioc () )

fun await-resumption/show( AwaitResumption(scope,label,_) ) : string
  label ++ " at " ++ scope.show

// The outer `:async` effect handler. This is automatically applied by the compiler
// around the `main` function if it has an `:async` effect.
pub fun async-evloop(evloop : event-loop, action : () -> <async,ioc> () ) : ioc ()
  // Outstanding async requests
  val outstanding : ref<global,list<await-resumption>> = ref([])

  // Has the root async-scope been canceled?
  val canceled : ref<global,bool> = ref(False)
  
  // Setup a new async await request with a given callback `resume`
  fun setup-await( setup : await-setup<a>, scope : async-scope, label : string, resume : (await-result<a>) -> ioc ()) : ioc ()
    // Each callback gets its own async-scope so we can tell when a given
    // callback can be removed from `outstanding`
    val cscope = new-child-scope-ioc(scope)

    // `disposer` holds the dispose function (that releases resources on cancelation)
    val disposer : ref<global,dispose-fn> = ref(noop)

    // The resumption callback that is invoked when the async request completes (or on cancelation)
    fun resume-cb( res : await-result<_>, is-done : bool ): ioc () 
      if is-done then
        outstanding := (!outstanding).remove(cscope)                  
        val dispose = disposer.swap(noop)  // unreference early
        if res.is-cancel then dispose()
      resume(res)

    outstanding := Cons(AwaitResumption(cscope,label,resume-cb), !outstanding)
    match(setup(evloop,resume-cb))                   // primitive setup: registers `resume-cb` as the callback for an async request
      Ok(dispose) -> disposer := dispose
      Error(exn)  -> resume-cb(Exception(exn),True)  // if setup fails, we resume immediately with an exception result

  // Cancel all outstanding async requests that are in `scope`
  fun cancel-outstanding-in-scope( scope : async-scope ) : ioc ()
    (!outstanding).foreach fn(AwaitResumption(await-scope,label,resume-cb))
      if await-scope <= scope then 
        trace("cancel: " ++ label ++ " at " ++ scope.show)
        resume-cb(Cancel,True)
      else 
        trace("no cancel: " ++ label ++ " at " ++ scope.show)
  
  // Schedule an async call for the next evloop iteration (uv_check_t / setImmediate)
  fun schedule( f : () -> ioc () ) : ioc ()    
    match immediate-setup(evloop,f)  
      Ok    -> ()   // scheduled f to be called
      Error -> f()  // if setup fails we call it immediately

  with handler<async-await>
    raw ctl do-await(setup,scope,lab)   setup-await(setup, scope, lab, fn(x) rcontext.resume(x) )    
    fun no-await(setup,scope,lab,cb)    setup-await(setup, scope, lab, cb )    

  with handler<async-ioc>
    fun async-ioc(f)                    f()
    fun async-schedule-ioc(f)           schedule(f)    

  with handler<async-cancel>
    val async-scope = root-scope    
    fun is-scope-canceled(scope)        (scope==root-scope && !canceled)
    fun cancel-scope(scope)             
      trace("cancel-scope: " ++ scope.show)
      if scope == root-scope then canceled := True
      cancel-outstanding-in-scope(scope)
  
  with handler<discontinue>
    final ctl discontinue()             
      cancel-outstanding-in-scope(root-scope)
      trace("discontinue occurred at the toplevel async-scope")      

  action()
    

pub fun async( action : () -> <async,ioc> () ) : ioc () 
  val evloop = event-loop-init()
  async-evloop(evloop,action)
  event-loop-run(evloop)
  
pub fun @default-async( action : () -> <async,ioc> () ) : ioc () 
  async(action)


// ----------------------------------------------------------------------------
// Cancelation async-scope
// ----------------------------------------------------------------------------

// Set up a new cancelation scope
pub fun cancelation-scope( action : () -> <async|e> a) : <async|e> a
  val cscope = new-child-scope(async-scope)
  var canceled := False 
  override handle<async-cancel>(action)
    val async-scope = cscope
    fun is-scope-canceled(scope)  
      (scope<=cscope && canceled) || is-scope-canceled(scope)
    fun cancel-scope(scope)       
      if scope==cscope then canceled := True
      cancel-scope(scope)

pub fun in-parent-scope( action : () -> <async|e> a ) : <async|e> a
  override handle<async-cancel>(action)
    val async-scope = async-scope.parent 
    fun is-scope-canceled(scope)  is-scope-canceled(scope)
    fun cancel-scope(scope)       cancel-scope(scope)    
  
// Catch a discontinue as a `:maybe`
pub fun unsafe-uncancel( action : () -> <async|e> a): <async|e> maybe<a>
  override handle(action)
    return(x)               Just(x)
    final ctl discontinue() Nothing
    

// ----------------------------------------------------------------------------
// Scope identifiers
// ----------------------------------------------------------------------------

pub fun new-child-scope( scope : async-scope ) : async async-scope
  async-ioc{ new-child-scope-ioc(scope) }

// Convenience functions for async-scope maps
fun remove( xs : list<await-resumption>, scope : async-scope ) : list<await-resumption>
  match xs
    Cons(AwaitResumption(s,_),xx) | s == scope -> xx
    Cons(x,xx) -> Cons(x,remove(xx,scope))
    Nil        -> Nil


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
// Note: once you exit the `child-async-scope` async-scope where `await-to-channel` was called, the callback is invoked with a `Cancel` exception.
// The channel should always be awaited within the same `child-async-scope` async-scope as the `await-to-channel` invokation.
pub fun await-to-channel( setup : (cb : (a,bool) -> ioc ()) -> ioc (maybe<() -> ioc ()>), ch : channel<b>, f : a -> b) : async channel<b>
  fun wrapped-setup(cb: (await-result<_>,bool) -> ioc ())
    setup fn (value, b)
      cb(Result(value), b)

  no-await(wrapped-setup, async-scope) fn(res)
    ch.emit-io(f(res.continue-await))
  ch
*/


/*
// Execute `action` in a child-async-scope async-scope. Once `action` is complete
// (successfully or otherwise), all outstanding actions
// within this async-scope are canceled.
pub fun child-async-scope( action : () -> <async|e> a ) : <async|e> a
  child-async-scope-without-auto-cancel fn(cid)
    with finally
      // cancel any outstanding operations still in our async-scope.
      // this might be needed for `no-await` operations.
      cancel-scope(cid)
    action()

// like child-async-scope, but the caller is responsible
// for canceling the created async-scope
fun child-async-scope-without-auto-cancel(action : (int) -> <async|e> a ) : <async|e> a
  val cid = async-iox{ unique() }
  handle ({mask behind<async> { action(cid) }})
    val async-scope = async-scope.child(cid)
    fun do-await(setup,async-scope) do-await(setup,async-scope)
    fun no-await(setup,async-scope,f) no-await(setup,async-scope,f)
    fun cancel-scope(async-scope) cancel-scope(async-scope)
    fun async-iox(f) async-iox(f)
    fun discontinue() discontinue()

pub fun on-cancel(fin: () -> <async|e> (), action: () -> <async|e> a): <async|e> a
  handle<async>({ mask behind<async>(action) })
    final ctl discontinue()
      fin()
      discontinue()

    // passthrough others
    val async-scope = async-scope
    fun do-await(setup, async-scope) do-await(setup,async-scope)
    fun no-await(setup, async-scope, f) no-await(setup,async-scope, f)
    fun async-iox(x) async-iox(x)
    fun cancel-scope(async-scope) cancel-scope(async-scope)

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

  // within the outer async-scope, spawn a canary action
  // to capture cancelation triggered from above.
  fun setup-cb(evloop : event-loop, cb: ((await-result<()>, bool) -> ioc ())): ioc error<dispose-fn>
    dangling-cb.set(Just(cb))
    return Ok(cancel) // cleanup fn
  no-await(setup-cb, async-scope, fn(_) ())

  // create a new async-scope root, so that any callbacks registered
  // underneath this async-scope will not be seen as children of
  // any scopes above us (and therefore not canceled if an outer
  // async-scope is canceled)
  val cid = async-iox { unique() }
  val new-async-scope = empty-async-scope.child(cid)

  with finally {
    cancel-scope(cid) // don't leave anything dangling in case of exn etc
  }

  with handler<async>
    val async-scope = new-async-scope
    fun do-await(setup,async-scope) do-await(setup,async-scope)
    fun no-await(setup,async-scope,f) no-await(setup,async-scope,f)

    // We notice cancelation from a parent via the canary. We also
    // need to modify the `cancel-scope` hook so that we notice (and defer)
    // cancelation triggered from within this async-scope, but only if it's
    // targeted at exactly this async-scope.
    fun cancel-scope(async-scope-id)
      if async-scope-id == cid then
        async-iox(cancel)
      else
        // pass it along, it's presumably a child async-scope
        // nested under this uncancelable one
        cancel-scope(async-scope-id)

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
