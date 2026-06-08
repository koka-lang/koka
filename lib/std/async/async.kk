/*---------------------------------------------------------------------------
  Copyright 2018-2026, Tim Whiting, Microsoft Research, Daan Leijen

  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the LICENSE file at the root of this distribution.
---------------------------------------------------------------------------*/

/* Asynchronous primitives.

This module is based closely on [@Leijen:async] and aims to have robust and composable asynchronous
primitives. In particular, any outstanding asynchronous operation can be canceled (through `cancel-outstanding`)
within a certain cancelation scope which allows for composable primitives like `timeout` and `interleave`.

# Cancelation 

When calling `cancel-outstanding`, it cancels any outstanding async operations 
in the current cancelation scope and any of its child scopes.
Some reasons for cancelation:
 - returning normally, cancelation prevents dangling callbacks
 - returning abnormally (e.g. an exception), we cancel any pending callbacks
 - within a `firstof`, we have decided to cancel all remaining strands after
   the first is one completes

After `cancel-outstanding` is called, the caller continues as normal -
calling `cancel-oustanding` simply returns `()`.

The way that cancelation is observed is that all outstanding async operaions in
the cancelation scope receive a `Cancel` result instead of a `Result` (or `Exception`). 
Aside from low-level code, a user won't observe this as the abstractions
awaiting a result automatically use `discontinue()` when `Cancel` a result 
is given.

We use a separate effect `:discontinue` instead of an exception since it
is intended that a `discontinue` is never handled and always discontinues
a stsrand. Of course, `finally` blocks still execute so external resources
can be released properly.

# References 

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
import async/internal

import async/evloop
  [host=libc|wasmweb|jsnode] async/uv/evloop
  [host=jsweb]               async/web/evloop


pub effect discontinue  
  // Internal: The current strand of execution is canceled
  final ctl discontinue() : a

// The asynchronous effect is composed of four primitive effects.
// `:async-await` allows awaiting asynchronous operations,
// `:async-cancel` allows cancelation of outstanding asynchronous operations,
// `:async-ioc` allows execution of linear I/O (`:ioc`) at the handler scope,
// and `:discontinue` discontinues any running strand (using `final ctl`).
pub alias async = <async-await,async-ioc,async-cancel,discontinue>

// Cancel any _other_ outstanding operations that are interleaved and
// part of the same cancelation scope (see `cancelation-scope`).
pub fun cancel-outstanding()
  cancel-scope(async-scope)

// Is the current cancelation canceled?
pub fun is-canceled()
  is-scope-canceled(async-scope)

// Perform a stateful operation in the outer async handler.
pub fun async-st(action: () -> st<global> a): async a
  async-ioc(action)

// Show an await result
pub fun await-result/show(r: await-result<a>, ?show : a -> string) : string
  match r
    Cancel         -> "Cancel"
    Exception(exn) -> "Exception(" ++ exn/show(exn) ++ ")"
    Result(x)      -> "Result(" ++ x.show ++ ")"


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
  setup/await(label) fn(_evloop,resume) setup(fn(x) resume(Ok(x)))

// Await with the exception effect
pub fun setup/await( label : string = "", setup : (evloop : event-loop, resume : error<a> -> ioc ()) -> ioc error<dispose-fn> ) : <async,exn> a  
  await-error1(label,setup).untry

// Primitive await: call `setup` to set up an asynchronous callback with the host platform. 
// The setup is passed the eventloop of the async handler, and a `resume` function
// that takes an `:await-result`: a cancelation, exception, or regular result.
// The `setup` function returns either an error, or a `dispose` funcion that is invoked on cancellation 
// to dispose of any resources (see the implementation of `wait`).
// The `resume` is invoked exactly once (either on the async completion callback, or on cancelation)
pub fun setup/await-error( label : string, setup : (evloop : event-loop, resume : (a) -> ioc ()) -> ioc error<dispose-fn> ) : async error<a>
  val res = do-await(fn(evloop,resume) { setup(evloop, fn(res) resume(Result(res),True)) }, async-scope, label)
  res.error


pub fun setup/await-error1( label : string, setup : (evloop : event-loop, resume : error<a> -> ioc ()) -> ioc error<dispose-fn> ) : async error<a>
  val res = do-await(fn(evloop,resume) { setup(evloop, fn(eres) resume(eres.await-result,True)) }, async-scope, label)
  res.error

fun error/await-result( res : error<a> ) : await-result<a>
  match res
    Error(exn)     -> Exception(exn)
    Ok(value)      -> Result(value)

fun await-result/error( res : await-result<a>, ?label : string ) : discontinue error<a>
  match res
    Result(value)  -> Ok(value)
    Exception(exn) -> Error(exn)
    Cancel -> 
      trace("discontinue from await: " ++ label)
      discontinue()

extend type exception-info
  ExnSync

pub fun do-sync( action : (evloop : event-loop) -> ioc error<()>, label : string = "" ) : <exn,async> ()
  val res = do-await(fn(evloop,resume) { 
                        match action(evloop)
                          Error(err) -> Error(err) 
                          Ok(_)      -> Error(Exception("do-sync",ExnSync)) 
                    }, async-scope, label)
  match res
    Exception(Exception(_,ExnSync)) -> ()
    _ -> res.error.untry

  


// ----------------------------------------------------------------------------
// Default async handler
// ----------------------------------------------------------------------------

fun noop(): ()
   ()

// The main async handler keeps a list of outstanding "callbacks" as `:await-resumption`s
// This is used for cancelation and debugging. The scope of any await resumption
// is always a unique (child) scope so can be used to uniquely identify an specific resumption.
type await-resumption
  AwaitResumption<a>( scope : async-scope, label : string, resume : (await-result<a>,bool) -> ioc () )

fun await-resumption/show( AwaitResumption(scope,label,_) ) : string
  label ++ " at " ++ scope.show

// Remove a specific `:await-resumption` from the list.
fun remove( xs : list<await-resumption>, scope : async-scope ) : list<await-resumption>
  match xs
    Cons(AwaitResumption(s,_),xx) | s == scope -> xx
    Cons(x,xx) -> Cons(x,remove(xx,scope))
    Nil        -> Nil

// The outer `:async` effect handler. This is automatically applied by the compiler
// around the `main` function if it has an `:async` effect.
fun async-evloop(evloop : event-loop, action : () -> <async,ioc> () ) : ioc ()
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
    
// The main `:async` handler. Uses "libuv" for the C backend,
// (with a libuv emscripten wrapper for the Wasm Web backend).
// Uses the standard async calls for Javascript (web or Node).
pub fun async( action : () -> <async,ioc> () ) : ioc () 
  val evloop = event-loop-init()
  async-evloop(evloop,action)
  event-loop-run(evloop)
  
// The default async handler called from `main`.  
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

// Run an `action` as if it is part of the parent scope 
// (so it will be canceled only if the parent scope is canceled)
pub fun in-parent-scope( action : () -> <async|e> a ) : <async|e> a
  override handle<async-cancel>(action)
    val async-scope = async-scope.parent 
    fun is-scope-canceled(scope)  is-scope-canceled(scope)
    fun cancel-scope(scope)       cancel-scope(scope)    
  
// Catch a discontinue as a `:maybe`.
// Use with care as cancelation should not be ignored.
pub fun unsafe-uncancel( action : () -> <async|e> a): <async|e> maybe<a>
  override handle(action)
    return(x)               Just(x)
    final ctl discontinue() Nothing
    


