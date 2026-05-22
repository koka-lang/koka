/*---------------------------------------------------------------------------
  Copyright 2026, Tim Whiting, Microsoft Research, Daan Leijen.

  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the LICENSE file at the root of this distribution.
---------------------------------------------------------------------------*/

module std/async/interleave

import std/async/async
import std/async/channel
import std/time/duration
import std/core/unsafe
import std/core/undiv

// ----------------------------------------------------------------------------
// Interleaved strands of execution
// ----------------------------------------------------------------------------

// Execute `action` but if it is not finished within `secs` seconds duration
// `cancel` it (and return `Nothing`). Due to the generality of `cancel`, this `timeout`
// abstraction can reliably time out over any composition of asynchronous operations
// and is therefore quite expressive.
pub fun timeout( secs : duration, action : () -> <async,ndet|e> a ) : <async,ndet|e> maybe<a>
  firstof { wait(secs); Nothing } { Just(action()) }

// Execute `a` and `b` interleaved. As soon as one of them finishes,
// `cancel` the other one and return the result of finished action.
pub fun two/firstof( action1 : () -> <async,ndet|e> a, action2 : () -> <async,ndet|e> a) : <async,ndet|e> a
  list/firstof-any(action1,[action2])

// Interleave a list of actions `[x]++xs` and return the result of the first action
// that completes (and cancel all others).
// todo: we need a `:list1` type.
pub fun list/firstof-any( x : () -> <async,ndet|e> a, xs : list<() -> <async,ndet|e> a>) : <async,ndet|e> a
  Cons(x,xs).interleaved-raw(False).first-completed-strand
  
// Interleave two actions
pub fun two/interleaved( action1 : () -> <async|e> a, action2 : () -> <async|e> b ) : <async|e> (a,b)
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

// Ensure we prefer `Finalized` over `Canceled`
fun collapse( wrong : list<strand-result<a>> ) : strand-result<()>
  match wrong
    Cons(Canceled,rest as Cons(Canceled,_)) -> collapse(rest)
    Cons(Canceled,Cons(Finalized(yld),_))   -> Finalized(yld)
    Cons(Canceled,_)                        -> Canceled
    _                                       -> Completed(())

// Partition strands into completed ones, and a single (collapsed) uncompleted one where
// we prefer a `Finalized` result over `Canceled`. 
// (If all strands completed succesfully, the second element is returned as `Completed(())`).
fun partition-strands( strands : list<strand-result<a>> ) : async (list<a>, strand-result<()>)
  val (xs,wrong) = strands.partition-map fn(res) match(res)
                      Completed(x) -> Left(x)
                      _            -> Right(res)
  (xs,wrong.collapse)

// Return strands that completed normally (and ignore the others)
fun any-completed-strands( strands : list<strand-result<a>> ) : <async,ndet> list<a>
  strands.partition-strands.fst

// Check that all strands completed normally or reyield the first finalized one (prefering non-canceled ones)
fun all-completed-strands( strands : list<strand-result<a>> ) : async list<a>
  val (xs,wrong) = strands.partition-strands
  match wrong
    Finalized(yld) -> unsafe-reyield(yld)
    Canceled       -> discontinue()
    _              -> xs

// Return the first strand result that is not finalized (i.e. exception or canceled)
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


// Catch a discontinue as a `:maybe`
fun unsafe-uncancel( action : () -> <async|e> a): <async|e> maybe<a>
  with override handler
    final ctl discontinue() Nothing
    return(x)               Just(x)
    // passthrough others
    val async-scope = async-scope
    fun do-await(setup, scope)    do-await(setup,scope)
    fun no-await(setup, scope, f) no-await(setup,scope, f)
    fun async-ioc(x)              async-ioc(x)
    fun cancel-scope(scope)       cancel-scope(scope)  
  action()


noinline fun interleave-strands(xs : list<() -> <async|e> a>, needall : bool ) : <async,strands<a> |e> ()
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
          with unsafe-unfinalize     // catches any final ctl (exceptions)
          with unsafe-uncancel       // catches any discontinue()
          with mask<strands>         
          action()
        match res
          Left(yld)      -> { strand-done(i,Finalized(yld)); if needall then cancel-outstanding() }
          Right(Nothing) -> { /* trace("caught cancel: strand " ++ i.show); */ strand-done(i,Canceled) }
          Right(Just(x)) -> { strand-done(i,Completed(x)); if !needall then cancel-outstanding() }

  // while we receive resumptions in the channel, invoke them
  with pretend-no-div
  while { strands-are-busy() }       // while there are resumptions on the strands..
    val strand-resume = pretend-no-ndet{ ch.receive }   // receive yields back to the outer event loop
    with mask<div>
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
