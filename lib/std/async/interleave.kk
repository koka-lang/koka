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
  with collapse-ndet // safe: <ndet,ndet,async|e> to <ndet,async|e>
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
  with pretend-no-ndet   // remove outer ndet: if all completed with a value, the final list of values is deterministic
  xs.interleaved-raw(True).all-completed-strands


// A strand result is either a finalizing yield (exception, cancel, etc), or a completed result value.
type strand-result<a>  
  Canceled
  Finalized(yld : yield-context)     // exception etc.
  Completed(result : a)

fun strand-result/show(self : strand-result<a> ) : e string
  match self
    Canceled  -> "Canceled"
    Finalized -> "Finalized(...)"
    Completed -> "Completed(...)"


// Ensure we prefer `Finalized` over `Canceled`
fun collapse( wrong : list<strand-result<a>> ) : strand-result<()>
  match wrong
    Cons(Canceled,Nil)     -> Canceled
    Cons(Canceled,rest)    -> collapse(rest)
    Cons(Finalized(yld),_) -> Finalized(yld)
    _                      -> Completed(())

// Partition strands into completed ones, and a single (collapsed) uncompleted one where
// we prefer a `Finalized` result over `Canceled`. 
// (If all strands completed succesfully, the second element is returned as `Completed(())`).
fun partition-strands( strands : list<strand-result<a>> ) : async (list<a>, strand-result<()>)
  val (xs,wrong) = strands.partition-map fn(res) match(res)
                      Completed(x) -> Left(x)
                      _            -> Right(res)
  (xs,wrong.collapse)

// Return strands that completed normally (and ignore the others)
fun any-completed-strands( strands : list<strand-result<a>> ) : <async|e> list<a>
  strands.partition-strands.fst

// Check that all strands completed normally or reyield the first finalized one (prefering non-canceled ones)
fun all-completed-strands( strands : list<strand-result<a>> ) : <async|e> list<a>
  val (xs,wrong) = strands.partition-strands
  match wrong
    Finalized(yld) -> unsafe-reyield(yld)
    Canceled       -> discontinue()
    _              -> xs

// Return the first strand result that is not finalized (i.e. exception or canceled)
fun first-completed-strand( strands : list<strand-result<a>> ) : <async|e> a
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
  // Are there still strands that need to be resumed?
  fun strands-are-busy() : bool
  // Call this when a strand is done.
  fun strand-done(idx : int, result : strand-result<a>) : ()
  

// Interleave a list of actions by tracking through a local state 
// when each action is done (or finalized)
fun list/interleaved-raw(xs : list<() -> <async|e> a>, needall : bool ) : <async,ndet|e> list<strand-result<a>>
  with strand-results(xs.length)
  interleave-strands(xs,needall)  

fun strand-results( total : int, action : () -> <async,strands<a>|e> () )  : <async|e> list<strand-result<a>>
  var results : some<a> list<(int,strand-result<a>)> := []
  with handler<strands<_>>    
    return(_)                   results.map(snd)
    fun strands-are-busy()      results.length < total      
    fun strand-done(idx,res)    
      trace("strand " ++ idx.show ++ ": " ++ res.show)
      results := results.insert(idx,res)      
  with mask<local>
  action()
  
// Insert in order
tail fun insert( xs : list<(int,a)>, idx : int, value : a, n : int  = 0 ) : list<(int,a)>
  match xs
    Cons(x,xx) | x.fst < idx -> Cons(x, insert(xx, idx, value, n + 1))
    _ -> Cons((idx,value),xs)

// Catch a discontinue as a `:maybe`
fun unsafe-uncancel( action : () -> <async|e> a): <async|e> maybe<a>
  override handle(action)
    final ctl discontinue() Nothing
    return(x)               Just(x)
    // passthrough others
    val async-scope = async-scope
    fun do-await(setup, scope, l)     do-await(setup,scope,l)
    fun no-await(setup, scope, l, f)  no-await(setup,scope,l,f)
    fun async-ioc(x)                  async-ioc(x)
    fun cancel-scope(scope)           cancel-scope(scope)  

noinline fun interleave-strands(xs : list<() -> <async|e> a>, needall : bool ) : <async,ndet,strands<a> |e> ()
  with pretend-no-div                                // our while loop always terminates
  with is-canceled <- tracked-cancelation-scope      // limit cancelation to our interleaving        
  // start strands emitting to the channel          
  val  ch : channel<() -> <async|_> ()> = channel()  // channel of resumptions
  xs.list/foreach-indexed fn(i,action)
    if is-canceled() then
      // immediately cancel if our cancelation scope received a cancel
      strand-done(i,Canceled) 
    else
      // otherwise run the action (up to the first async call)
      with emit-on-await(ch)        // redirect async calls to emit resumptions on the channel (and return () instead of waiting)            
      val res = 
        with unsafe-unfinalize      // catches any final ctl (exceptions)
        with unsafe-uncancel        // catches any discontinue()
        with mask<strands,div,ndet> // mask our effects that should not be visible to `action`
        action()
      match res
        Left(yld)      -> { strand-done(i,Finalized(yld)); if needall then cancel-outstanding() }
        Right(Nothing) -> { strand-done(i,Canceled) }
        Right(Just(x)) -> { strand-done(i,Completed(x)); if !needall then cancel-outstanding() }

  // while the strands are not finished
  while{ strands-are-busy() }
    ch.receive.()                     // receive and resume
