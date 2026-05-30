/*---------------------------------------------------------------------------
  Copyright 2026, Tim Whiting, Microsoft Research, Daan Leijen.

  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the LICENSE file at the root of this distribution.
---------------------------------------------------------------------------*/

module async/interleave

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
  match Cons(x,xs).interleave-list(False)
    Cons(y,_) -> y
    Nil       -> impossible("empty result list")
  
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
  xs.interleave-list(True)


// Interleave a list of actions 
fun list/interleave-list(xs : list<() -> <async|e> a>, needall : bool ) : <async,ndet|e> list<a>
  with strand-list(needall)
  interleave-strand-list(xs,needall)  


// -------------------------------------------------------
// Strands
// -------------------------------------------------------

// A strand result is either `Canceled` (by `discontinue`),  
// `Finalized` (by an exception etc) with a yield context (so we can reyield), 
// or a `Completed` result value.
type strand-result<a>  
  Canceled
  Finalized(yld : yield-context)     // exception etc.
  Completed(result : a)

fun strand-result/show(self : strand-result<a> ) : e string
  match self
    Canceled  -> "Canceled"
    Finalized -> "Finalized(...)"
    Completed -> "Completed(...)"

// Continue from a strand result by either returning the 
// completed value or reyielding the finalized effect (or `discontinue`).
fun strand-continue( res : strand-result<a> ) : async a
  match res
    Completed(x)    -> x
    Finalized(yld)  -> unsafe-reyield(yld)
    Canceled        -> discontinue()

// Merge a strand result. Used to collect all strand results.
fun strand-merge( result : strand-result<a>, res : strand-result<b>, needall : bool, merge : (a,b) -> a, unit : a ) : strand-result<a>
  match res
    Completed(x) -> match result
      Completed(y) -> Completed(merge(y,x))
      _            -> if !needall then Completed(merge(unit,x)) else result
    Finalized(yld) -> match result
      Completed | needall -> Finalized(yld)
      Canceled  -> Finalized(yld)  // prefer exceptions over canceled
      _         -> result
    Canceled -> match result
      Completed | needall -> Canceled
      _         -> result


// Private effect to keep track of when a strand in an interleaving is done.
// Preferred over using built-in state as this works well if there is an outer handler
// over the state that resumes more than once -- redoing part of the interleaving.
// See `test/algeff/async5.js`
effect strands<a>
  // Are there still strands that need to be resumed?
  fun strands-are-active() : bool
  // Start a strand and return its sequence number
  fun strand-init() : ()
  // Strand is done.
  fun strand-done(result : strand-result<a>) : ()


// Run asynchronous operations with unit results
fun strand-actions( needall : bool, action : () -> <async,strands<a>|e> () )  : <async|e> ()
  var result : strand-result<()> := Completed(())
  var outstanding := 0
  with handler<strands>
    return(_)                 -> result.strand-continue
    fun strands-are-active()  -> outstanding > 0
    fun strand-init()         -> outstanding := outstanding + 1      
    fun strand-done(res)    
      trace("strand done: " ++ res.show)
      result := result.strand-merge(res,needall,fn(_,_) (),())
      outstanding := outstanding - 1      
  with mask<local>
  action()  

// Run an indexed async operation and return the results (in the same order)
fun strand-list( needall : bool, action : () -> <async,strands<(int,a)>|e> () )  : <async|e> list<a>
  var result : some<a> strand-result<list<(int,a)>> := Completed([])
  var outstanding := 0
  with handler<strands>
    return(_)                 result.strand-continue.map(snd)
    fun strands-are-active()  outstanding > 0
    fun strand-init()         outstanding := outstanding + 1      
    fun strand-done(res)    
      trace("strand done: " ++ res.show)
      result := result.strand-merge(res,needall,ordered-insert,[])      
      outstanding := outstanding - 1      
  with mask<local>
  action()

// Insert in order
tail fun ordered-insert( xs : list<(int,a)>, y : (int,a)  ) : list<(int,a)>
  match xs
    Cons(x,xx) | x.fst < y.fst -> Cons(x, ordered-insert(xx,y))
    _          -> Cons(y,xs)
  
  

// -------------------------------------------------------
// Interleaving primitives
// -------------------------------------------------------

// Interleave a fixed number of asychronous actions
pub fun interleave-strand-list(xs : list<() -> <async|e> a>, needall : bool ) : <async,ndet,strands<(int,a)> |e> ()
  // we issed a fixed number of interleave's and thus always terminate (for the outer `:div`)
  with pretend-no-div                                
  interleave-strands(needall) fn(interleave)
    xs.list/foreach-indexed fn(i,action) 
      interleave
        (i,action())

// Interleave _dynamically_ over a producer channel `ch` with an asynchronous consumer function `f`.
// Results are indexed but may be produced out of order.
pub fun interleave-strand-channel(needall : bool, max-interleave : int = 1000, ch : channel<a>, f : a -> <async|e> b ) : <async,ndet,div,strands<(int,b)>|e> ()
  with collapse-ndet
  interleave-strand-compose(needall, { ch.receive() }, fn(x) with mask<ndet> in f(x), max-interleave )

// TODO: make an `interleave-strand-channels` (or compose) that receives from one channel but then 
// emits on another channel _in order_ while still being somewhat concurrent (up to some N) by starting 
// processing on other elements but such that  they won't emit out of order.

// Interleave _dynamically_ over an asynchronous producer function `f` with an asynchronous consumer function `g`.
// Results are indexed but may be produced out of order.
pub fun interleave-strand-compose(needall : bool, f : () -> <async|e> a, g : a -> <async|e> b, max-interleave : int = 1000 ) : <async,ndet,div,strands<(int,b)>|e> ()
  var interleave-count := 0
  var index := 0
  interleave-strands-ex(needall) fn(interleave)    
    interleave
      fun loop()
        val i = index
        index := i + 1
        val x = with mask<strands,ndet,div> in f()          
        if interleave-count < max-interleave then
          // interleave the call to g
          interleave-count := interleave-count + 1          
          interleave 
            with finally
              interleave-count := interleave-count - 1
            with mask<strands,ndet,div> in (i,g(x))
          loop()
        else          
          // stop interleaving if too many interleavings
          with mask<strands,ndet,div> in (i,g(x))
          loop()
      loop()
      

// -------------------------------------------------------
// Primitive interleave strands
// -------------------------------------------------------

alias async-ex<a> = <async,div,ndet,strands<a>>

fun interleave-strands( needall : bool, setup : (ileave : (action : () -> <async|e> a ) -> <async-ex<a>|e> ()) -> <async-ex<a>|e> () )  : <async-ex<a>|e> ()
  interleave-strands-ex(needall) fn(interleave)
    setup fn(action) 
      interleave 
        with mask<strands,ndet,div> in action()

fun interleave-strands-ex( needall : bool, setup : (ileave : (action : () -> <async-ex<a>|e> a ) -> <async-ex<a>|e> ()) -> <async-ex<a>|e> () )  : <async-ex<a>|e> ()
  val ch : channel<() -> <async|_> ()> = channel()   // channel of resumptions
  val () =
    with cancelation-scope                    // limit cancelation to our interleaving            
    setup fn(action)                          // setup interleaved operations
      interleave-strand(needall,ch,action)    // interleave on our channel
  while{ strands-are-active() }           
    ch.receive().()                           // receive and resume (note: this receive must be outside the cancelation scope)

// Interleave an action such that all async operations are captured and emit
// a resumption on the given channel.
fun interleave-strand(needall : bool, ch : channel<() -> <async,strands<a>|e> ()>, action : () -> <async,strands<a>|e> a ) : <async,strands<a>|e> ()
  strand-init()                   // always initialize to keep outstanding count
  if is-canceled() then 
    strand-done(Canceled)         // don't start new interleavings once the scope is canceled
  else            
    with emit-on-await(ch)        // redirect async calls to emit resumptions on the channel (and return () instead of waiting)            
    val res = 
      with unsafe-unfinalize      // catches any final ctl (exceptions)
      with unsafe-uncancel        // catches any discontinue() (so we can prefer exceptions over cancelations)
      action()
    match res
      Right(Just(x)) -> { strand-done(Completed(x)); if !needall then cancel-outstanding() }
      Right(Nothing) -> { strand-done(Canceled) }
      Left(yld)      -> { strand-done(Finalized(yld)); if needall then cancel-outstanding() }
    
