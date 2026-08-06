/*---------------------------------------------------------------------------
  Copyright 2018-2026, Microsoft Research, Daan Leijen.

  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the LICENSE file at the root of this distribution.
---------------------------------------------------------------------------*/

/* Asynchronous internal primitives

This module defines the primitive asynchronous effects for asynchronous
operations (`:async-await`) and cancelation (`:async-cancel`). The operations
of these effects break the abstraction and should only be used by library 
implementations.

*/
module async/internal

// ----------------------------------------------------------------------------
// Async effect
// ---------------------------------------------------------------------------- 

// Asynchronous operations have the `:async-await` effect.
// Usually there is only one handler for asynchronous operations at
// the `main` function using the default handler (inserted
// automatically if `main` has the `:async` effect).
pub effect async-await
  // Internal: setup a primitive callback and await its completion. The setup is performed in the handler scope.
  ctl do-await( setup : await-setup<a>, async-scope : async-scope, label : string ) : await-result<a>

  // Internal: Setup a callback to execute `cb` on completion, but return control to the caller immediately
  // after the setup is performed. `cb` is performed in the handler scope.
  fun no-await( setup : await-setup<a>, async-scope : async-scope, label : string, cb : await-result<a> -> ioc () ) : ()

// The cancelation effect which allows for cancelation of any 
// outstanding asynchronous (interleaved) operations in a cancelation scope (`:async-scope`).
pub linear effect async-cancel
  // Internal: The current cancelations scope.
  val async-scope : async-scope

  // Internal: Cancel any outstanding async operations of a specific cancelation scope (and all its children)
  fun cancel-scope( scope : async-scope ) : ()

  // Internal: Has the current scope ever been canceled?
  fun is-scope-canceled( scope : async-scope ) : bool

// Effect to allow `:ioc` operations to execute in the scope of the handler.
pub linear effect async-ioc
  // Internal: perform an I/O operation in the outer async handler
  fun async-ioc( action : () -> ioc a ) : a

  // Internal: perform an I/O operation in the outer async handler in the next event loop tick
  fun async-schedule-ioc( action : () -> ioc () ) : ()

  
// Function that disposes any resources for an await call (i.e. `clearTimeout`)
// and should ensure that any primitive callback is no longer called.
pub alias dispose-fn = () -> ioc ()

// Abstract type for an external event loop. 
pub alias event-loop = any

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

// ----------------------------------------------------------------------------
// Cancelation Scopes
// ----------------------------------------------------------------------------

// A async-scope has an ID, plus a list of child IDs.
// Every created async-scope has a unique ID
alias scope-id = int

// An abstract cancelation async-scope
abstract value type async-scope
  Scope( scopes : list<scope-id> )

pub fun async-scope/show( Scope(ids) : async-scope ) : string
  ids.map(show).join("<")

// A scope is the chain starting with the current scope, including parent scopes up to the root scope-id
pub val root-scope = Scope([0])

pub fun new-child-scope-ioc( Scope(ids) : async-scope ) : ioc async-scope
  val cid = unique()+1
  Scope(Cons(cid,ids))  

pub fun new-child-scope( scope : async-scope ) : async-ioc async-scope
  async-ioc{ new-child-scope-ioc(scope) }


fun scope-id( Scope(ids) ) : scope-id
  ids.head(0)

pub fun parent( Scope(ids) : async-scope ) : async-scope
  match ids
    Cons(_,idd as Cons) -> Scope(idd)
    _                   -> root-scope
  
// Is `async-scope` equal to, or a child async-scope of `root`?
pub fun (<=)( Scope(ids) : async-scope, root : async-scope ) : bool
  val sid = root.scope-id
  ids.find(fn(x) x == sid).is-just

// O(1). Are the scopes equal?
pub fun async-scope/(==)(s1 : async-scope, s2 : async-scope ) : bool
  s1.scope-id == s2.scope-id

