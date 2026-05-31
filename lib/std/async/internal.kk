/*---------------------------------------------------------------------------
  Copyright 2012-2021, Microsoft Research, Daan Leijen.

  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the LICENSE file at the root of this distribution.
---------------------------------------------------------------------------*/

/* Asynchronous internal primitives

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
module async/internal

// ----------------------------------------------------------------------------
// Async effect
// ---------------------------------------------------------------------------- 

// Asynchronous operations have the `:async` effect.
// Usually there is only one handler for asynchronous operations at
// the `main` function using the default `handle-evloop` (inserted
// automatically if `main` has the `:async` effect).
pub effect async-await
  // Setup a callback and await its completion. The setup is performed in the handler async-scope.
  ctl do-await( setup : await-setup<a>, async-scope : async-scope, label : string ) : await-result<a>

  // Internal: Setup a callback to execute `cb` on completion, but return control to the caller immediately
  // after the setup is performed. `cb` is performed in the handler async-scope.
  fun no-await( setup : await-setup<a>, async-scope : async-scope, label : string, cb : await-result<a> -> ioc () ) : ()

pub linear effect async-cancel
  // The current cancelations scope.
  val async-scope : async-scope

  // Cancel any outstanding async operations of a specific cancelation scope (and all its children)
  fun cancel-scope( scope : async-scope ) : ()

  // Has the current scope ever been canceled?
  fun is-scope-canceled( scope : async-scope ) : bool
  
// Function that disposes any resources for an await call (i.e. `clearTimeout`)
pub alias dispose-fn = () -> ioc ()

// Abstract type for an external event loop
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
// Scope identifiers
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

