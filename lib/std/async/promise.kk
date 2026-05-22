/*---------------------------------------------------------------------------
  Copyright 2026, Tim Whiting, Microsoft Research, Daan Leijen.

  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the LICENSE file at the root of this distribution.
---------------------------------------------------------------------------*/

module std/async/promise
import std/async/async

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
