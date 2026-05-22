/*---------------------------------------------------------------------------
  Copyright 2026, Tim Whiting, Microsoft Research, Daan Leijen.

  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the LICENSE file at the root of this distribution.
---------------------------------------------------------------------------*/

module std/async/channel
import std/async/async

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
  Waiting( listener : a -> ioc (), listeners : list<a -> ioc ()> = [] )

fun from-values(values : list<a> ) : channel-state<a>
  match values
    Nil -> Empty
    Cons(v,vs)  -> Values(v,vs)

fun from-waiting(listeners : list<a -> ioc ()>) : channel-state<a>
  match listeners
    Nil -> Empty
    Cons(l,ls)  -> Waiting(l,ls)

// Create a new asynchronous channel.
pub fun channel() : async channel<a>
  async-iox
    Channel(unique(), ref(Empty))

val no-dispose = Ok({})    

// Receive (and remove) a value from the channel: returns immediately if a value is available and otherwise
// waits asynchronously until a value becomes available.
pub fun receive( ch : channel<a>) : <async,div,ndet> a
  fun setup( cb : _ -> ioc () ) : ioc ()
    val r = ch.state
    match !r
      Empty -> r := Waiting(cb,[])
      Waiting(l,ls) -> r := Waiting(l,ls ++ [cb])
      Values(v,vs)->   // this case should not happen due to `try-receive`
        r := from-values(vs)
        cb(v)        

  match ch.try-receive
    Just(v) -> v
    Nothing -> await1-noexn(setup)

// Return immediately if a value is available on the channel and otherwise returns `Nothing`.
pub fun try-receive( ch : channel<a> ) : <async,ndet> maybe<a>
  async-ioc
    val r = ch.state
    match (!r)
      Values(v, vs) ->
        r := from-values(vs)
        Just(v)
      _ -> Nothing

fun emit-io( ch : channel<a>, value : a ) : ioc ()
  val r = ch.state
  match !r
    Empty -> r := Values(value, [])
    Values(v,vs)  -> r := Values(v,vs ++ [value])
    Waiting(l,ls) ->
      r := from-waiting(ls)
      l(value)

// Emit a value asynchronously into a channel.
pub fun emit( ch : channel<a>, value : a ) : <async,div> ()
  async-ioc
    emit-io(ch,value)

fun trace-channel( msg : string, ch : channel<a> ) : <async,ndet> ()
  async-ioc
    trace-channel-io( msg, ch )

fun trace-channel-io( msg : string, ch : channel<a> ) : ioc ()
  val msgx = msg ++ ": id=" ++ ch.chid.show
  val r = ch.state
  match !r
    Empty -> trace(msgx ++ ", empty")
    Values(v,vs) -> trace-any(msgx ++ ", full: " ++ (1 + vs.length).show ++ ": ", v )
    Waiting(_,ls) -> trace(msgx ++ ", listeners: " ++ (1 + ls.length).show)

fun trace-anyx( s : string, x : a ) : async ()
  trace-any(s,x)

// Returns on an await call and emits the resumption eventually in the given channel
pub fun emit-on-await(ch : channel<() -> <async|e> ()>, action : () -> <async|e> () ) : <async|e> ()
  with override handler
    raw ctl do-await(setup,scope)
      // turn awaits into no-awaits, allowing other strands to execute
      no-await(setup, scope) fn(res)
        ch.emit-io({ rcontext.resume(res) })
      ()
    // passthrough others
    val async-scope = async-scope
    fun no-await(setup,scope,f) no-await(setup,scope,f)
    fun async-iox(f) async-iox(f)
    fun cancel-scope(scope) cancel-scope(scope)
    fun discontinue() discontinue()
  action()