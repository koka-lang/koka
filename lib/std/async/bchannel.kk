/*---------------------------------------------------------------------------
  Copyright 2026, Tim Whiting, Microsoft Research, Daan Leijen.

  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the LICENSE file at the root of this distribution.
---------------------------------------------------------------------------*/

/* Bounded (windowed, batching) cross-thread channels with backpressure.

A `bchannel` layers flow control over the unbounded thread-safe `xchannel`
using a two-channel topology, entirely in portable Koka:

- data flows receiver<-producer as *batches* through one `xchannel` (owned by
  the receiver's loop),
- grants flow producer<-receiver as *ints* through a second `xchannel` (owned
  by the producer's loop) that acts as the producer's ack mailbox.

Each batch carries the sender of its producer's ack mailbox, so the receiver
can attribute consumption back to the right producer without any registry. A
grant is "how many more items I am willing to accept": the receiver grants a
batch's size back once that batch is fully *consumed* (not merely delivered),
so at most `window` items per producer are unconsumed anywhere in the system
-- the C queue, the receiver's Koka channel, and the current batch combined.
Grants are POOLED per producer and cross only in half-window chunks (plus a
final flush at `bchannel/close`): granting per batch would let an eager
producer lock into size-1 batches with a reverse crossing per item (measured:
~50x slower than the unbounded xchannel; with pooled grants a saturated
stream runs half-window batches both ways and measures FASTER than xchannel).
Pooling cannot stall a suspended producer: the receiver only ever waits when
it has consumed everything delivered, and a suspended producer has a full
window outstanding, so consuming it necessarily crosses the half-window
threshold and fires a grant first.

The producer ships eagerly while it has demand (low latency when the receiver
keeps up) and buffers loop-locally while it does not; a granted producer ships
its whole backlog as a single transfer (one `mark_shared` traversal, one mutex
acquisition, one `uv_async_send` for the entire batch). Batches are capped at
half the window so consumption pipelines with transfer (double buffering).
`emit` suspends -- awaiting grants, never blocking the OS thread -- once the
local backlog reaches `cap`: that is the backpressure point.

Push vs demand is purely the receiver's grant policy: the ack is an int, so a
receiver may grant less (shrink the window when it is backed up) or more
(grow it) than it consumed. The default policy regrants exactly what it
consumes, keeping the window constant.

Typical use (the `bconnect` token is the only thing that crosses the spawn):

    val b : bchannel<int> = bchannel(64)   // on the receiving loop
    val c = b.connect
    spawn-thread(fn()
      val s = c.sender                     // on the producer's own loop
      list(1,1000).foreach fn(i) s.emit(i)
      s.close                              // flush + await full window
    )
    val xs = list(1,1000).map(fn(_) b.receive)
    b.close
*/
module async/bchannel

import async/async
import async/channel
import async/thread

// A batch crossing the thread boundary: the items (oldest first), the sender of
// the producer's ack mailbox, and the producer's id (so the receiver can pool
// grants per producer -- `xsender`s themselves are not comparable).
abstract value struct batch<a>( id : int, ack : xsender<int>, items : list<a> )

// Receiver-side cursor into the batch currently being consumed.
abstract struct bcursor<a>( id : int, ack : xsender<int>, remaining : list<a>, size : int )

// A bounded channel (the RECEIVER half); lives on the loop that created it.
// `grants` pools consumed-counts per producer: granting per BATCH would let an
// eager producer degenerate into size-1 batches forever (each ack(1) refills
// demand for exactly one item), so grants are held until they reach half a
// window and cross as one int (`close` flushes any sub-threshold remainder).
abstract struct bchannel<a>(
  data   : xchannel<batch<a>>,
  window : int,
  cursor : ref<global, maybe<bcursor<a>>>,
  grants : ref<global, list<(int, xsender<int>, int)>>   // (producer id, ack, pooled grant)
)

// Create a bounded channel on the current loop. `window` is the maximum number
// of unconsumed items each producer may have in flight.
pub fun bchannel( window : int = 64 ) : <async,ioc> bchannel<a>
  Bchannel(xchannel(), max(window, 1), ref(Nothing), ref([]))

// The portable connection token: hand THIS across a `spawn-thread` (never the
// `bchannel`, which holds receiver-loop state). Carries only the cross-thread
// data sender and the agreed window; abstract so the transport (currently an
// `xchannel` of batches) stays an implementation detail.
abstract value struct bconnect<a>( data : xsender<batch<a>>, window : int )

pub fun bchannel/connect( b : bchannel<a> ) : bconnect<a>
  Bconnect(b.data.sender, b.window)

// The producer half; MUST be created (via `bconnect/sender`) on the producer's
// own loop, since its ack mailbox is an `xchannel` owned by that loop. All of
// its state is loop-local.
abstract struct bsender<a>(
  id        : int,                        // this producer's identity in receiver grant pooling
  data      : xsender<batch<a>>,
  window    : int,
  cap       : int,                        // local backlog bound: emit suspends at/above this
  acks      : xchannel<int>,              // grant mailbox, delivered on THIS loop
  ack-send  : xsender<int>,               // crosses with every batch
  demand    : ref<global, int>,           // items we may still ship
  pending   : ref<global, list<a>>,       // local backlog, newest first
  pending-n : ref<global, int>
)

// Open the producer half on the CURRENT (producer) loop. `cap` bounds the
// local backlog (`emit` suspends once it is reached); it defaults to the
// window, bounding this producer's total footprint to about two windows.
pub fun bconnect/sender( c : bconnect<a>, cap : int = 0 ) : <async,ioc> bsender<a>
  val acks : xchannel<int> = xchannel()
  val capx = if cap <= 0 then c.window else cap
  Bsender(unique(), c.data, c.window, capx, acks, acks.sender, ref(c.window), ref([]), ref(0))

// Fold any grants that arrived while we were busy into `demand` (never waits).
fun drain-acks( s : bsender<a> ) : <async,ioc> ()
  match s.acks.try-xreceive
    Just(n) ->
      val d = s.demand
      d := !d + n
      drain-acks(s)
    Nothing -> ()

// Suspend this strand until the next grant arrives (the producer's loop keeps
// running other strands; no OS thread blocks).
fun await-ack( s : bsender<a> ) : <async,ioc> ()
  val n = s.acks.xreceive
  val d = s.demand
  d := !d + n

// Ship as much backlog as demand allows, oldest first, in batches of at most
// half a window (so the receiver consumes one batch while the next crosses).
fun ship( s : bsender<a> ) : <async,ioc> ()
  val rd = s.demand
  val rp = s.pending
  val rn = s.pending-n
  val d = !rd
  val n = !rn
  if d > 0 && n > 0 then
    val k = min(d, min(n, max(1, s.window / 2)))
    val items = (!rp).reverse
    rp := items.drop(k).reverse
    rn := n - k
    rd := d - k
    s.data.xemit(Batch(s.id, s.ack-send, items.take(k)))
    ship(s)

// Emit from the producer. Ships eagerly while the receiver has granted demand;
// otherwise buffers loop-locally. Suspends (awaiting grants) once the local
// backlog reaches `cap` -- this is where backpressure bites.
pub fun bsender/emit( s : bsender<a>, v : a ) : <async,ioc> ()
  val rp = s.pending
  val rn = s.pending-n
  rp := Cons(v, !rp)
  rn := !rn + 1
  drain-acks(s)
  ship(s)
  if !rn >= s.cap then emit-wait(s)

fun emit-wait( s : bsender<a> ) : <async,ioc> ()
  await-ack(s)
  ship(s)
  val rn = s.pending-n
  if !rn >= s.cap then emit-wait(s)

// Ship the entire backlog, awaiting grants as needed, until nothing is pending.
pub fun bsender/flush( s : bsender<a> ) : <async,ioc> ()
  drain-acks(s)
  ship(s)
  val rn = s.pending-n
  if !rn > 0 then
    await-ack(s)
    flush(s)

// Flush, await the full window back (every shipped item consumed and granted),
// then close the ack mailbox. After this the receiver holds no live reference
// into the producer's loop, so both loops can drain and exit.
pub fun bsender/close( s : bsender<a> ) : <async,ioc> ()
  s.flush
  val rd = s.demand
  fun settle()
    if !rd < s.window then
      await-ack(s)
      settle()
  settle()
  s.acks.close

// Pool `k` consumed items for producer `id`, granting (one int crossing) once
// the pool reaches half a window. Held-back remainders are released by
// `flush-grants` when the receiver idles or closes.
fun add-grant( b : bchannel<a>, id : int, ack : xsender<int>, k : int ) : <async,ioc> ()
  val rg = b.grants
  val threshold = max(1, b.window / 2)
  fun bump( gs : list<(int, xsender<int>, int)> ) : ioc list<(int, xsender<int>, int)>
    match gs
      Cons((gid, gack, p), rest) ->
        if gid == id then
          val p2 = p + k
          if p2 >= threshold then
            gack.xemit(p2)
            Cons((gid, gack, 0), rest)
          else
            Cons((gid, gack, p2), rest)
        else
          Cons((gid, gack, p), bump(rest))
      Nil ->
        if k >= threshold then
          ack.xemit(k)
          [(id, ack, 0)]
        else
          [(id, ack, k)]
  rg := bump(!rg)

// Release every pooled grant. Called only at `bchannel/close`: it lets a
// producer waiting in `bsender/close` settle its sub-threshold remainder. It
// must NOT run on every receiver wait -- that reintroduces a reverse crossing
// per item under light load (the measured 50x ping-pong pathology).
fun flush-grants( b : bchannel<a> ) : <async,ioc> ()
  val rg = b.grants
  rg := (!rg).map fn((id, ack, p))
    if p > 0 then ack.xemit(p)
    (id, ack, 0)

// Receive on the receiver's loop. Consumed counts are pooled per producer and
// granted back in half-window chunks, waking suspended producers.
pub fun bchannel/receive( b : bchannel<a> ) : <async,ioc> a
  val cursor = b.cursor
  match !cursor
    Just(cur) -> match cur.remaining
      Cons(v, rest) ->
        match rest
          Nil ->
            cursor := Nothing
            add-grant(b, cur.id, cur.ack, cur.size)
          _   ->
            cursor := Just(cur( remaining = rest ))
        v
      Nil ->
        cursor := Nothing               // defensive: producers never ship empty
        b.receive
    Nothing ->
      match b.data.try-xreceive
        Just(bt) ->
          cursor := Just(Bcursor(bt.id, bt.ack, bt.items, bt.items.length))
        Nothing  ->
          val bt = b.data.xreceive
          cursor := Just(Bcursor(bt.id, bt.ack, bt.items, bt.items.length))
      b.receive

// Close the receiver half once every producer has `close`d (or will consume no
// further grants). Pooled grants are flushed first so a producer suspended in
// `close`/`flush` can settle; producer ack mailboxes close in `bsender/close`.
pub fun bchannel/close( b : bchannel<a> ) : <async,ioc> ()
  flush-grants(b)
  b.data.close
