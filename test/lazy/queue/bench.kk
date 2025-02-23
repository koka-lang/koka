module bench

import std/core/undiv
import std/core/unsafe
import std/os/env
import std/num/random
import std/num/int32
import std/num/int64


pub alias rndstate = sfc
alias rndres = sfc-result

fun rnd-step( r : rndstate ) : rndres
  sfc-step(r)

fun rnd-init( s0 : int, s1 : int ) : rndstate
  (sfc-init32(s0.int32,s1.int32))

pub value struct queueI<q>
  qempty  : q
  qsnoc   : (q,int) -> div q
  quncons : q -> div maybe2<int,q>

fun bench-snoc( i : int32, qi : queueI<q>, rs : rndstate, queue : q ) : div q
  if i > 0.int32 then
    val step = rnd-step(rs)
    val q' = (qi.qsnoc)(queue, step.rnd.int)
    bench-snoc(i - 1.int32, qi, step.rstate, q')
  else queue

fun bench-pass-on( i : int32, from : q, to : q, qi : queueI<q>) : div q
  if i > 0.int32 then
    match (qi.quncons)(from)
      Just2( x, from' ) ->
        val to' = (qi.qsnoc)(to, x)
        bench-pass-on(i - 1.int32, from', to', qi)
      Nothing2 -> impossible("uncons failed")
  else
    to

fun bench-uncons( i : int32, queue : q, qi : queueI<q>, acc = 0.int32 ) : div int32
  if i > 0.int32 then
    match (qi.quncons)(queue)
      Just2( x, q ) -> bench-uncons(i - 1.int32, q, qi, acc + i*x.int32)
      Nothing2 -> impossible("uncons failed")
  else acc

fun bench-iterate( i : int32, n : int32, queue : q, qi : queueI<q>) : div int32
  if n > 1.int32 then
    val q = bench-pass-on(i, queue, qi.qempty, qi)
    bench-iterate(i, n - 1.int32, q, qi)
  else
    bench-uncons(i, queue, qi)

pub fun bench( ops : int32, n : int32, qi : queueI<q> ) : _ int32
  val q = bench-snoc( ops, qi, rnd-init(42,43), qi.qempty)
  bench-iterate( ops, n, q, qi )

pub fun benchmain( qi : queueI<q>, ops : int = 100000, queues : int = 100) : io ()
  val n = get-args().head("").parse-int.default(queues).int32
  val sum = bench(ops.int32, n * 10.int32, qi) - 1097638789.int32
  println("Checksum: " ++ show(sum))

