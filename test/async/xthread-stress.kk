// Regression test for cross-thread heap corruption (thread-shared refcounts).
//
// Historically three distinct bugs corrupted the heap once a second OS thread ran
// Koka code concurrently (spawn-thread / std/async/thread):
//  1. the empty evidence-vector singleton was a plain-refcounted heap block shared
//     by every thread (now the static KK_HEADER_STATIC block),
//  2. `kk_block_make_shared` mapped rc `N` (= N+1 refs) to `-N` (= N refs), losing
//     one reference for every marked block with rc >= 1 (now `~rc`),
//  3. string literals and computed module toplevel constants live in C statics but
//     had PLAIN refcounts, dup/dropped non-atomically from every thread (literals
//     are now stuck via `kk_string_literal_init`; computed toplevels are marked
//     thread-shared at module init).
//
// Part 1 hammers shared constants/literals from both threads concurrently
// (`duration.show` churn -- the historical crasher at ~50% of runs); part 2 is the
// original xchannel repro: a worker allocating concurrently (wait) while streaming
// boxed structs to main.
import std/async
import std/time/duration
import std/async/thread

struct model( ticks : int, result : maybe<int> )

fun churn( n : int, acc : int ) : ioc int
  if n <= 0 then acc else
    val s = (n.milli-seconds).show      // dup/drops shared decimal/duration constants + literals
    churn(n - 1, acc + s.count)

fun sender-loop( out : xsender<model>, n : int ) : <async,ioc,exn> ()
  if n >= 200 then () else
    wait(1.milli-seconds)               // concurrent allocation on the worker heap
    out.xemit(Model(n, if n > 3 then Just(n) else Nothing))
    sender-loop(out, n + 1)

fun main()
  // part 1: both threads churn the same shared constants/literals
  val c : xchannel<int> = xchannel()
  val csend = c.sender
  spawn-thread(fn()
    with handle/try fn(err) println("worker exn: " ++ err.show)
    csend.xemit(churn(10000, 0))
  )
  val m = churn(10000, 0)
  val w = c.xreceive
  println("churn: main=" ++ m.show ++ " worker=" ++ w.show ++ (if m == w then " ok" else " MISMATCH"))
  c.close
  // part 2: boxed structs cross while the worker keeps allocating
  val x : xchannel<model> = xchannel()
  val out = x.sender
  spawn-thread(fn()
    with handle/try fn(err) println("worker exn: " ++ err.show)
    sender-loop(out, 0)
  )
  val got = list(1, 200).map(fn(_) x.xreceive.ticks)
  println("stream: received " ++ got.length.show ++ ", sum=" ++ got.sum.show)
  x.close
