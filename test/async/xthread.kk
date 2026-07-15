// Thread-safe channel + persistent worker thread (native libuv).
//
// The main loop owns an `xchannel`; a `spawn-thread` worker runs its OWN async
// loop (init loop + async/exn handlers + uv_run) and `xemit`s values across the
// thread boundary through the channel's `sender`. Each cross-thread emit marks
// the value thread-shared and wakes the owner loop via uv_async, which delivers
// it to the waiting `xreceive`. Native only (real OS threads).
import std/async
import std/time/duration
import std/async/api/uv/thread

fun main()
  val x : xchannel<int> = xchannel()      // owned by the MAIN loop
  val s = x.sender                          // the cross-thread SENDER half (no Koka channel)
  spawn-thread(fn()                         // a worker thread with its OWN async loop
    list(1, 5).foreach fn(i)
      s.xemit(i * 10)                        // wake the main loop from another thread
      wait(10.milli-seconds)
  )
  val got = list(1, 5).map(fn(_) x.xreceive)
  println("received: " ++ got.show)          // [10,20,30,40,50], in order
  println("sum: " ++ got.sum.show)           // 150
