// Bounded channel with MULTIPLE producer threads.
//
// Each producer opens its own sender (own ack mailbox, own window/backlog) on
// its own loop; batches carry their producer's ack sender, so grants minted by
// `receive` wake the right producer. Interleaving across producers is
// arbitrary, but each producer's items must arrive in FIFO order and nothing
// may be lost or duplicated.
import std/async
import std/async/thread
import std/async/bchannel

fun producer( c : bconnect<(int,int)>, id : int, n : int ) : <async,ioc> ()
  val s = c.sender
  list(1, n).foreach fn(i)
    s.emit((id, i))
  s.close

fun main()
  val b : bchannel<(int,int)> = bchannel(16)
  val c = b.connect
  spawn-thread(fn() producer(c, 1, 1000))
  spawn-thread(fn() producer(c, 2, 1000))
  val got = list(1, 2000).map(fn(_) b.receive)
  val one = got.filter(fn(x) x.fst == 1).map(fn(x) x.snd)
  val two = got.filter(fn(x) x.fst == 2).map(fn(x) x.snd)
  println("p1 in order: " ++ (one == list(1, 1000)).show ++
          ", p2 in order: " ++ (two == list(1, 1000)).show)
  b.close
