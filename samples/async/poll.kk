import uv/poll
import uv/event-loop
import std/async

fun main()
  val poll = uv-fs-poll-init().untry
  poll.uv-fs-poll-start("scratch/test.kk", 1000) fn (res)
    match res
      Ok((old, new)) -> 
        old.fstat/show.trace
        new.fstat/show.trace
      Error(err) -> 
        err.show.trace
  wait(5.seconds)
  poll.uv-fs-poll-stop()
  ()
