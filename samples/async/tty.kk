import uv/tty
import uv/stream
import std/num/int32

fun main()
  with default-event-loop
  val uv = uv-tty-init(0.int32).untry
  uv.stream.try-write(["Hello, world!\n".bytes])
  val size = uv.uv-tty-get-winsize().untry
  println("Window size is " ++ size.show)