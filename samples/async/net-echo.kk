pub import std/os/uv
import std/os/net
import std/time
import std/os/stream
import std/num/int32

// Async Library, but using low level uv functions from std/os/net instead of the high level interface in std/os/net-async

fun main()
  val str = "GET / HTTP/1.1\r\n\r\n"
  val client = tcp-init().untry
  val server = tcp-init().untry
  val sa = SockAddr(AF_INET, "0.0.0.0", Just(8000.int32))
  server.bind(sa, 0.int32)
  server.stream.listen(0.int32) fn(err)
    match tcp-init()
      Ok(x) ->
        server.stream.accept(x.stream)
        x.stream.read-start fn(buf)
          ("Server got: " ++ buf.string).println
          x.stream.write([buf]) fn(e)
            ("Write: " ++ e.message).println
            x.stream.read-stop()
            x.stream.shutdown fn(ers2)
              ("Shutdown3: " ++ ers2.message).println
              x.uv-handle.close fn()
                ("Close3").println
            ()
        ()
      _ -> ()
  val a = SockAddr(AF_INET, "127.0.0.1", Just(8010.int32))
  val er = client.connect(a) fn (err)
    ("Connect: " ++ err.message).println
    client.stream.read-start fn (buf)
      ("Server got: " ++ buf.string).println
      client.stream.read-stop()
      server.uv-handle.close fn()
        ("Close1").println
      client.stream.shutdown fn(ers2)
        ("Shutdown2: " ++ ers2.message).println
        client.uv-handle.close fn()
          ("Close2").println
      ()
    client.stream.write([str.bytes]) fn(er2)
      ("Write Sync: " ++ er2.message).println
  ("Connect Sync: " ++ er.message).println