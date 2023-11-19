pub import std/os/uv
import std/os/net
import std/time
import std/os/stream
import std/num/int32

fun main()
  val str = "GET / HTTP/1.1\r\n\r\n"
  val client = tcp-init().untry
  val server = tcp-init().untry
  val sa = SockAddr(AF_INET, "0.0.0.0", Just(8000.int32))
  server.bind(sa, 0.int32)
  server.as-stream.listen(0.int32) fn(err)
    match tcp-init()
      Ok(x) ->
        server.as-stream.accept(x.as-stream)
        x.as-stream.read-start fn(buf)
          ("Server got: " ++ buf.to-string()).println
          x.as-stream.write([buf]) fn(e)
            ("Write: " ++ e.message).println
            x.as-stream.read-stop()
            x.as-stream.shutdown fn(ers2)
              ("Shutdown3: " ++ ers2.message).println
              x.uvHandle.close fn()
                ("Close3").println
            ()
        ()
      _ -> ()
  val a = SockAddr(AF_INET, "127.0.0.1", Just(8000.int32))
  val er = client.connect(a) fn (err)
    ("Connect: " ++ err.message).println
    client.as-stream.read-start fn (buf)
      ("Server got: " ++ buf.to-string()).println
      client.as-stream.read-stop()
      server.uvHandle.close fn()
        ("Close1").println
      client.as-stream.shutdown fn(ers2)
        ("Shutdown2: " ++ ers2.message).println
        client.uvHandle.close fn()
          ("Close2").println
      ()
    client.as-stream.write([str.from-string]) fn(er2)
      ("Write Sync: " ++ er2.message).println
  ("Connect Sync: " ++ er.message).println