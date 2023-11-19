
pub import std/os/uv
import std/os/net-async
import std/os/net
import std/os/stream
import std/num/int32
import std/async

fun handleStream(connection)
  val buf = connection.read
  connection.write(buf) 
  connection.shutdown()

fun handleConnections(connections)
  match connections.receive
    Ok(c) -> handleStream(c)
    _ -> throw("Error accepting connection")

fun main()
  val str = "GET / HTTP/1.1\r\n\r\n"
  val server = tcp()
  server.bind("0.0.0.0", 8000)
  val connections = server.listen
  interleaved({while({True}, {handleConnections(connections)})}) fn()
    val stream = tcp().connect("127.0.0.1", 8000)
    stream.write(str.from-string)
    val buf = stream.read
    buf.to-string().println
    server.shutdown()
    stream.shutdown() // also closes the tcp client handle
  println("Done")