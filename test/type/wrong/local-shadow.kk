struct address
  hostname: string
  port: int

fun log-connect(address: address)
  val port = 80 // shadow of `port` accessor
  println("connecting to port " ++ address.port.show)
