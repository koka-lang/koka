effect fun read(): int
effect fun write(): (() -> read int)

fun f() { 
  write()
}

fun g() : console (() -> <read> int) {
  println("in write handler") 
  return (fn(){ read() }) // () -> <read> ()  <<h : read>>  ev[0].read
}
fun main() : console int {
  with fun write() { 
    g();
  }
  with fun read(){ 42 } 
  // f(1) : () -> <read> int
  // ~> <read|write>
  // << h1 : write, h2 : read >>
  f()()
}