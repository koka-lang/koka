// test subtyping etc
fun ioapp( f ) : _e () {
  print("hi")
  return f()
}

fun test() {
  ioapp(fun(){ () })
}
