// test subtyping etc
fun ioapp( f ) : io () {
  return f()
}

fun test() {
  ioapp(fun(){ () })
}
