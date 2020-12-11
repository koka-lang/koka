// test subtyping etc
fun ioapp( f ) :  () {
  return f()
}

fun test() {
  ioapp(fn(){ () })
}
