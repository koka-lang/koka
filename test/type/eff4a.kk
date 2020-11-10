// test subtyping etc
fun ioapp( f : () -> <io|e> () ) : <io|e> () {
  return f()
}

fun test() {
  ioapp(fn(){ () })
}
