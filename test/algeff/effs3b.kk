effect amb {
  flip() : bool
}

fun mmap( f, xs ) {
  match(xs) {
    Nil -> Nil
    Cons(x,xx) -> Cons(f(x),mmap(f,xx))
  }
}

fun xor() : amb bool {
  val p = flip() 
  val q = flip()
  (p||q) && not(p&&q)
}

fun xorss() {
  val xs = mmap( fun(i) { xors() }, [1,2] )
  xs.join(",")
}

fun upcast1() {
  val xs = mmap( id, ["x","y"]) // id is getting cps transformed
  if (flip()) then xs.join(",") else "darn"
}

fun upcast2() {
  mmap(id,["x","y"]).join(",")  // mmap should use a default k, and id should be transformed...
}

fun xors() : amb string {
  show(xor())
}
 
fun amb_handle() {
  handler {
    return x -> [x]
    flip()   -> resume(False) + resume(True)
  }
}

fun main() {
  amb_handle()(xors).join(",").println
}