
effect linear state<s> {
  fun get() : s
  fun put(x : s) : ()
}

val state = handler(s) {
  return x -> x + 2
  get()    -> resume(s,s)
  put(t)   -> 42
}

fun foo() {
  val i = get()
  put(i+1)
  put(i+2)
  get()
}

fun main() {
  state(0,foo).println  // 42
}