
effect linear state<s> {
  get() : s;
  put(x : s) : ()
}

val state = handler(s) {
  return x -> x
  get()    -> resume(s,s)
  put(t)   -> 20 + resume(t,()) 
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