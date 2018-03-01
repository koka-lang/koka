// Test inject over handled effects

effect heap<a> {
  fun get() : a 
  fun set(value : a) : ()
}

val new-ref = handler(s) {
  get() -> resume(s,s)
  set(x) -> resume((),x)
}

fun test() {
  new-ref(2) {
    new-ref(44) {
      (get() : int) - inject<heap<int>>{get()}  // 44 - 2
    }
  }
}

fun main() {
  println(test())
}