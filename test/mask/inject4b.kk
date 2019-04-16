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
  new-ref("hi") {
    new-ref(44) {
      // inject without giving parameters to `heap`
      get() - inject<heap>{(get():string).count}  // 44 - "hi".count
    }
  }
}

fun main() {
  println(test())
}