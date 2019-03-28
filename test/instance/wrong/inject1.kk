// Test inject over handled effects

effect heap<a> {
  fun get() : a 
  fun set(value : a) : ()
}

val new-ref = handler(s) {
  get() -> resume(s,s)
  set(x) -> resume((),x)
}

fun main() {
  new-ref("hi") {
    new-ref(44) {    
      get() - inject<int>{(get():string).count}  // 44 - "hi".count
    }
  }
}