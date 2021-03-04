// Test inject over handled effects

effect heap<a> {
  fun get() : a
  fun set(value : a) : ()
}

fun new-ref(init,action) {
  var s := init
  with {  
    fun get(){ s }
    fun set(x){ s := x }
  }
  action()
}

fun test() {
  with new-ref(2)
  with new-ref(44)
  (get() : int) - mask<heap<int>>{get()}  // 44 - 2  
}

fun main() {
  println(test())
}
