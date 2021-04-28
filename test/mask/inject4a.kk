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
  with new-ref("hi") 
  with new-ref(44) 
  // heap<int>,heap<string>|e>
  get() - mask<heap<int>>{(get():string).count}  // 44 - "hi".count  
}

fun main() {
  println(test())
}