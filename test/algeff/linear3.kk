
effect state<s> {
  ctl get() : s
  ctl put(x : s) : ()
}

fun state(init,action) {
  var s := init
  with {
    return(x){ x + 2 }
    fun get(){ s }
    ctl put(t){ 42 } 
  }
  action()
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