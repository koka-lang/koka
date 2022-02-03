
effect state<s> {
  ctl get() : s
  ctl put(x : s) : ()
}

fun state(init,action) {
  var s := init
  with {
    fun get(){ s }
    ctl put(t){ s := t; val x = resume(()); s := t; 20 + x + resume(()) } 
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
  state(0,foo).println  // 68
}