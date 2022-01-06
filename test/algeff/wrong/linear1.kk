module wrong/linear1

linear effect state<s> {
  fun get() : s
  fun put(x : s) : ()
}

fun state(init,action) 
  var s := init
  handle(action)
    fun get()  { s }
    ctl put(t) { s := t; val x : int = resume(()); s := t; x + resume(()) } 


fun foo() {
  val i = get()
  put(i+1)
  put(i+2)
  get()
}

pub fun main() {
  state(0,foo).print  // 42
}