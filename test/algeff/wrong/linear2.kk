module wrong/linear2

effect amb {
  ctl flip() : bool
}

val amb = handler {
  return(x)  -> [x]
  ctl flip() -> resume(True) ++ resume(False)
}

linear effect state<s> {
  fun get() : s
  fun put(x : s) : ()
}

fun state(init,action) 
  var s := init
  handle(action)
    fun get()  { s }
    fun put(t) { if flip() then s := t; () }

fun foo() {
  val i = get()
  put(i+1)
  put(i+2)
  get()
}

pub fun main() {
  amb({state(0,foo)}).showList(show).print  // 42
}