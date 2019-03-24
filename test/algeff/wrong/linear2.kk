public module wrong/linear2

effect amb {
  fun flip() : bool
}

val amb = handler {
  return x -> [x]
  flip()   -> resume(True) + resume(False)
}

effect linear state<s> {
  fun get() : s
  fun put(x : s) : ()
}

val state = handler(s) {
  return x -> x
  get()    -> resume(s,s)
  put(t)   -> if (flip()) then resume((),t) else resume((),s) 
}

fun foo() {
  val i = get()
  put(i+1)
  put(i+2)
  get()
}

public fun main() {
  amb({state(0,foo)}).showList(show).print  // 42
}