effect amb {
  flip() : bool
}

effect state<s> {
  get()  : s;
  set(i:s) : ()
}

fun xor() : amb bool {
  val p = flip() 
  val q = flip()
  (p||q) && not(p&&q)
}


fun foo() : <amb,state<int>> bool {
  val p = flip() 
  val i = get()
  set(i+1)
  if (i>0 && p) then xor() else False
}         

 
// handlers

fun amb_handle() {
  handler {
    return x -> [x]
    flip()   -> resume(False) + resume(True)
  }
}

fun state_handle() {
  handler(i) {
    return x -> x
    get()    -> resume(i,i)
    set(j)   -> resume(j,()) 
  }
}

fun test1() {
  amb_handle()(xor)
}


fun test2()  {
  state_handle()(0){amb_handle()(foo)}
}

fun test3()  {
  amb_handle()({state_handle()(0,foo)})
}

fun main() {
  test1().show.println
  test2().show.println
  test3().show.println
}