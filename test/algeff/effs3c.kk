effect amb {
  ctl flip() : bool
}

effect state<s> {
  fun get()    :  s;
  fun set(x:s) : ()
}

fun xor() : amb bool {
  val p = flip() 
  val q = flip()
  (p||q) && not(p&&q)
}

fun xors() : amb string {
  show(xor())
}

fun foo() : <amb,state<int>> bool {
  val p = flip() 
  val i = get()
  set(i+1)
  if (i>0 && p) then xor() else False
}         

fun foos() : <amb,state<int>> string {
  show(foo())
}    
 
fun amb_handle(action : () -> <amb|e> a) : e list<a> {
  with {
    return(x)  { [x] }
    ctl flip() { resume(False) ++ resume(True) }
  }
  action()
}

fun state_handle( init, action ) {
  var s : int := init   
  with {
    fun get()  { s }
    fun set(i) { s := i }
  }
  action()
}

fun test1()  {
  amb_handle(xors).join(",")
}


fun test2()  {
  state_handle(0,{amb_handle(foos)}).join(",")
}

fun test3()  {
  amb_handle({state_handle(0,foos)}).join(",")
}

fun main() {
  [test1(),test2(),test3()].join("\n").println
}