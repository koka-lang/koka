import std/time/timer


extern inline tailcall(action : () -> e a) : e a {
  js "$std_core._tailcall"
}

effect state<s> {
  fun get()      : s
  fun put(i : s) : ()
}

fun fib(n) {
  if (n<=1) then 1 else fib(n - 1) + fib(n - 2);
}

fun comp() { 
  fib(4)
}
 
fun count() {
  comp();
  val i = get()
  if (i == 0) then i else {
    put(i - 1)
    tailcall(count)
  }
}


fun test-tail(i0,action) {
  handle(action)(i=i0) {
    return x -> x
    get()    -> resume(i,i)
    put(j)   -> resume(j,())
  }
}

fun test-nontail(i0,action) {
  handle(action)(i=i0) {
    return x -> x
    get()    -> id(resume(i,i))
    put(j)   -> id(resume(j,()))
  }
}

fun testD(i) {
  comp();
  if (i==0) then i else testD(i - 1)
}

val n = 1000000
fun main() {
  print-elapsed({testD(n)},             "direct")
  print-elapsed({test-tail(n,count)},   "tail-handler")
  print-elapsed({test-nontail(n,count)},"non-tail handler")
}