import std/time

external include {
  js file "perf1-inline1.js"
}

effect state<s> {
  get()      : s;
  put(i : s) : ()
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
    count()
  }
}

external direct_count() : state<int> int {
  js inline "_direct_count(_k)"
}

fun testH(i0,action) {
  handle(action)(i=i0) {
    return x -> x
    get()    -> resume(i,i)
    put(j)   -> resume(j,())
  }
}


fun testD(i) {
  comp();
  if (i==0) then i else testD(i - 1)
}

val n = 10000000
fun main() {
  print-elapsed({testD(n)},             "direct")
  print-elapsed({testH(n,count)},       "handler")
  print-elapsed({testH(n,direct_count)},"internal handler")
}