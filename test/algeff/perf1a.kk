import std/time

fun fib(n) {
  if (n<=1) then 1 else fib(n - 1) + fib(n - 2);
}

fun comp() { 
  fib(4)
}

effect state<s> {
  fun get()      : s
  fun put(i : s) : ()
}
 
fun count() {
  comp()
  val i = get()
  if (i == 0) then i else {
    put(i - 1)
    count()
  }
}

fun testH(i0) {
  handle(count)(i=i0) {
    return x -> x
    get()    -> resume(i,i)
    put(j)   -> resume((),j)
  }
}

effect linear state1<s> {
  fun get1()    : s
  fun put1(i:s) : ()
}

fun count1() {
  comp();
  val i = get1()
  if (i == 0) then i else {
    put1(i - 1)
    count1()
  }
}

fun testH1(i0) {
  handle(count1)(i=i0) {
    return x -> x
    get1()   -> resume(i,i)
    put1(j)  -> resume((),j)
  }
}

fun testD(i) {
  comp();
  if (i==0) then i else testD(i - 1)
}

fun testR(n) {
  var i := n;
  fun countR() {
    comp()
    if (i == 0) then i else {
      i := i - 1
      countR()
    }
  }
  countR()
}


val n = 2000000
fun main() {
  print-elapsed({testD(n)}, "direct         ")
  print-elapsed({testR(n)}, "recursive      ")
  print-elapsed({testH1(n)},"linear handler ")
  print-elapsed({testH(n)}, "regular handler")
}