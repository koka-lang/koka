
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
    count()
  }
}


fun test-normal(i0,action) {
  var i := i0
  handle({mask<local>(action)}) {
    fun get()  { i }
    fun put(j) { i := j; () }
  }
}

fun test-direct(i) {
  comp();
  if (i==0) then i else test-direct(i - 1)
}

val n = 10000000
fun main() {
  test-direct(n).println
  //test-normal(n,count).println
}
