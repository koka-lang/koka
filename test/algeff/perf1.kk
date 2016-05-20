import std/date

external include {
  js file "perf1-inline1.js"
} 

function timer(f) {
  val t0 = now().time
  val adjust = now().time - t0
  println("start")
  val start = now().time
  val x = f()
  val end = now().time
  println("ended in " + (end - start - adjust).show + "ms")
  x
}

effect state<s> {
  get()      : s;
  put(i : s) : ()
}

function fib(n) {
  if (n<=1) then 1 else fib(n - 1) + fib(n - 2);
}

function comp() { 
  fib(5)
}
 
function count() {
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

function testH(i0,action) {
  handle(action)(i=i0) {
    return x -> x
    get()    -> resume(i,i)
    put(j)   -> resume(j,())
  }
}


function testD(i) {
  comp();
  if (i==0) then i else testD(i - 1)
}

function main() {
  timer({testD(10000000)}).show
  timer({testH(10000000,direct_count)}).show
  timer({testH(10000000,count)}).show
}