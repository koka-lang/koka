import std/date

function timer(f) {
  val t0 = now().time
  val adjust = now().time - t0
  println("start")
  val start = now().time
  val x = f()
  val end = now().time
  println("ended in " + (end - start).show + "ms")
  x
}

effect state {
  get()        : int;
  put(i : int) : ()
}
 
function count() {
  val i = get()
  if (i == 0) then return i else {
    put(i-1)
    count()
  }
}

function test(i0) {
  handle(count,i0)(i) {
    return x -> x
    get()    -> resume(i,i,True)
    put(j)   -> resume((),j,True)
  }
}


function testD(i) {
  if (i==0) then i else testD(i-1)
}

function main() {
  timer({test(10000000)}).show
  timer({testD(100000000)}).show
}