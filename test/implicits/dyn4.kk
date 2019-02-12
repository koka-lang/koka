type tree<a> {
  con Bin(value:a,left:tree<a>, right:tree<a>)
  con Leaf
}

dynamic<a> fun yield(value:a) : ()

fun iterate(t) {
  match(t) {
    Leaf -> ()
    Bin(x,l,r) {
      iterate(l)
      yield(x)
      iterate(r)
    }
  }
}

fun tip(x) { Bin(x,Leaf,Leaf)}
val tree5 = Bin(2,tip(1),Bin(4,tip(3),tip(5)))

fun test1() {
  with fun yield(x:int) { print(x) }
  tree5.iterate
}

fun print4(action) {
  var count := 0
  with effect yield(x:int) {
    if (count >= 4) then () else {
      count := count + 1
      print(x)
      resume(())
    }
  }
  action()
}

fun test2() {
  with print4
  tree5.iterate
}


fun fiterate(t,f) {
  match(t) {
    Leaf -> ()
    Bin(x,l,r) {
      l.fiterate(f)
      if (inject<yield>{f(x)}) then yield(x)
      // if (f(x)) then yield(x)
      r.fiterate(f)
    }
  }
}

fun fsize1(t,f) {
  var count := 0
  val _ = with fun yield(x) { count := count + 1 } do fiterate(t,f)
  count
}

fun fsize2(t,f) {
  match(t) {
    Leaf -> 0
    Bin(x,l,r) {
      val lc = l.fsize2(f)
      val rc = r.fsize2(f)
      val c  = if (f(x)) then 1 else 0
      c + lc + rc
    }
  }
}

fun test3a() {
  val fsize = fsize1
  fun g(x:int) { yield(x); even?(x) }
  with fun yield(x:int) { print(x) }
  tree5.fsize(g)
}

fun test3b() {
  val fsize = fsize2
  fun g(x:int) { yield(x); even?(x) }
  with fun yield(x:int) { print(x) }
  tree5.fsize(g)
}

fun main() {
  println( ", even count a: " + test3a().show )
  println( ", even count b: " + test3b().show )
}
