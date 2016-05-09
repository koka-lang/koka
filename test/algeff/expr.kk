// --------------------------------------------------------
// Definition of players
// --------------------------------------------------------

type expr {
  Int( i : int )
  Div( e1 : expr, e2: expr )
}

function show( e : expr ) {
  match(e) {
    Int(i) -> core/show(i)
    Div(e1,e2) -> "(" + e1.show + "/" + e2.show + ")"
  }
}

val expr1 = Int(1)
val expr2 = Div(Div(Int(10),Int(2)),Int(3))
val exprX = Div(Div(Int(10),Int(2)),Int(0))

function eval0( e : expr ) : int {
  match(e) {
    Int(i) -> i
    Div(e1,e2) -> {
      val x = eval0(e1)
      val y = eval0(e2)
      x / y
    }
  }
}

// --------------------------------------------------------
// Exceptions
// --------------------------------------------------------

effect exc {
  raise<b>(s : string) : b
}

alias eval0 = <exc>

val hexc = handler {
  return x -> Right(x)
  raise(s) -> Left(s)
}

function showErr( s : a -> string, x : either<string,a> ) : string {
  match(x) {
    Left(l)  -> "exception: " + l
    Right(r) -> s(r)
  }
}

function show1( x : either<string,int> ) {
  showErr(show,x)
}

function eval1( e : expr ) : eval0 int {
  match(e) {
    Int(i) -> i
    Div(e1,e2) -> {
      val x = eval1(e1)
      val y = eval1(e2)
      if (y==0) then raise("divide by zero") else x / y
    }
  }
}

function test1(e) {
  val r = hexc{ eval1(e) }
  println(show1(r))
}
// --------------------------------------------------------
// State
// --------------------------------------------------------

effect tick {
  xtick() : ()
}

alias eval2 = <tick,exc>

val htick = handler(i) {
  return x -> (x,i)
  xtick()   -> resume((),i+1)
}

function show2( x : (either<string,int>,int) ) : string {
  "ticks: " + x.snd.show + "\n" + show1(x.fst)
}

function eval2( e : expr ) : eval2 int {
  match(e) {
    Int(i) -> i
    Div(e1,e2) -> {
      val x = eval2(e1)
      val y = eval2(e2)
      xtick()
      if (y==0) then raise("divide by zero") else x / y
    }
  }
}

function test2(e) {
  htick({ hexc{ eval2(e) }},0).show2.println
}


// --------------------------------------------------------
// Output
// --------------------------------------------------------

effect out {
  line(e:expr) : ()
}

alias eval3 = <out,tick,exc>

val hout = handler {
  return x -> x
  line(e)  -> {
    println(e.show)
    resume(())
  }
}

function eval3( e : expr ) : eval3 int {
  line(e)
  match(e) {
    Int(i) -> i
    Div(e1,e2) -> {
      val x = eval3(e1)
      val y = eval3(e2)
      xtick()
      if (y==0) then raise("divide by zero") else x / y
    }
  }
}

function test3(e) {
  htick({ hout{ hexc{ eval3(e) }}},0) .show2.println
}
