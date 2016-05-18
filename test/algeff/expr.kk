infixl 1 (>>=)
// --------------------------------------------------------
// Definition of expression
// --------------------------------------------------------

type expr {
  Int( i : int )
  Div( e1 : expr, e2: expr )
}

function show( e : expr ) : total string {
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
// Monadic
// --------------------------------------------------------


alias stexn<a> = int -> either<string,(a,int)>

function run(s,m) {
  m(s)
}

function show( r : either<string,(int,int)> ) : total string {
  match(r) {
    Right((x,s)) -> x.core/show + " (state=" + s.core/show + ")"
    Left(str)    -> "error: " + str
  }
}

function unit(x : a) : stexn<a> {
  return fun(s) { Right((x,s))}
}

function (>>=)( m : stexn<a>, f : a -> stexn<b> ) : stexn<b> {
  return fun(s0) { 
    match(m(s0)) {
      Right((x,s1)) -> f(x)(s1)
      Left(str)     -> Left(str)
    }
  }
}

function tick() {
  return fun(s) { Right(((),s+1))}
}

function fail(err) {
  return fun(s) { Left(err) }
}

function eval( e : expr ) : stexn<int> {
  match(e) {
    Int(i)   -> unit(i)
    Div(x,y) ->
      tick() >>= (fun(_){
        eval(x) >>= (fun(i) {
          eval(y) >>= (fun(j) {
            (if (j==0) then fail("division by zero") else unit(())) >>= (fun(_){
               unit( i / j )
      })})})}) 
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
  xtick()   -> resume(i+1,())
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
  htick(0,{ hexc{ eval2(e) }}).show2.println
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
  htick(0,{ hout{ hexc{ eval3(e) }}}) .show2.println
}

function main() {
  test1(expr2)
  test2(expr2)
  test3(expr2)
}