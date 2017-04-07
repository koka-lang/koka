infixl 1 (>>=)
// --------------------------------------------------------
// Definition of expression
// --------------------------------------------------------

type expr {
  Int( i : int )
  Div( e1 : expr, e2: expr )
}

val expr1 = Int(1)
val expr2 = Div(Div(Int(16),Int(2)),Int(3))
val exprX = Div(Div(Int(16),Int(2)),Int(0))

fun eval0( e : expr ) : int {
  match(e) {
    Int(i) -> i
    Div(e1,e2) -> {
      val x = eval0(e1)
      val y = eval0(e2)
      x / y
    }
  }
}

fun show( e : expr ) : total string {
  match(e) {
    Int(i) -> core/show(i)
    Div(e1,e2) -> "(" + e1.show + "/" + e2.show + ")"
  }
}

// --------------------------------------------------------
// Monadic
// --------------------------------------------------------


alias stexn<a> = int -> either<string,(a,int)>

fun run(s,m) {
  m(s)
}

fun unit(x : a) : stexn<a> {
  return fun(s) { Right((x,s))}
}

fun (>>=)( m : stexn<a>, f : a -> stexn<b> ) : stexn<b> {
  return fun(s0) { 
    match(m(s0)) {
      Right((x,s1)) -> f(x)(s1)
      Left(str)     -> Left(str)
    }
  }
}

fun mtick() {
  return fun(s) { Right(((),s+1))}
}

fun mfail(err) {
  return fun(s) { Left(err) }
}

fun eval( e : expr ) : stexn<int> {
  match(e) {
    Int(i)   -> unit(i)
    Div(x,y) ->
      mtick() >>= (fun(_){
        eval(x) >>= (fun(i) {
          eval(y) >>= (fun(j) {
            (if (j==0) then mfail("division by zero") else unit(())) >>= (fun(_){
               unit( i / j )
      })})})}) 
  }
}

fun show( r : either<string,(int,int)> ) : total string {
  match(r) {
    Right((x,s)) -> x.core/show + " (state=" + s.core/show + ")"
    Left(str)    -> "error: " + str
  }
}



// --------------------------------------------------------
// Exceptions
// --------------------------------------------------------

effect exc {
  failure(s : string) : a
}

val exc = handler {
  return x   -> Right(x)
  failure(s) -> Left(s)
}

fun eval1( e : expr ) : exc int {
  match(e) {
    Int(i) -> i
    Div(e1,e2) -> {
      val x = eval1(e1)
      val y = eval1(e2)
      if (y==0) then failure("divide by zero") else x / y
    }
  }
}

fun test1(e) {
  val r = exc{ eval1(e) }
  println(show1(r))
}


fun showErr( s : a -> string, x : either<string,a> ) : string {
  match(x) {
    Left(l)  -> "exception: " + l
    Right(r) -> s(r)
  }
}

fun show1( x : either<string,int> ) {
  showErr(show,x)
}

// --------------------------------------------------------
// State
// --------------------------------------------------------

effect state {
  tick() : ()
}

val state = handler(i) {
  return x -> (x,i)
  tick()   -> resume(i+1,())
}

fun eval2( e : expr ) : <state,exc> int {
  match(e) {
    Int(i) -> i
    Div(e1,e2) -> {
      val x = eval2(e1)
      val y = eval2(e2)
      tick()
      if (y==0) then failure("divide by zero") else x / y
    }
  }
}

fun test2(e) {
  val res = state(0){ exc{ eval2(e) }}
  res.show2.println
}

fun show2( x : (either<string,int>,int) ) : string {
  "ticks: " + x.snd.show + "\n" + show1(x.fst)
}


// --------------------------------------------------------
// Output
// --------------------------------------------------------

effect out {
  line(e:expr) : ()
}

val out = handler {
  return x -> x
  line(e)  -> {
    println(e.show)
    resume(())
  }
}

fun eval3( e : expr ) : <out,state,exc> int {
  line(e)
  match(e) {
    Int(i) -> i
    Div(e1,e2) -> {
      val x = eval3(e1)
      val y = eval3(e2)
      tick()
      if (y==0) then failure("divide by zero") else x / y
    }
  }
}

fun test3(e) {
  val res = state(0){ out{ exc{ eval3(e) }}}
  res.show2.println
}

fun main() {
  test1(expr2)
  test2(expr2)
  test3(expr2)
}