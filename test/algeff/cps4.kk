infixl 1 (>>=)
// --------------------------------------------------------
// Definition of expression
// --------------------------------------------------------

type expr {
  Int( i : int )
  Div( e1 : expr, e2: expr )
}

fun show( e : expr ) : total string {
  match(e) {
    Int(i) -> core/show(i)
    Div(e1,e2) -> "(" + e1.show + "/" + e2.show + ")"
  }
}

val expr1 = Int(1)
val expr2 = Div(Div(Int(10),Int(2)),Int(3))
val exprX = Div(Div(Int(10),Int(2)),Int(0))

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


// --------------------------------------------------------
// Monadic
// --------------------------------------------------------


alias stexn<a> = int -> either<string,(a,int)>

fun run(s,m) {
  m(s)
}

fun show( r : either<string,(int,int)> ) : total string {
  match(r) {
    Right((x,s)) -> x.core/show + " (state=" + s.core/show + ")"
    Left(str)    -> "error: " + str
  }
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

fun tick() {
  return fun(s) { Right(((),s+1))}
}

fun failure(err) {
  return fun(s) { Left(err) }
}

fun eval( e : expr ) : stexn<int> {
  match(e) {
    Int(i)   -> unit(i)
    Div(x,y) ->
      tick() >>= (fun(_){
        eval(x) >>= (fun(i) {
          eval(y) >>= (fun(j) {
            (if (j==0) then failure("division by zero") else unit(())) >>= (fun(_){
               unit( i / j )
      })})})}) 
  }
}

fun main() {
  run(0,eval(expr2)).show.println
}