infixl 1 (>>=)
// --------------------------------------------------------
// Definition of expression
// --------------------------------------------------------

type expr {
  Int( i : int )
  Div( e1 : expr, e2: expr )
}

fun expr/show( e : expr ) : total string {
  match(e) {
    Int(i) -> show(i)
    Div(e1,e2) -> "(" ++ e1.show ++ "/" ++ e2.show ++ ")"
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

fun run(s : int, m : stexn<a> ) : total either<string,(a,int)> {
  m(s)
}

fun either/show( r : either<string,(int,int)> ) : string {
  match(r) {
    Right((x,s)) -> x.show ++ " (state=" ++ s.show ++ ")"
    Left(str)    -> "error: " ++ str
  }
}

fun unit(x : a) : stexn<a> {
  return fn(s) { Right((x,s))}
}

fun (>>=)( m : stexn<a>, f : a -> stexn<b> ) : stexn<b> {
  return fn(s0) {
    match(m(s0)) {
      Right((x,s1)) -> f(x)(s1)
      Left(str)     -> Left(str)
    }
  }
}

fun tick() {
  return fn(s) { Right(((),s+1))}
}

fun failure(err) {
  return fn(s) { Left(err) }
}

fun eval( e : expr ) : stexn<int> {
  match(e) {
    Int(i)   -> unit(i)
    Div(x,y) ->
      tick() >>= (fn(_){
        eval(x) >>= (fn(i) {
          eval(y) >>= (fn(j) {
            (if (j==0) then failure("division by zero") else unit(())) >>= (fn(_){
               unit( i / j )
      })})})})
  }
}

pub fun main() {
  run(0,eval(expr2)).show.println
}