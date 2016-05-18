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

function main() {
  run(0,eval(expr2)).show.println
}