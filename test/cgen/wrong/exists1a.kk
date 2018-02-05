// tests codegeneration for existentials
type expr<a> {
  Lit( x : a )
  If( pred : expr<bool>, t : expr<a>, e : expr<a> )
  App<b>( f : expr<b -> a>, arg : expr<b> )
}

val bexpr : expr<bool> =  App(Lit((pos? : int -> bool)),Lit(42))

fun eval(expr : expr<a>) : a {
  match(expr) {
    Lit(x)    -> x
    If(p,t,e) -> if (p.eval) then t.eval else e.eval
    App(f,x)  -> (x.eval)
  }
}

fun main() {
  println(eval(bexpr))
}
