// tests codegeneration for existentials
type expr<a> {
  Lit( x : a )
  If( pred : expr<bool>, t : expr<a>, e : expr<a> )
  App<b>( f : expr<b -> a>, arg : expr<b> )
}

fun is-positive( i : int ) : bool {
  i.is-pos
}

val bexpr : expr<bool> =  App(Lit(is-positive),Lit(42))

fun eval(expr : expr<a>) : a {
  match(expr) {
    Lit(x)    -> x
    If(p,t,e) -> if (p.eval) then t.eval else e.eval
    App(f,x)  -> (f.eval)(True)
  }
}

fun main() {
  println(eval(bexpr))
}
