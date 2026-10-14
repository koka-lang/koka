// A divergent recursive definition whose annotation quantifies in another order
// than its generalized type: `forall<a,f>` against `forall<f,a>`, of different
// kinds. The recursive call takes the type arguments of the generalized type.
fun walk( x : f<a>, n : int ) : div int
  if n == 0 then 0 else walk(x, n)

fun main()
  println(walk(Just(1), 0))
