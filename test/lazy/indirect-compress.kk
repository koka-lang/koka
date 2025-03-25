
// example that takes n^2 time if there is no indirection compression
type chain
  Done( value : int )
  lazy Next( c : chain ) -> c


fun eval( c : chain ) : div int
  match c
    Done(x) -> x

fun build( count : int ) : pure list<chain>
  if count<=0 then [Done(1)] else
    val xs = build(count - 1)
    Cons(Next(head(xs).unjust), xs)

fun traverse( n : int ) : pure int
  build(n).map(eval).sum

fun main()
  traverse(10000).print