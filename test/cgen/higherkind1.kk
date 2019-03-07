fun bar( f : forall<c,a> ( x:c<a>,y:a) -> list<a> ) {
  f([2],1)
}

fun foo( x:a,y:b ) : list<b> {
  [y]
}

fun main() { bar(foo) }
