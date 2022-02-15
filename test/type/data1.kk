struct foo( i : int, d : float64 )

fun foo( i : int, d : float64 = 0.0 ) : foo {
  Foo(i,d)
}

fun foo( d : float64 ) : foo {
  Foo(0,d)
}

fun main() {
  val f = foo(1)
  ()
}