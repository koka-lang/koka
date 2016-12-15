struct foo( i : int, d : double )

fun foo( i : int, d : double = 0.0 ) : foo {
  Foo(i,d)
}

fun foo( d : double ) : foo {
  Foo(0,d)
}

fun main() {
  val f = foo(1)
  ()
}