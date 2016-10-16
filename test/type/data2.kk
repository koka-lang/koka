struct foo( i : int, d : double = 0.0 )

// Dependency issue: if called 'foo' it is checked *before* .create-foo is defined
fun foo( i : int ) : foo { 
  Foo(i) 
}

fun test(x) {
  val f = Foo(x)
  ()
}