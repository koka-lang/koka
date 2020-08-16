
rectype foo {
  Foo( i : int )  
}

fun testx(foo:foo)  {
  match(foo) {
    Foo(ii) -> ii
  }
}
