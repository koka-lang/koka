// https://github.com/koka-lang/koka/issues/879

effect fun foo(x : string) : ()

fun int/impl() : foo int
  foo("Int")
  -1

fun tuple2/impl(?a/impl : () -> foo a, ?b/impl : () -> foo b) : foo (a, b)
  foo("Tuple2:")
  (a/impl(), b/impl())

fun print-foo(?impl : () -> foo a) : console a
  with fun foo(x)
    println(x)
  impl()

fun main()
  val tup : (int, (int, int)) = print-foo()
  tup