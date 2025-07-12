
fun test-landin()
  val r = ref(fn() ())
  fun f()
    (!r)()
  r := f
  f()

fun test1( init , ?hdiv )
  val r = ref(init)
  fn(x) { !r; x }

fun test2()
  val r = ref(1)
  !r

// fun test3()
//  test1()

fun test-pure1()
  val r = ref(1)
  !r

fun test-pure2(x)
  val r = ref(x)
  !r

