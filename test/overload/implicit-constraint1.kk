
pub fun list/sum( xs : list<int> ) : int
  var total : int := 0
  var comp  : int := 0
  xs.foreach fn(x)
    val t = total + x
    val c = if total.abs >= x.abs then (total - t) + x else (x - t) + total
    comp  := comp + c
    total := t
  total + comp;

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

fun main()
  test1(0)(0).println
  test2().println
  test-pure1().println
  test-pure2(1).println