// Update function
fun update(xs: list<int>): console list<int>
  println(if xs.unsafe-is-unique then "unique a" else "not unique a")
  xs.map fn(x)
    x + 1
  
// This is unsafe, since the behavior that depends on this check needs to be observationally equivalent
inline fip extern unsafe-is-unique( ^v : list<a> ) : bool
  c inline "kk_datatype_is_unique(#1, kk_context())"

effect choice
  ctl choice() : int

// https://github.com/koka-lang/koka/issues/544#issuecomment-2155184608 Anton's example
fun antons-example()
  // Nondeterministic choice, should copy vectors
  var a := vector-init-total(3, fn(i) 0)
  a[1] := 1
  val xs =
    with handler
      return(x) [x]
      ctl choice()
        resume(0) ++ resume(1)
    a.map(fn(x) abs(x - choice()))
  xs.map(fn(v) v.map(show).join(",")).println
  ()
  
fun main()
  println("Non-fip copy of vector and items")
  val v = [[1], [2], [3], [4]].vector
  val xs = v
  // Copies the vector and the items are no longer unique
  v.map(update).map(fn(x) x.show).join("").println
  xs.map(fn(x) x.show).join("").println
  
  // Items are unique, since the vector is unique
  println("Fip updates to items in vector")
  val v1 = [[1], [2], [3], [4]].vector
  v1.map(update).map(fn(x) x.show).join("").println

  println("Fip updates to vector, but only one items")
  val v2 = [[1], [2], [3], [4]].vector
  val v3 = v2.set(0, [-10])
  val v4 = v3.update(1, fn(l) l.update())
  v4.map(fn(x) x.show).join(",").println

  println("Non-fip copies of vector in multiple resumptions")
  antons-example()

  // Fip update to item in local vector variable
  println("Fip update to item in local vector variable")
  var a1 := vector-init-total(3, fn(i) [i])
  a1[1] := [5]
  update(std/core/types/@byref(a1), 2, update)
  a1.map(fn(x) x.show).join("").println

  println("Non-fip update to item in local vector variable")
  var a2 := vector-init-total(3, fn(i) [i])
  val a3 = a2
  update(std/core/types/@byref(a2), 2, update)
  a2.map(fn(x) x.show).join("").println
  a3.map(fn(x) x.show).join("").println


// TODO: Needs to be fixed!!!
extend type exception-info
  ExnL(l: list<int>)

fun update-throw(x: list<int>)
  println(if x.unsafe-is-unique then "unique a" else "not unique a")
  throw-exn(Exception("err", ExnL(x)))

fun update-throw2(x: list<int>)
  println(if x.unsafe-is-unique then "unique a" else "not unique a")
  throw(x.show)

// The nice thing about multiple references is that it will always be copied - so there is no issue
// The problem with exceptions, is the reference count is too low
// 
fun test-error1()
  println("Non-fip update to item in local vector variable")
  var a2 := vector-init-total(3, fn(i) [i])
  val a3 = a2
  try {
    update(std/core/types/@byref(a2), 2, update-throw)
    a2.map(fn(x) x.show).join("").println
  } fn(err) {
      match err.info 
        ExnL(l) -> println(if l.unsafe-is-unique then "unique a" else "not unique a")
      a2.foreach(fn(x) x.show.trace)
  }

fun test-error2()
  println("Non-fip update to item in local vector variable")
  val a4 = vector-init-total(3, fn(i) [i])
  var a5 := a4
  try {
    update(std/core/types/@byref(a5), 2, update-throw)
    a5.map(fn(x) x.show).join("").println
  } fn(err) {
      match err.info
        ExnL(l) -> println(if l.unsafe-is-unique then "unique a" else "not unique a")
      a5.foreach(fn(x) x.show.trace)
  }

fun test-error3()
  println("Non-fip update to item in local vector variable")
  val a6 = vector-init-total(3, fn(i) [i])
  var a7 := a6
  try {
    update(std/core/types/@byref(a7), 2, update-throw)
    a7.map(fn(x) x.show).join("").println
  } fn(err) {
      match err.info
        ExnL(l) -> println(if l.unsafe-is-unique then "unique a" else "not unique a")
      a7.foreach(fn(x) x.show.trace)
  }

// This works for some reason?
fun test-error4()
  println("Non-fip update to item in local vector variable")
  val a6 = vector-init-total(3, fn(i) [i])
  var a7 := a6
  update(std/core/types/@byref(a7), 2, update-throw)
