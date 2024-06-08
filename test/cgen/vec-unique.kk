// Update function
fun update(xs: list<int>): console list<int>
  println(if xs.unsafe-is-unique then "unique a" else "not unique a")
  xs.map(_ + 1)

fun update-trace(xs: list<int>): list<int>
  trace(if xs.unsafe-is-unique then "unique a" else "not unique a")
  xs.map(_ + 1)
  
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
  xs.map(_.map(show).join(",")).println
  ()
  
fun main()
  println("Non-fip copy of vector and items")
  val v = [[1], [2], [3], [4]].vector
  val xs = v
  // Copies the vector and the items are no longer unique
  v.map(update).map(_.show).join("").println
  xs.map(_.show).join("").println
  
  // Items are unique, since the vector is unique
  println("Fip updates to items in vector")
  val v1 = [[1], [2], [3], [4]].vector
  v1.map(update).map(_.show).join("").println

  println("Fip updates to vector, but only one items")
  val v2 = [[1], [2], [3], [4]].vector
  val v3 = v2.set(0, [-10])
  val v4 = v3.update(1, fn(l) l.update())
  v4.map(_.show).join(",").println

  println("Non-fip copies of vector in multiple resumptions")
  antons-example()

  // Fip update to item in local vector variable
  println("Fip update to item in local vector variable")
  var a1 := vector-init-total(3, fn(i) [i])
  a1[1] := [5]
  std/core/types/@byref(a1).update(2, update-trace)
  a1.map(_.show).join("").println

  println("Non-fip update to item in local vector variable")
  var a2 := vector-init-total(3, fn(i) [i])
  val a3 = a2
  std/core/types/@byref(a2).update(2, update-trace)
  a2.map(_.show).join("").println
  a3.map(_.show).join("").println