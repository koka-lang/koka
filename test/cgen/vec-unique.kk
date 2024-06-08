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
  val v = [[1], [2], [3], [4]].vector
  val xs = v
  v.map(update).map(fn(x) x.show).join("").println
  xs.map(fn(x) x.show).join("").println
  
  val v1 = [[1], [2], [3], [4]].vector
  v.map(update).map(fn(x) x.show).join("").println

  antons-example()

  // TODO:
  // var a := vector-init-total(3, fn(i) [i])
  // a[1] := [5]
  // update(std/core/types/@byref(a), 2, update)
  // a.map(fn(x) x.show).join("").println

