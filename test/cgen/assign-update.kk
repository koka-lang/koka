// `x := e` gives the same result whether it is checked as an update (`e` total)
// or as a plain assignment (`e` reads another local, or may throw)
fun counter( n : int ) : div int
  var i := 0
  var s := 0
  while { i < n }
    s := s + i
    i := i + 1
  s

// `?xs` is resolved inside the update, where `xs` is the current value
fun add-len( y : int, ?xs : list<int> ) : list<int>
  Cons(y + xs.length, xs)

fun main()
  var xs := [1, 2, 3]
  xs := xs.map(fn(x) x * 2)
  println(xs)
  var ys := [4]
  xs := xs ++ ys
  println(xs)
  xs := xs.map(fn(x) if x > 10 then throw("too big") else x)
  println(xs)
  xs := xs ++ xs.map(fn(y) y + 1)
  println(xs)
  xs := (match xs { Cons(_, rest) -> rest ++ rest; Nil -> [] })
  println(xs)
  xs := xs.map(fn(x) { val y = x * 10; y })
  println(xs)
  xs := xs.take(2) ++ (fn() { fun incr(l : list<int>) : list<int> { match l { Cons(z, zs) -> Cons(z + 1, incr(zs)); Nil -> Nil } }; incr([1, 2]) })()
  println(xs)
  xs := xs ++ (fn() { fun seven() [7]; seven() })()
  println(xs)
  xs := add-len(xs.length)
  println(xs)
  println(counter(10))
