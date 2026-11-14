// `x := e` with `x` in a total `e` moves the value into `e`, so it is unique
// there; otherwise (here: `e` reads another local) the variable keeps a reference.
extern is-unique( ^xs : list<int> ) : bool
  c inline "(kk_datatype_is_ptr(#1) && kk_datatype_ptr_is_unique(#1,kk_context()))"

fun mark( xs : list<int> ) : list<int>
  if is-unique(xs) then Cons(1, xs) else Cons(0, xs)

fun moved() : list<int>
  var xs := [7]
  xs := mark(xs)
  xs

fun kept() : list<int>
  var xs := [7]
  var n := 0
  xs := mark(xs) ++ (if n > 0 then [n] else [])
  xs

fun main()
  println(moved())
  println(kept())
