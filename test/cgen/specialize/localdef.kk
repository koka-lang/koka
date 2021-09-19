// Applies a function `f` to list of increasing elements from `lo`  to `hi`
// (including both `lo`  and `hi` ).
// If `lo > hi`  the function returns the empty list.
fun li( lo : int, hi: int, f : int -> e a) : e list<a>
  fun enumerate( low:int, high:int, acc )
    if (low > high)
     then acc
     else enumerate(low, unsafe-decreasing(high - 1), Cons(f(high),acc))
  enumerate(lo,hi,[])

fun main()
  li(0, 10) fn(i)
    i.show
  .show.println