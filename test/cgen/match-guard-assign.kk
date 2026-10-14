// A match with one branch but several guards, whose result is assigned to a variable:
// a guard that holds must not fall through to the next guard.
type pair
  Pair(a : int, b : int)

noinline fun smaller(p : pair, d : int) : int
  val x = match p
            Pair(a, _) | a <= d -> a
                       | True   -> d
  x + 0

fun main()
  println(smaller(Pair(1, 2), 3))
  println(smaller(Pair(4, 2), 3))
