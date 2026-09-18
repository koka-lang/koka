// https://github.com/koka-lang/koka/issues/925
//
// A tail call assigns the loop parameters in order, so an argument that still
// reads an earlier parameter needs a temporary: `k1` below is emitted as
// `z.key` and `z` is assigned first, and `swap` passes a parameter directly.

type chain
  Link( key : int, next : chain )
  End

fun walk( z : chain, k : int, acc : int ) : div int
  match z
    Link(k1, Link(_, rest)) -> walk(rest, k1, acc + k)
    Link(k1, End)           -> acc + k + k1
    End                     -> acc + k

fun swap( a : int, b : int, n : int ) : div int
  if n <= 0 then a - b else swap(b, a, n - 1)

pub fun main()
  val c = Link(1, Link(2, Link(3, Link(4, End))))
  println( walk(c, 0, 0) )
  println( swap(10, 3, 1) )
  println( swap(10, 3, 2) )
