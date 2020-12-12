module nqueens
import std/num/int32

alias solution = list<int32>
alias solutions = list<list<int32>>

fun safe( queen : int32, diag : int32, xs : solution ) : bool {
  match(xs) {
    Cons(q,qs) -> (queen != q && queen != (q+diag) && queen != (q - diag) && safe(queen,diag.inc,qs))
    _          -> True
  }
}

fun appendSafe( queen : int32, xs : solution, xss : solutions ) : div solutions {
  if (queen <= 0.int32) then xss
  elif (safe(queen,1.int32,xs)) then appendSafe( queen.dec, xs, Cons(Cons(queen,xs),xss) )
  else appendSafe( queen.dec, xs, xss )
}

fun extend(queen : int32, acc : solutions, xss : solutions ) : div solutions {
  match(xss) {
    Cons(xs,rest) -> extend(queen, appendSafe(queen,xs,acc), rest)
    Nil           -> acc
  }
}

fun findSolutions( n : int32, queen : int32 ) : div solutions {
  if (queen.is-zero)
   then [[]]
   else extend(n,[], findSolutions(n,queen.dec))
}

public fun queens( n : int32 ) : div int {
  length(findSolutions(n,n))
}

public fun main() {
  queens(13.int32).println
}
