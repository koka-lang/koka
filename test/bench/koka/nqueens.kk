module nqueens

alias solution = list<int>
alias solutions = list<list<int>>

fun safe( queen : int, diag : int, xs : solution ) : bool {
  match(xs) {
    Cons(q,qs) -> (queen != q && queen != (q+diag) && queen != (q - diag) && safe(queen,diag+1,qs))
    _          -> True
  }
}

fun appendSafe( queen : int, xs : solution, xss : solutions ) : div solutions {
  if (queen <= 0) then xss
  elif (safe(queen,1,xs)) then appendSafe( queen - 1, xs, Cons(Cons(queen,xs),xss) )
  else appendSafe( queen - 1, xs, xss )
}

fun extend(queen : int, acc : solutions, xss : solutions ) : div solutions {
  match(xss) {
    Cons(xs,rest) -> extend(queen, appendSafe(queen,xs,acc), rest)
    Nil           -> acc
  }
}

fun findSolutions( n : int, queen : int ) : div solutions {
  if (queen == 0) 
   then [[]]
   else extend(n,[], findSolutions(n,queen - 1))  
}

public fun queens( n : int ) : div int {
  length(findSolutions(n,n))
}

public fun main() {
  queens(13).println
}

