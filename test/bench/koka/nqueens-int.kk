module nqueens-int

alias solution = list<int>
alias solutions = list<list<int>>

fun safe( queen : int, diag : int, ^xs : solution ) : bool
  match xs
    Cons(q,qs) -> (queen != q && queen != q + diag && queen != q - diag && safe(queen,diag + 1,qs))
    _          -> True

fun append-safe( queen : int, xs : solution, xss : solutions ) : div solutions
  if queen <= 0 then xss
  elif safe(queen,1,xs) then append-safe( queen - 1, xs, Cons(Cons(queen,xs),xss) )
  else append-safe( queen - 1, xs, xss )

fun extend(queen : int, acc : solutions, xss : solutions ) : div solutions
  match xss
    Cons(xs,rest) -> extend(queen, append-safe(queen,xs,acc), rest)
    Nil           -> acc

fun find-solutions( n : int, queen : int ) : div solutions
  if queen == 0
   then [[]]
   else extend(n,[], find-solutions(n,queen - 1))

pub fun queens( n : int ) : div int
  find-solutions(n,n).length

pub fun main()
  queens(13).println

