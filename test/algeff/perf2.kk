import std/time

// -----------------------------------------------------------------
// Shared
// -----------------------------------------------------------------

alias solution = list<int>

fun safe( queen : int, diag : int, xs : solution ) : bool {
  match(xs) {
    Cons(q,qs) -> (queen != q && queen != (q+diag) && queen != (q - diag) && safe(queen,diag+1,qs))
    _ -> True
  }
}

// -----------------------------------------------------------------
// Hand-tuned, tail-recursive solution
// -----------------------------------------------------------------

alias solutions = list<list<int>>

fun append-safe( queen : int, xs : solution, xss : solutions ) : div solutions {
  if (queen <= 0) xss
  else {
   if (safe(queen,1,xs))
    then append-safe( queen - 1, xs, Cons(Cons(queen,xs),xss) )
    else append-safe( queen - 1, xs, xss )
  }
}

fun extend(queen : int, acc : solutions, xss : solutions ) : div solutions {
  match(xss) {
    Cons(xs,rest) -> extend(queen, append-safe(queen,xs,acc), rest)
    Nil           -> acc
  }
}

fun find-solutions( n : int, queen : int ) : div solutions {
  if (queen == 0) 
   then [[]]
   else extend(n,[], find-solutions(n,queen - 1))  
}

public fun queens( n : int ) : div solutions {
  find-solutions(n,n)
}

// -----------------------------------------------------------------
// Elegant solution with user-defined effects
// -----------------------------------------------------------------

effect choose {
  pick(n : int) : int;
  fail<b>()     : b
}

fun choose-all( action ) {
  handle(action){
    return x  -> [x]
    pick(n)   -> list(1,n,resume).concat
    fail      -> []
  }
}

fun find-solution( n : int, queen : int ) : <choose,div> solution {
  if (queen==0) then return []
  val xs = find-solution(n,queen - 1)
  val i  = pick(n)
  if (safe(i,1,xs)) then Cons(i,xs) else fail()
}

fun queens-choose(n : int ) : div solutions {
  choose-all{ find-solution(n,n) }
}


// -----------------------------------------------------------------
// Testing
// -----------------------------------------------------------------

fun test(n) {
  print-elapsed({ queens(n) }).length.println
  print-elapsed({ queens-choose(n) }).length.println
}

fun main() {
  test(12)
}


fun show-solutions( xss : solutions ) : io () {
  println(xss.length.show)
  xss.foreach fun(xs) {
    println(" " + xs.show)
  }  
}

