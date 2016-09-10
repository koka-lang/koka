import std/time

fun timer(f) {
  print-elapsed(f)
}

alias solution = list<int>
alias solutions = list<list<int>>

fun safe( queen : int, diag : int, xs : solution ) : bool
{
  match(xs) {
    Cons(q,qs) -> (queen != q && queen != (q+diag) && queen != (q - diag) && safe(queen,diag+1,qs))
    _          -> True
  }
}

fun appendSafe( queen : int, xs : solution, xss : solutions ) : div solutions
{
  if (queen <= 0) xss
  else {
   if (safe(queen,1,xs))
    then appendSafe( queen - 1, xs, Cons(Cons(queen,xs),xss) )
    else appendSafe( queen - 1, xs, xss )
  }
}

fun extend(queen : int, acc : solutions, xss : solutions ) : div solutions
{
  match(xss) {
    Cons(xs,rest) -> extend(queen, appendSafe(queen,xs,acc), rest)
    Nil           -> acc
  }
}

fun findSolutions( n : int, queen : int ) : div solutions
{
  if (queen == 0) 
   then [[]]
   else extend(n,[], findSolutions(n,queen - 1))  
}

public fun queens( n : int ) : div solutions
{
  findSolutions(n,n)
}


effect choose {
  pick(n : int) : int;
  fail<b>()     : b
}



fun chooseAll( action ) {
  handle(action){
    return x  -> [x]
    pick(n)   -> list(1,n,resume).concat
    fail      -> []
  }
}

fun findSolution( n : int, queen : int ) : <choose,div> solution {
  /*
  if (queen==0) then [] else {
    val xs = findSolution(n,queen-1)
    val i  = pick(n)
    if (safe(i,1,xs)) then Cons(i,xs) else fail() 
  }
  */
  // this goes wrong due to the return being mistranslated
  if (queen==0) then return []
  
  val xs = findSolution(n,queen - 1)
  val i  = pick(n)
  if (safe(i,1,xs)) then Cons(i,xs) else fail() // extend1(n, findSolution(n, queen-1))
  
}

fun showSolutions( xss : solutions ) : io () {
  println(xss.length.show)
  xss.foreach fun(xs) {
    println(" " + xs.show)
  }  
}

fun queensChoose(n : int ) : div solutions {
  chooseAll({findSolution(n,n)} )
}

fun test(n) {
  timer({ queens(n) }).length.show.println
  timer({ queensChoose(n) }).length.show.println
}

fun main() {
  test(11)
}

/*
safeAddition :: [Int] -> Int -> Int -> Bool
safeAddition [] _ _ = True
safeAddition (r:rows) row i =
   row /= r &&
   abs (row - r) /= i &&
   safeAddition rows row (i + 1)

-- hand-coded solution to the n-queens problem
queensPure :: Int -> [[Int]]
queensPure n = foldM f [] [1..n] where
    f rows _ = [row : rows |
                row <- [1..n],
                safeAddition rows row 1]

-- n queens using an abstract Choose operator
queensComp :: [handles|h {Choose}|] => Int -> Comp h [Int]
queensComp n = foldM f [] [1..n] where
    f rows _ = do row <- choose [1..n]
                  if (safeAddition rows row 1)
                    then return (row : rows)
                    else failed
*/