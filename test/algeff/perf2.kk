import std/date

function timer(f) {
  val t0 = now().time
  val adjust = now().time - t0
  println("start")
  val start = now().time
  val x = f()
  val end = now().time
  println("ended in " + (end - start).show + "ms")
  x
}

alias solution = list<int>
alias solutions = list<list<int>>

function safe( queen : int, diag : int, xs : solution ) : bool
{
  match(xs) {
    Cons(q,qs) -> (queen != q && queen != (q+diag) && queen != (q - diag) && safe(queen,diag+1,qs))
    _          -> True
  }
}

function appendSafe( queen : int, xs : solution, xss : solutions ) : div solutions
{
  if (queen <= 0) xss
  else {
   if (safe(queen,1,xs))
    then appendSafe( queen-1, xs, Cons(Cons(queen,xs),xss) )
    else appendSafe( queen-1, xs, xss )
  }
}

function extend(queen : int, acc : solutions, xss : solutions ) : div solutions
{
  match(xss) {
    Cons(xs,rest) -> extend(queen, appendSafe(queen,xs,acc), rest)
    Nil           -> acc
  }
}

function findSolutions( n : int, queen : int ) : div solutions
{
  if (queen == 0) 
   then [[]]
   else extend(n,[], findSolutions(n,queen-1))  
}

public function queens( n : int ) : div int
{
  length(findSolutions(n,n))
}


effect choose {
  pick(n : int) : int;
  fail<b>()     : b
}

function listmap(n : int, f : int -> e a) : e list<a> {
  if (n < 1) then [] else Cons(f(n),listmap(unsafeDecreasing(n-1),f))
}

function chooseAll( action ) {
  val h = handler{
    return x  -> [x]
    pick(n)   -> {
      list(1,n).map(resume).concat
    }
    fail      -> []
  }
  h(action)
}

function extend1( n : int, xs : solution ) : choose solution {
  val i = pick(n)
  if (safe(i,1,xs)) then Cons(i,xs) else fail()
}

function findSolution( n : int, queen : int ) : choose solution {
  if (queen==0) then []
   else extend1(n, findSolution(n, unsafeDecreasing(queen-1)))
}

function showSolutions( xss : solutions ) : io () {
  xss.foreach fun(xs) {
    println(xs.show)
  }
}

function queensChoose(n : int ) : io () {
  val xss = chooseAll({findSolution(n,n)} )
  xss.showSolutions
}

function test(n) {
  println( queens(n).show )
  queensChoose(n) 
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