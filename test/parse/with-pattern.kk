// A `with` statement may bind a pattern instead of a plain name, so a chain of
// fallible steps reads as a sequence rather than as nested matches. The pattern
// is matched in the continuation lambda that `with` builds.
//
// No `.kk.out` is committed: record it with `--mode=new`.
fun and-then( r : either<string,a>, next : (a) -> either<string,b> ) : either<string,b>
  match r
    Left(x)  -> Left(x)
    Right(v) -> next(v)

fun pair() : either<string,int>
  with (a, b) <- Right((1,2)).and-then
  Right(a + b)

fun triple() : either<string,int>
  with (a, b, c) <- Right((1,2,3)).and-then
  Right(a + b + c)

// nested patterns bind too
fun nested() : either<string,int>
  with (a, (b, c)) <- Right((1,(2,3))).and-then
  Right(a + b + c)

// and the short-circuiting path still skips the continuation
fun short() : either<string,int>
  with (a, b) <- Left("no").and-then
  Right(a + b)

fun main()
  println(pair().show)
  println(triple().show)
  println(nested().show)
  println(short().show)
