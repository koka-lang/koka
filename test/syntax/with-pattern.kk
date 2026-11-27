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
