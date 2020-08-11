// Unzip a list of pairs into two lists
fun test( xs : list<int32> ) : exn int32
{
  match(xs) {
    Cons(_,Cons(x)) -> x
  }
}
