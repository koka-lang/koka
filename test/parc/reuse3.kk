// Unzip a list of pairs into two lists
fun test( xs : list<(int32,b)> ) : exn int32
{
  match(xs) {
    Cons(_,Cons((x,_))) -> x
  }
}
