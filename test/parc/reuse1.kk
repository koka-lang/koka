// Unzip a list of pairs into two lists
fun unzipx( xs : list<(a,b)> ) : (list<a>,list<b>)
{
  fun iter( ys, acc1, acc2 )
  {
    match(ys) {
      Cons((x,y),xx) -> iter(xx,Cons(x,acc1),Cons(y,acc2))
      Nil            -> (reverse(acc1),reverse(acc2))
    }
  }

  iter(xs,[],[])
}
