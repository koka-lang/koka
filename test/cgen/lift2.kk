val maxListStack = 100

// Reverse a list.
fun xreverse(xs : list<a>) : list<a>
{
  xreverse-append( xs, Nil )
}

// Efficiently reverse a list `xs` and append it to `tl`:\
// `reverse-append(xs,tl) == reserve(xs) + tl
fun xreverse-append( xs : list<a>, tl : list<a> ) : list<a>
{
  fun reverse-acc(acc : list<a>, ys : list<a> ) : list<a> {
    match(ys) {
      Cons(x,xx) -> reverse-acc(Cons(x,acc),xx)
      _          -> acc
    }
  }
  reverse-acc(tl,xs)
}

// Append two lists.
fun xappend(xs : list<a>, ys : list<a> ) : list<a>
{
  // append using _constant_ stack space (by reversing the argument list)
  fun rev-append(xx,yy) {
    match(xx) {
      Cons(z,zz) -> rev-append(zz,Cons(z,yy))
      _          -> yy
    }
  }
  // append for the first `maxListStack` elements over the stack
  fun append(n,xx,yy) {
    if (n > maxListStack) {
      rev-append(xreverse(xx),yy)
    }
    else {
      match(xx) {
        Cons(z,zz) -> Cons(z,append(n+1,zz,yy))
        Nil -> yy
      }
    }
  }

  match(ys) {
    Nil -> xs
    _   -> match(xs) {
             Nil -> ys
             _   -> append(0,xs,ys)
           }
  }
}

fun test() {
  xappend(list(1,100),list(1,100))
}
