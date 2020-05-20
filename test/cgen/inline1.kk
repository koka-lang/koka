public module inline1
// recurse using an accumulator using constant heap space
private fun xmap-acc(g : (int,a,list<a>) -> e b , n : int, acc : list<b>, ys : list<a>) : e list<b> {
  match(ys) {
    Cons(x,xx) -> xmap-acc(g,n+1,Cons(g(n,x,xx),acc),xx)
    Nil -> reverse(acc)
  }
}

// Apply a function `f`  to each element of the input list in sequence where takes
// both the index of the current element, the element itself, and the tail list as arguments.
fun xxmap-indexed-peek(xs : list<a>, f : (idx : int, value : a, rest : list<a> ) -> e b) : e list<b>
{
  return xmap-acc(f,0,Nil,xs)
}

// Apply a function `f`  to each element of the input list in sequence where takes
// both the index of the current element, the element itself, and the tail list as arguments.
fun xmap-indexed-peek(xs : list<a>, f : (idx : int, value : a, rest : list<a> ) -> e b) : e list<b>
{
  // recurse for the first `maxListStack` elements over the stack (to avoid extra heap allocation)
  fun map-iter(g : (int,a,list<a>) -> e b, n : int, ys : list<a>) : e list<b> {
    if (n > 100) {
      xmap-acc(g,n,Nil,ys)
    }
    else {
      match(ys) {
        Cons(x,xx) -> Cons(g(n,x,xx),map-iter(g,n+1,xx))
        Nil        -> Nil
      }
    }
  }

  return map-iter(f,0,xs)
}
