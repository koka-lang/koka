// Test creation of direct- and cps- converted functions.

// Apply a fun `f`  to each element of the input list in sequence where takes
// both the index of the current element, the element itself, and the tail list as arguments.
fun mapIndexedPeekX(xs : list<a>, f : (idx : int, value : a, rest : list<a> ) -> e b) : e list<b>
{
  // recurse using an accumulator using constant heap space
  fun mapAcc(g : (int,a,list<a>) -> e b , n : int, acc : list<b>, ys : list<a>) : e list<b> {

    match(ys) {
      Cons(x,xx) -> mapAcc(g,n+1,Cons(g(n,x,xx),acc),xx)
      Nil -> reverse(acc)
    }
  }

  // recurse for the first 100 elements over the stack (to avoid extra heap allocation)
  fun mapIter(g : (int,a,list<a>) -> e b, n : int, ys : list<a>) : e list<b> {
    if (n > 100) {
      mapAcc(g,n,Nil,ys)
    }
    else {
      match(ys) {
        Cons(x,xx) -> Cons(g(n,x,xx),mapIter(g,n+1,xx))
        Nil        -> Nil
      }
    }
  }

  return mapIter(f,0,xs)
}

fun main() {
  [1,2].mapIndexedPeekX( fun(idx,x:int,xs) { x+idx }).show.println
}