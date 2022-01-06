// Apply a function `f`  to each element of the input list in sequence where takes
// both the index of the current element and the element itself as arguments.
fun xmap-indexed(xs : list<a>, f : (idx : int, value : a) -> e b) : e list<b> 
  fun map-idx(ys,i)
    match ys
      Cons(y,yy) -> val j = i+1 in Cons(f(i,y), map-idx(yy,i+1)) 
      Nil        -> Nil
  map-idx(xs,0)

fun main() 
  list(1,10).xmap-indexed(fn(i,x) i+x).show.println