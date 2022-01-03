// a generator effect with one operation
effect yield<a> 
  fun yield( x : a ) : ()

// traverse a list and yield the elements
fun traverse( xs : list<a> ) : yield<a> () 
  match xs
    Cons(x,xx) -> { yield(x); traverse(xx) }
    Nil        -> ()

// bind the yield operation dynamically
fun main() : console () 
  with fun yield(i:int) 
    println("yielded " ++ i.show) 
  [1,2,3].traverse

