// A generator effect with one operation
effect yield<a> {
  fun yield( x : a ) : ()
}

// Traverse a list and yield the elements
fun traverse( xs : list<int> ) : yield<int> () {
  match(xs) {
    Cons(x,xx) -> { yield(x); traverse(xx) }
    Nil        -> ()
  }
}

fun main() : console () {
  with fun yield(i : int) {
    println("yielded " + i.show)    
  }
  [1,2,3].traverse
}

