// Fold a list from the left, i.e. `foldl([1,2],0,(+)) == (0+1)+2`
// Since `foldl` is tail recursive, it is preferred over `foldr` when using an associative function `f`
fun xfoldl(xs,z,f) {
  match(xs) {
    Cons(x,xx) -> xfoldl(xx,f(z,x),f)
    Nil        -> z
  }
}

fun xfoldl1(xs : list<a>, f : (a,a) -> <exn|e> a) : <exn|e> a {
  match(xs) {
    Cons(x,xx) -> xx.xfoldl(x,f)
    Nil        -> throw("unexpected Nil in std/core/xfoldl1")
  }
}

fun foo() {
  list(1,10).xfoldl( 0, (+) ).show.println
}

fun main() {
  list(1,10).xfoldl1( (+) ).show.println
}