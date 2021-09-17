// Fold a list from the right, i.e. `foldr([1,2],0,(+)) == 1+(2+0)`
// Note, `foldr` is less efficient than `foldl` as it reverses the list first.
fun xfoldr(xs,z,f) {
  xs.reverse.xfoldl(z,fn(x,y){f(y,x)})
}

// Fold a list from the left, i.e. `foldl([1,2],0,(+)) == (0+1)+2`
// Since `foldl` is tail recursive, it is preferred over `foldr` when using an associative function `f`
fun xfoldl(xs,z,f) {
  match(xs) {
    Cons(x,xx) -> xfoldl(xx,f(z,x),f)
    Nil        -> z
  }
}

fun main() {
  list(1,10).foldr( 0, (+) ).println
}