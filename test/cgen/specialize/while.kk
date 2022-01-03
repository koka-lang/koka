fun countdown(n_ : int) : _ ()
  var n := n_
  while { n > 0 }
    n.println
    n := n - 1


fun main() {
  countdown(10)
}