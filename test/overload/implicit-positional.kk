// Merge sort can be done on any pre-order
pub fun pre-order/sort(l : list<a>, ?(<=) : (a, a) -> e bool) : e list<a>
  []

// But a total order also works of course
pub fun order/sort(l : list<a>, ?cmp : (a, a) -> e order) : e list<a>
  []

// We can define a left-to-right total order on vectors, if the elements are totally ordered
pub fun vector/cmp-lr(xs : vector<char>, ys : vector<char>, ?cmp : (char,char) -> e order): e order
  Gt

// ----- MAIN ------
pub fun main()
  val labels = ["abc".vector, "abd".vector]
  labels.sort(cmp-lr)
  ()