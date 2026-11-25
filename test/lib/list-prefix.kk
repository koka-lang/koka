// `list/is-prefix-of`: the empty list is a prefix of everything, and a list is a prefix of itself
fun main()
  println([1,2].is-prefix-of([1,2,3]))
  println([1,2,3].is-prefix-of([1,2,3]))
  println(Nil.is-prefix-of([1]))
  println((Nil : list<int>).is-prefix-of(Nil))
  println([1,3].is-prefix-of([1,2,3]))
  println([1,2,3].is-prefix-of([1,2]))
  println(["a","b"].is-prefix-of(["a","b","c"]))
