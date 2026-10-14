// Inlining this `main` into the generated `@main` beta-reduces the `for`
// wrapper, whose fresh parameter name can equal the name of `xs`'s later read
// (`xs@0@10003`). Substituting through that rebinding would make the inlined
// `length` read the local-var ref cell as a list (a crash at -O2).
fun main()
  var xs := []
  for(1,2) fn(i) xs := Cons(i, xs)
  println(xs.length)
