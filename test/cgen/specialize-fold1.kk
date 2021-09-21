fun foo(f) {
  list(1,10).foldl( 0, f )
}

fun bar(f) {
  list(1,10).foldl1(f)
}

fun main() {
  // specializes
  list(1,10).foldl(0, (+)).show.println
  // does not specialize
  // seems like simplifyDupN does not inline the definition of (+) here
  // i.e. + is a Var, not a lambda
  foo((+)).show.println
  bar((+)).show.println
}