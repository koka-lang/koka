fun foo(f) {
  list(1,10).foldl( 0, f )
}

fun bar(f) {
  list(1,10).foldl1(f)
}

fun main() {
  // specializes
  list(1,10).foldl(0, (+)).show.println
  // this seems to specialize if the next call to bar is commented
  foo((+)).show.println
  // does not specialize; check if fold1 is specializable
  bar((+)).show.println
}