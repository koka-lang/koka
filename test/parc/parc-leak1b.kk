fun main() {
  (match(Just([1,2])) { 
    Just(xy) -> xy
  }).head.default(42).println
}
