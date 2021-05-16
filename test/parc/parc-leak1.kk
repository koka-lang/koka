fun main() {
  (match(Just((1,2))) { 
    Just(xy) -> xy
  }).fst.println
}
