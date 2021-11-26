inline fun maptwice(f : int -> _) : total list<list<int>> {
  [[1,2,3]].map(fn(xs) xs.map(f))
}

fun main() {
  maptwice(inc)[0].unjust[0].unjust.println
}