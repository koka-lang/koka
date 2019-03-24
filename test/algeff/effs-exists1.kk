// test operators with existential bindings

effect many {
  fun flip() : bool
  fun fail() : a
}

val many = handler {
  return x -> [x]
  flip()   -> resume(True) + resume(False)
  fail()   -> []
}

fun test() {
  if (flip()) then 1 else fail()
}

fun main() {
  many(test).map(show).join(",").println
}