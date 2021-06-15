
private fun test(x) {
  if (not(x)) then x else False
  /*
  r = if (x) then False else True
  if r then x else False
  ~> { case-of-case }
  if (x) then (if False then x else False) else (if (x) then x else False)
  ~>
  if (x) then False else x
  */
}

fun main() {
  /*
  match(True) {
    True -> "hi".println
    False -> "no".println
  */
  test(True).println
}
