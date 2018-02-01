fun foo1(b) {
  if (b) return 42
  println("hi")
  32
}

// checks if a mix of U and I in a match is handled to a I case (see Core.UnReturn.urTopDef)
val bar1 = match(True) {
  True -> (fun(x) { if (random-bool()) return 42; println("hi");  x })
  False -> id
}




// check for return under a bind
effect input {
  fun getstr() : string
}

fun foo2() {
  val x = match (getstr()) {
    "hi" -> return 42
    _    -> getstr()
  }
  if (x=="there") then 32 else 52
}

// check code duplication
fun foo3() {
  val x = match (getstr()) {
    "hi" -> return 42
    "ho" -> "there"
    _    -> getstr()
  }
  if (x=="there") then 32 else 52
}

fun main() {
  foo1(True)
}