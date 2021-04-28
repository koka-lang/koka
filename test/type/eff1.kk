fun omega(): div () {
  return ()
}

fun main() {
  fun choose(x,y){ if (True) then x else y}
  choose(throw("hi"),omega())
}