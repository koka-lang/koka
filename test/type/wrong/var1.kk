fun escape() {
  local-var(1, fn(i) {
    i := i + 1
    (fn(){ i := i + 1; 10 })
  })
}

fun main() {
  //val f = escape()
  //f().println
  //f().println 
}