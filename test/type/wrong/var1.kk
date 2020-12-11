fun escape() {
  var i := 1
  fn(){ i := i + 1; i }  
}

fun main() {
  val f = escape()
  f().println
  f().println 
}