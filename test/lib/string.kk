fun test-case-change()
  println("Добар Дан".to-lower == "добар дан")
  println("Добар Дан".to-upper == "ДОБАР ДАН")
  println("ßß".to-upper == "SSSS")
  println("İİİ".to-lower == "i̇i̇i̇")

fun test-trim()
  println("." ++ "".trim-left ++ "." == "..")
  println("." ++ "abc".trim-left ++ "." == ".abc.")
  println("." ++ " \t\xA0 a b \xA0 \xA0".trim-left ++ "." == ".a b \xA0 \xA0.")
  println("." ++ "\xA0\t\t".trim-left ++ "." == "..")
  
  println("." ++ "".trim-right ++ "." == "..")
  println("." ++ "abc".trim-right ++ "." == ".abc.")
  println("." ++ " \t\xA0 a b\xA0 \xA0".trim-right ++ "." == ". \t\xA0 a b.")
  println("." ++ "\xA0\t\t".trim-right ++ "." == "..")

  println("." ++ "".trim ++ "." == "..")
  println("." ++ "abc".trim ++ "." == ".abc.")
  println("." ++ " \t\xA0 a b \xA0 \xA0".trim ++ "." == ".a b.")
  println("." ++ "\xA0\t\t".trim ++ "." == "..")


// See https://github.com/koka-lang/koka/issues/486
fun main()
  val some-string = "hello"
  println("some-string = [" ++ some-string ++ "]")
  println("some-string.vector.string = [" ++ some-string.vector.string ++ "]")
  test-case-change()
  test-trim()