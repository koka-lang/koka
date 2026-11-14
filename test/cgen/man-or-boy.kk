// Regression for issue #966: the Man or boy test.
fun a(k, x1, x2, x3, x4, x5) : _ int
  if k <= 0 then x4() + x5()
  else
    val m = ref(k)
    fun b()
      m := !m - 1
      a(!m, b, x1, x2, x3, x4)
    b()

fun main()
  println(a(10, {1}, {-1}, {-1}, {1}, {0}))
