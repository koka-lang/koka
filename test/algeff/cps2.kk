// triggers strange lifting during cps of letrec
// because the else branch needs cps translation (but not the rest)

val test = if (True || True) (fun(x){x}) else (fun(x) { !(ref(x)) })

fun main() {
  test(42).show.println
}