// triggers strange lifting during cps of letrec
// because the else branch needs cps translation (but not the rest)

val test = if (True || True) then (fn(x){x}) else (fn(x) { !(ref(x)) })

fun main() {
  test(42).show.println
}