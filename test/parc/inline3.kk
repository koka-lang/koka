type alpha<a> { con A(x: a, y: beta<a>) A2 }
type beta<a> { B(x: a) }

fun test(x) {
  match(x) {
    A(y, B(z))
      -> A(z, B(y)) // reuse both x and inner B
    _ -> A2
  }
}
