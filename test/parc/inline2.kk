type alpha<a> { A(x: a, y: beta<a>) }
type beta<a> { con B(x: a) B2 }

fun test(x) {
  match(x) {
    A(y, B(z))
      -> B(y) // reuse inner B
    _ -> B2
  }
}
