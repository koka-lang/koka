fun twice(f,x) {
  return f(f(x))
}

fun twice2(f,x) {
  return id(f(id(f(x))))
}