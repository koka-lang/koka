function twice(f,x) {
  return f(f(x))
}

function twice2(f,x) {
  return id(f(id(f(x))))
}