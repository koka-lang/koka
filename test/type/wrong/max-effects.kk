// an argument that can throw does not satisfy `max-effects<e,<div,ndet>>`
fun apply-max(f : () -> e a, ?max-effects : max-effects<e,<div,ndet>>) : e a
  f()

fun g() : exn int
  apply-max(fn() throw("no"))
