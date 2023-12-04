// issue: https://github.com/koka-lang/koka/issues/60
effect gen<a> {
  ctl yield(item: a): ()
}

fun list<a,e>(program : () -> <gen<a>, e> ()): e list<a> {
  handle(program) {
    yield(x) -> Cons(x, resume(()))
    return _ -> Nil
  }
}