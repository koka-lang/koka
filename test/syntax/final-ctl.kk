// https://github.com/koka-lang/koka/issues/456
effect final ctl err-not-found(name: string): a

fun error-handler(eval: () -> <err-not-found|e> int): e string
  with final ctl err-not-found(name) "function not found: " ++ name
  eval().show

fun test()
  with error-handler
  "foo".println
  err-not-found("foo")
  0
