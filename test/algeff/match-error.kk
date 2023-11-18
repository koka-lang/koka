effect change<v>
  ctl change(v:v): maybe<v>

type greet
  Hi
  Bye

fun show(g: greet): string
  match g
    Hi -> "Hi"
    Bye -> "Bye"

fun test()
  match change(1)
    Just(v) | v % 2 == 1 -> Hi
    Nothing -> Bye

fun main()
  with handler
    ctl change(v)
      resume(Just(v))
      resume(Just(v + 1))
  test().show.println