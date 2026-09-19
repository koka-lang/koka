// A function that is multi-step specializable (fexec-loop calls feval which is
// specializable on `e` through the lambda passed to `map`) but whose recursive
// call does not pass the specialized parameter `e` along unchanged (it passes
// `fset(e,...)`, which stays a "good" argument because the total `fset` is
// `@open`-ed into the caller's effect row instead of being beta-reduced).
// The specializer used to loop forever on this at -O1 and up, printing
// "specInnerCalls: argument does not match" with a core dump to stderr each
// round (issue: specializer loops / unbounded specInnerCalls traces).
// Now the specializer declines to specialize such calls.

effect viol-flag
  fun mark-viol(knd: string): ()

type rv
  RInt(n: int)
  RArr(elems: list<rv>)

alias fenv = list<(string, rv)>

type fexpr
  FLit(n: int)
  FVar(name: string)
  FArr(elems: list<fexpr>)

fun fget(e: fenv, key: string): rv
  match e.filter(fn(p) p.fst == key)
    Cons((_, v), _) -> v
    Nil -> RInt(0)

fun fset(e: fenv, key: string, v: rv): fenv
  Cons((key, v), e.filter(fn(p) p.fst != key))

fun as-int(v: rv): exn int
  match v
    RInt(n) -> n
    _ -> throw("int expected")

fun feval(e: fenv, ex: fexpr): <div,exn,viol-flag> rv
  match ex
    FLit(n) ->
      if n == 0 then mark-viol("zero")
      RInt(n)
    FVar(x) -> fget(e, x)
    FArr(elems) -> RArr(elems.map(fn(el) feval(e, el)))

fun fexec-loop(e: fenv, c: string, body: fexpr): <div,exn,viol-flag> rv
  if fget(e, c).as-int == 0 then feval(e, body)
  else fexec-loop(fset(e, c, RInt(fget(e, c).as-int - 1)), c, body)

fun fexec(e: fenv, c: string, b: int, body: fexpr): <div,exn,viol-flag> rv
  fexec-loop(fset(e, c, RInt(b)), c, body)

fun run-program(body: fexpr): <div,exn> rv
  var first := ""
  with fun mark-viol(knd)
    if first == "" then first := knd
  fexec([], "c", 3, body)

pub fun main()
  match run-program(FArr([FLit(1), FVar("c")]))
    RArr(Cons(v, _)) -> println(v.as-int)
    _ -> println("unexpected")
