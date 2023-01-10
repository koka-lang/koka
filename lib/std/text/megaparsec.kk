pub effect parse<x::V,s::V>
  final ctl fail(error : x) : a
  ctl pick() : bool
  fun satisfy(pred : (s) -> maybe<(a,s)>) : maybe<a>

pub alias parser<e,x,s,a> = () -> <parse<x,s>|e> a

pub type result<x,s,a>
  Done(result: a, rest : s)
  Fail(error : x, rest : s)

pub fun parse(input : sslice, p : parser<e,x,sslice,a>) : e result<x,sslice,a>
  var current := input
  handle p
    return(x)
      Done(x, current)

    final ctl fail(msg)
      Fail(msg, current)

    ctl pick()
      val save = current // save current input
      match resume(True)
        Done(x, rest) -> Done(x, rest)
        _ ->
          current := save // backtrack to saved input
          resume(False)

    fun satisfy(pred)
      match pred(current)
        Just((init, rest)) ->
          current := rest
          Just(init)
        Nothing -> Nothing

fun psomething(): <parse<string, sslice>> char
  match satisfy(fn(x:sslice) if x.string == "a" then Just(('a', x)) else Nothing)
    Just(x) -> x
    Nothing -> fail("expected 'a'")

fun show(result: result<string,sslice,char>)
  match result
    Done(r, rest) -> "Got: " ++ r.string ++ " Left: " ++ rest.string
    Fail(msg, rest) -> "Failed: " ++ msg ++ " Left: " ++ rest.string

fun main()
  parse("".slice, psomething).show.println