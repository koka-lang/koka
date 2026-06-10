// Perceus miscompiled sequential match expressions when an inner match arm
// called a 2-arg function (https://github.com/koka-lang/koka/issues/894)

type myval
  MStr(s : string)
  MInt(n : int)

fun my-parse() : either<string, list<(string, myval)>>
  Right([("title", MStr("hello")), ("count", MInt(42))])

fun my-get(doc : list<(string, myval)>, key : string) : maybe<myval>
  match find(doc, fn(e) e.fst == key)
    Just(e) -> Just(e.snd)
    Nothing -> Nothing

// 1-arg version — passes
fun check1( b : bool ) : exn ()
  if !b then throw("fail")

// 2-arg version — used to corrupt the match dispatch of the next sequential match
fun check2( b : bool, msg : string ) : exn ()
  if !b then throw(msg)

fun test-passes()
  match my-parse()
    Right(doc) ->
      match my-get(doc, "title")
        Just(MStr(v)) -> check1(v == "hello")
        _ -> check1(False)
      match my-get(doc, "count")
        Just(MInt(_)) -> check1(True)
        _ -> check1(False)
    Left(_) -> check1(False)

fun test-fails()
  match my-parse()
    Right(doc) ->
      match my-get(doc, "title")
        Just(MStr(v)) -> check2(v == "hello", "bad title")
        _ -> check2(False, "no title")
      match my-get(doc, "count")
        Just(MInt(_)) -> check2(True, "ok")
        _ -> check2(False, "no count")
    Left(_) -> check2(False, "parse fail")

fun main()
  match try({ test-passes() })
    Ok(_) -> println("test-passes: ok")
    Error(e) -> println("test-passes: FAIL — " ++ e.message)
  match try({ test-fails() })
    Ok(_) -> println("test-fails: ok")
    Error(e) -> println("test-fails: FAIL — " ++ e.message)
