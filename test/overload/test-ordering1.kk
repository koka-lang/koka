// issue #468
// main.kk

type myType
  MyCase(value:int)

fun show(result:error<myType>) : pure string
  match result
    Ok(_m) -> "MyCase"
    Error(x) -> exn/show(x)

fun eq(left:error<myType>, right:error<myType>) : pure bool
  match (left,right)
    (Ok(_),Ok(_)) -> True
    (Error(l), Error(r)) -> l.message == r.message
    _ -> False

pub fun myFun(p1:a, p2:a, ?eq:(a,a) -> pure bool, ?show:a -> pure string)
  "hello"

fun main()
  myFun(Ok(MyCase(1)),Ok(MyCase(2)))
