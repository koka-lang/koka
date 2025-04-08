// emit a string
effect fun emit( s : xx<int, string> ) : ()

type xx<a,b>
  Val(a : a, b : b)

fun show(x: xx<a, b>, ?a/show: (a) -> pure string, ?b/show: (b) -> pure string) : pure string
  match x
    Val(a, b) -> a.show ++ " " ++ b.show

fun pretty-emit3(action : () -> <emit,pure|e> a, ?show: int -> pure string) : <pure|e> string
  var out := ""
  with fun emit(s) 
    out := out ++ s.show ++ "\n"
  action()
  out