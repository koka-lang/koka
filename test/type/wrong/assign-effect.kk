// `x := e` updates `x` in place, so `e` must be total; `x ::= e` assigns any `e`
fun count() : console int
  var i := 0
  i := i + (fn() { println("tick"); 1 })()
  i
