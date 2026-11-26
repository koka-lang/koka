// `x := e` updates `x` in place, so `e` cannot use an effect that a handler can intercept;
// `x ::= e` assigns any `e`
fun count() : exn int
  var i := 0
  i := i + (if i < 0 then throw("negative") else 1)
  i
