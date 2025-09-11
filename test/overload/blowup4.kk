fun paren/myshow( x : a, ?myshow : a -> string ) : string
  "(" ++ myshow(x) ++ ")"

fun test()
  1.myshow