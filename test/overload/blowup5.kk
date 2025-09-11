fun paren/myshow( x : a, ?myshow : list<a> -> string ) : string
  "(" ++ myshow([x]) ++ ")"

fun test()
  1.myshow