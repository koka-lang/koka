fun paren/myshow( x : a, ?myshow : a -> string ) : string
  "(" ++ myshow(x) ++ ")"

fun wrong()
  1.myshow