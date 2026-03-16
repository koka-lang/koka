fun paren/myshow( x : a, ?myshow : list<a> -> string ) : string
  "(" ++ myshow([x]) ++ ")"

fun wrong()
  1.myshow

fun main()
  print("hi")