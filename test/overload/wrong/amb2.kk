// infinite
fun myshowx( x : a, ?myshowx : a -> string ) : string
  ""

fun amb/list/myshow( xs : list<a>, ?myshowx : a -> string ) : string
  xs.map(myshowx).join(",")

fun nonamb/list/myshow( xs : list<a> ) : string
  "[]"

fun wrong()
  [].myshow