fun int/myshowx( x : int ) : string
  x.show

fun bool/myshowx( x : bool ) : string
  x.show

fun amb/list/myshow( xs : list<a>, ?myshowx : a -> string ) : string
  xs.map(myshowx).join(",")

fun nonamb/list/myshow( xs : list<a> ) : string
  "[]"

fun wrong()
  [].myshow