
fun showx( xs : list<a>, ?showx : a -> string ) : string
  xs.map(showx).join(",")

fun show( xs : list<_>, ?show )
  xs.map(show).join(",")