fun id(x) = x

fun id2(x) { x }

fun id3(x:a) : a { x }

fun id4(x:a) : a
{ x  }

fun id5<a>(x : a ) : a
{
  x
}

val id6 : forall<a> a -> a = fun(x) { x }