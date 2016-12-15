function id(x) -> x

function id2(x) { x }

function id3(x:a) : a { x }

function id4(x:a) : a
{ x  }

function id5<a>(x : a ) : a
{
  x
}

val id6 : forall<a> a -> a = fun(x) { x }