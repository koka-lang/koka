fun choose(x,y) {
  if (True) then x else y
}

val ids = [id : forall<a> a -> a]

fun wrong(x) {
  choose(x,ids)
}