fun choose(x,y){ if (True) then x else y}

val ids1 = [id]
val ids2 = [id : forall<a> a -> a]

fun ok(f) {
  choose(id,f)
}