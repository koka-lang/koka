val ids1 = [id]
val ids2 = [id : forall<a> a -> a]

function ok(f) {
  choose(id,f)
}