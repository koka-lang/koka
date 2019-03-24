val ids = [id : forall<a> a -> a]

fun test1() {
  (head(ids).unjust)(1)
}