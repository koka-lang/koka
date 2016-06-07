val ids = [id : forall<a> a -> a]

function test1() {
  (head(ids).unjust)(1)
}