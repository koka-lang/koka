// A recursive local function matched on its own result (`val (a,b) = go(..)`):
// the recursive call is a type application once the group is generalized, and
// its variable must already carry the generalized type -- divergence analysis
// takes the type of the match scrutinee before the variable info is fixed up,
// and failed with "splitTForall: Expected forall".
fun take( xs0 : list<(string, int)>, k : string ) : div maybe<int>
  fun go( xs )
    match xs
      Nil -> (0, Nothing)
      Cons((key, entry), rest) ->
        if key == k then (1, Just(entry))
        else
          val (n, found) = go(rest)
          (n + 1, found)
  go(xs0).snd

pub fun main()
  println(take([("a",1),("b",2)],"b").show)
  println(take([("a",1),("b",2)],"c").show)
