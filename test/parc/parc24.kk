// nested match on an already-destructured polymorphic scrutinee: the
// known-scrutinee fold reconstructs the constructor witness with an
// explicit type application (`enrichConPattern` in Core/Simplify);
// the .out pins the typed witness shape under --showcoretypes.
struct pbox<a>
  fst : a
  snd : int

pub fun test( b : pbox<a> ) : (pbox<a>, int)
  match b
    Pbox(_, n) ->
      match b
        x as Pbox(_, _) -> (x, n + 1)

// record-copy on a polymorphic struct goes through the same fold
pub fun bump( b : pbox<a> ) : pbox<a>
  b(snd = b.snd + 1)
