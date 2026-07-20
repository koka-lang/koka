// nested match on an already-destructured polymorphic scrutinee:
// the known-scrutinee fold must reconstruct the constructor witness
// with an explicit type application (see `enrichConPattern` in Core/Simplify),
// since the core parser resolves constructors to their generic scheme
// when the witness is inlined cross-module.
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
