// Specializing mutually recursive functions with function-typed arguments
// must terminate (this file made the specializer diverge): an ancestor is
// only specialized again at a strictly smaller instantiated type (see
// specStack in Core/Specialize.hs). Multi-step specialization at shrinking
// types (twostep-large2.kk) must keep working.
module cgen/mutualspec
import cgen/mutualspec-lib

fun view( sel : int ) : html<int>
  Elem(0, list(1, 4).map( fn(i) Elem(i, [Text(if i == sel then 1 else 0)]) ))

pub fun main()
  val emit = fn(m : int) ()
  val t0 = build(view(0), emit)
  val t1 = patch(t0, view(1), emit)
  val t2 = patch(t1, view(2), emit)
  match t2
    NElem(_, kids) -> println(kids.length)
    NText(_) -> println(-1)
