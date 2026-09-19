// Constructor-context paths (`conCtxPath`) must survive the .kki round-trip:
// `partition`, specialized from std/core/list, builds its context nodes from an
// inline definition, and the multi-shot handler below copies those shared
// contexts by following the paths. Needs cross-module specialization (-O1).
module cctx-context-path

effect duo
  ctl flip() : bool

pub fun main()
  with handler
    ctl flip()
      resume(True)
      resume(False)
  val (a, b) = [1, 2, 3, 4].partition(fn(x) if x == 3 then flip() else x.is-odd)
  println(a.show ++ " / " ++ b.show)
