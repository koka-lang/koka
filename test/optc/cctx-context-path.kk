// REGRESSION TEST: constructor-context paths survive the core interface
// round-trip.
//
// `AnalysisCCtx`/`CTail` stamp `conCtxPath` on the ConRepr of each
// constructor allocation that forms a node of a constructor context; the C
// backend turns the stamp into the block's `_field_idx`, which
// `kk_cctx_copy` follows at runtime to copy a SHARED context. The stamp was
// never serialized into `.kki` interfaces, so inline definitions parsed back
// from an interface lost their paths: here, `partition` specialized from
// std/core/list allocated its context nodes with `_field_idx` 0, and the
// multi-shot handler below (both resumes share the captured in-progress
// contexts) forced the copy path, which then walked garbage -- a segfault.
//
// Needs cross-module specialization (-O1, this suite's flag) and a
// multi-shot handler; at -O0, same-module, or single-shot it always passed.
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
