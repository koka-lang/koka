// A local function declared in the scope of a `var` used to lose the effect row
// of whatever it called: `withvar/drain` was inferred `total` even though it calls
// `f`, so `withvar` itself came out `total` too, while the otherwise identical
// `withval` inferred correctly.
//
// Cause: `Type.InferMonad.isolate` would enter its isolating branch for a `local`
// effect, skip the unification that defines the replacement effect variable when
// the var-scope depth was non-zero, and then still return that fresh
// (unconstrained) variable -- discarding the whole row.
//
// The consequence was not just a wrong signature: evidence was built for a row the
// code never runs under, so `kk_evv_at` indexed past the end of the evidence vector
// and the program segfaulted at runtime.
pub effect eff1
  fun op1(): ()

pub fun withvar(f: (int) -> eff1 int)
  var total := 0
  fun drain(): _ int
    f(1)
  val r = drain()
  total := total + r
  total

pub fun withval(f: (int) -> eff1 int)
  val total = 0
  fun drain(): _ int
    f(1)
  val r = drain()
  total + r
