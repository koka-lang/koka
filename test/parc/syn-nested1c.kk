module syn-nested1c
import syn-nested1b

// note: `my-alias` never appears in this signature -- the callback lambda's
// parameter type below is only ever *inferred* (via unification with
// `apply-cb`'s parameter type), not declared here. A type synonym used only
// in such a nested position used to fail to round-trip through this module's
// own compiled interface (see syn-nested1.kk).
pub fun use-cb() : int
  apply-cb( fn(xs) xs.length )
