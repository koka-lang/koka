// Regression test for a type-synonym round-trip bug in module interfaces.
//
// `syn-nested1c` uses the `my-alias` synonym (defined in `syn-nested1a`) only on
// an inferred, nested lambda parameter deep inside `use-cb`'s body -- never in
// any of its own declared signatures. Core.Core.extractSignatures used to only
// look at each def's own top-level signature type when deciding which type
// synonyms to mirror into a module's compiled interface, so this nested use was
// never mirrored into `syn-nested1c`'s own interface. Reloading that interface
// (e.g. compiling this file against an unchanged, previously-compiled
// `syn-nested1c`) then resolved `my-alias` to a bare, unexpandable type
// constructor instead of the synonym, and the C backend failed with
// "unknown type name ..._my_alias" (or, before that, Core.Parc crashed outright
// with "cannot find type: ...").
//
// This file relies on `syn-nested1a`/`syn-nested1b`/`syn-nested1c` having
// already been compiled (and their interfaces cached) as their own tests
// earlier in this directory, so importing them here exercises the interface
// *reload* path rather than a fresh from-source check.

module syn-nested1
import syn-nested1c

pub fun run() : int
  use-cb()
