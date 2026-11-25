// Invariant: Core names are not guaranteed unique, so a `let` binder can rebind
// a name that a substitution maps. A term substitution must stop at such a
// binder: the names a definition group binds shadow the substitution for the
// groups after it and for the body (and, in a recursive group, for its own
// right-hand sides).
//
// Code that maintains this: `topDownLet` in `Core/Simplify.hs` (via `shadowSub`)
// and `|~>` in `Core/CoreVar.hs` (via `substDefGroups`). Fresh names from
// `uniqueTName` are only fresh for the current module's counter, while inlined
// bodies carry binders named by another module's counter, so any pass that
// substitutes into inlined code (beta-reduction, let-inlining) relies on it.
//
// At -O2, inlining `main` into `@main` and beta-reducing the `for` wrapper
// produces such a rebinding of `xs`.
fun main()
  var xs := []
  for(1,2) fn(i) xs := Cons(i, xs)
  println(xs.length)
