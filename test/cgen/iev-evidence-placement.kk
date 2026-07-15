// Implicit evidence (the hdiv `iev` variables) must be bound in the core
// that contains its occurrences. Local `var` vectors mutated in nested
// `for` loops next to a local recursive `fun`, under <exn,div>, used to
// leave the evidence unbound: the local fun's generalization solved the
// pending constraints and placed the evidence defs in its own (sibling)
// core, and the dangling variables surfaced as undeclared `iev_N`
// identifiers in the generated C once the monadic transform lifted them
// into yield continuations.

// longest increasing subsequence, imperative O(n^2) DP
fun lis( xs : list<int> ) : <div,exn> list<int>
  val v = xs.filter(fn(x) x >= 0).vector
  if v.length == 0 then [] else
    // best[i] = length of the LIS ending at i; prev[i] = predecessor index
    var best := vector(v.length, 1)
    var prev := vector(v.length, -1)
    for(0, v.length - 1) fn(i)
      for(0, i - 1) fn(j)
        if v[j] < v[i] && best[j] + 1 > best[i] then
          best[i] := best[j] + 1
          prev[i] := j
    var end := 0
    for(0, v.length - 1) fn(i)
      if best[i] > best[end] then end := i
    fun walk( i, acc )
      if i < 0 then acc else walk(prev[i], Cons(v[i], acc))
    walk(end, [])

pub fun main()
  println( lis([3,1,4,1,5,9,2,6]).map(show).join(",") )
