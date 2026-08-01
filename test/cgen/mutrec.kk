// Mutual-recursion fusion (Core/Fusion) test suite.
// Deep-recursion cases (n >= 100000) would stack-overflow without the pass.

// 1. Two-way mutual tail recursion, single int argument
fun my-even( n : int ) : div bool
  if n == 0 then True else my-odd(n - 1)

fun my-odd( n : int ) : div bool
  if n == 0 then False else my-even(n - 1)

// 2. Two-way mutual tail recursion, two arguments (wider @Call constructors)
fun count-down( n : int, acc : int ) : div int
  if n == 0 then acc else count-up(n - 1, acc + 1)

fun count-up( n : int, acc : int ) : div int
  if n == 0 then acc else count-down(n - 1, acc + 1)

// 3. Three-way mutual tail recursion (SCC of size 3)
fun f3( n : int ) : div int
  if n <= 0 then 0 else g3(n - 1)

fun g3( n : int ) : div int
  if n <= 0 then 1 else h3(n - 1)

fun h3( n : int ) : div int
  if n <= 0 then 2 else f3(n - 1)

// 4. Mixed tail / non-tail in the same function
fun ping( n : int ) : div int
  if n <= 0 then 0
  elif n % 2 == 0 then 1 + pong(n - 1)  // non-tail
  else pong(n - 1)                        // tail

fun pong( n : int ) : div int
  if n <= 0 then 0
  elif n % 2 == 1 then 1 + ping(n - 1)  // non-tail
  else ping(n - 1)                        // tail

// 5. Polymorphic mutual recursion, mismatched argument counts (2 vs 3).
//    zip-even/zip-odd's own type variables are independent existentials on
//    their own @Call-* constructor (see test 8). The sibling call here is
//    nested under Cons (modulo-cons); this pass does not fuse that into the
//    loop -- it's an ordinary non-tail call, correct but O(n) stack, so only
//    a shallow case is exercised here (a follow-up adds constant-stack
//    modulo-cons fusion).
fun zip-even( xs : list<a>, ys : list<b> ) : div list<(a,b)>
  match xs
    Nil        -> []
    Cons(x,xx) -> zip-odd(xx, ys, x)

fun zip-odd( xs : list<a>, ys : list<b>, saved : a ) : div list<(a,b)>
  match ys
    Nil        -> []
    Cons(y,yy) -> Cons((saved,y), zip-even(xs, yy))

// 6. Self-recursive (single-member SCC) — should NOT be fused.
fun self-sum( n : int, acc : int ) : div int
  if n <= 0 then acc else self-sum(n - 1, acc + n)

// 7. Mixed raw/scan field types across constructors.
//    even-step carries two booleans (raw); odd-step carries a list (scan).
fun even-step( n : int, flag1 : bool, flag2 : bool ) : div int
  if n <= 0 then (if flag1 then 1 else 0) + (if flag2 then 1 else 0)
  else odd-step(n - 1, [flag1, flag2])

fun odd-step( n : int, flags : list<bool> ) : div int
  match flags
    Cons(f1, Cons(f2, _)) -> even-step(n - 1, f1, f2)
    _ -> 0

// 8. Genuine polymorphic recursion: `s`'s type parameter is bound as an
//    existential on each @Call-* constructor (not shared by the mutrec
//    datatype -- see test 10), and each recursive call re-instantiates it as
//    (a,a) instead of reusing the enclosing call's own `a` -- unlike test 5,
//    where the existential keeps the same instantiation across every call in
//    the SCC. Exercises passing the call site's actual type argument (rather
//    than assuming it equals the caller's own) as the next @Call-*'s
//    existential type argument.
type pseq<a>
  PNil
  PZero( s : pseq<(a,a)> )
  POne( x : a, s : pseq<(a,a)> )

fun even-len( s : pseq<a>, acc : int ) : div int
  match s
    PNil -> acc
    PZero(s') -> odd-len(s', acc)
    POne(_x,s') -> odd-len(s', acc + 1)

fun odd-len( s : pseq<a>, acc : int ) : div int
  match s
    PNil -> acc
    PZero(s') -> even-len(s', acc)
    POne(_x,s') -> even-len(s', acc + 1)

// 9. Mutual tail recursion polymorphic over an *effect* type variable
//    (not just a value type) -- exercises that the synthesized mutrec
//    type's kind matches each type variable's own kind (e.g. E, not V).
fun run-a( n : int, step : (int) -> e int ) : <div|e> int
  if n <= 0 then 0 else run-b(n - 1, step)

fun run-b( n : int, step : (int) -> e int ) : <div|e> int
  if n <= 0 then 1 else run-a(n - 1, step)

// 10. Members whose own type parameters differ: mixed-a generalizes to
//     <a,e>, mixed-b only to <e>. The mutrec type's shared datatype
//     parameters are just the effect `e` and the driver's result index --
//     @mutrec-mixed-a<(e::E),a> -- and mixed-a's own value tv becomes an
//     existential on its OWN constructor, @Call-mixed-a<b>, never a datatype
//     slot; @Call-mixed-b has no existential at all. So members can freely
//     differ in how many value type parameters they bind -- each redirect
//     just supplies that constructor's own existential type argument (e.g.
//     @Call-mixed-a<e,a,int>) -- only the *effect* parameters must align
//     across the group (see test 9). The external caller below pins that
//     mixed-b's arity is not widened: each wrapper keeps its own original
//     signature (call sites are already fixed by the time this pass runs).
fun mixed-a( x : a, step : (int) -> e int, n : int ) : <div|e> int
  if n <= 0 then 0 else mixed-b(step, n - 1)

fun mixed-b( step : (int) -> e int, n : int ) : <div|e> int
  if n <= 0 then 1 else mixed-a(0, step, n - 1)

fun use-mixed-b( n : int ) : div int
  mixed-b(fn(y){ y + 1 }, n)

// 11. The same member tail-called at two DIFFERENT instantiations from one
//     driver: pick-a sends int to poly-b in one branch and bool in the other.
//     Each redirected call supplies its own @Call-poly-b<..> existential type
//     argument directly (@Call-poly-b<int> / @Call-poly-b<bool>), so the two
//     branches type independently -- there is no single shared slot that
//     would need one fixed instantiation for the whole @run. pick-a itself
//     binds no type parameters, so @mutrec-pick-a<a> carries only the
//     driver's result index; poly-b's own `v : x` is entirely its
//     constructor's existential (@Call-poly-b<b>), never a mutrec datatype
//     parameter, so there is nothing to fill in when it is out of scope.
fun pick-a( n : int ) : div int
  if n <= 0 then 0
  elif n % 2 == 0 then poly-b(n, fn(i : int){ i * 2 }, n - 1)
  else poly-b(True, fn(t : bool){ if t then 42 else 7 }, n - 1)

fun poly-b( v : x, f : (x) -> int, n : int ) : div int
  if n <= 0 then f(v) else pick-a(n - 1)

// 12. Mutually recursive group with DIFFERENT effect rows -- consumer USES ask,
//     producer HANDLES it around the recursive call. Only expressible with
//     annotations (inference rejects the unannotated and effect-polymorphic
//     forms: the recursive occurrence forces `e ~ <ask|e>`). The rows cannot
//     align, so the group is left unfused -- this pins the safe fallback
//     (correct results, no miscompile). Deep runs nest a handler per round trip
//     and are inherently O(n) frames (identical with --fno-fusion), so the
//     depth stays modest.
effect ask
  fun ask() : int

fun consumer( n : int ) : <ask,div> int
  if n <= 0 then ask() else n + producer(n - 1)

fun producer( n : int ) : div int
  with fun ask() n * 10 + 7
  consumer(n)

// 13. Both members share one row *including a handled effect* (`<ask,div>`):
//     the group fuses, and deep mutual tail recursion under ONE outer handler
//     (installed in main) runs in constant stack.
fun pinger( n : int ) : <ask,div> int
  if n <= 0 then ask() else ponger(n - 1)

fun ponger( n : int ) : <ask,div> int
  if n <= 0 then ask() + 1 else pinger(n - 1)

// 14. OVERRIDE with matching rows: both members are `<ask,div>` (so the group
//     fuses), but ov-b overrides ask around its recursive call -- each
//     ov-b(n>0) adds one increment layer. Safe by construction: in core every
//     evidence-changing construct (handler/override/mask) wraps its scope in a
//     lambda passed to the handler primitive, and the redirect only fires on a
//     direct sibling application at a tail leaf -- so a call under an override
//     can never be redirected; it re-enters @run through the wrapper under the
//     new evidence. A wrong redirect here would miscount the override layers.
fun ov-a( n : int ) : <ask,div> int
  if n <= 0 then ask() else ov-b(n - 1)

fun ov-b( n : int ) : <ask,div> int
  if n <= 0 then ask()
  else
    with override fun ask() ask() + 1
    ov-a(n - 1)

fun main()
  // 1: basic correctness + deep
  println(my-even(1).show)          // False
  println(my-even(100000).show)     // True

  // 2: multi-arg, deep
  println(count-down(4, 0).show)    // 4
  println(count-down(100000, 0).show) // 100000

  // 3: three-way cycle, deep
  println(f3(2).show)               // 2
  println(f3(100000).show)          // 1

  // 4: mixed tail/non-tail
  println(ping(6).show)             // 6

  // 5: polymorphic, mismatched arg counts (shallow: modulo-cons isn't fused yet)
  val r = zip-even([1,2,3], ['a','b','c'])
  println(r.show-list(fn((a,b)) { "(" ++ a.show ++ "," ++ b.show ++ ")" }))

  // 6: single-member SCC (no trampolining)
  println(self-sum(10, 0).show)     // 55

  // 7: mixed raw/scan fields, deep
  println(even-step(2, True, False).show)       // 1
  println(even-step(100000, True, True).show)   // 2

  // 8: genuine polymorphic recursion
  val s8 = PZero(POne((1,2), PNil))
  println(even-len(s8, 0).show)     // 1

  // 9: effect-polymorphic mutual tail recursion, deep
  println(run-a(5, fn(x){ x }).show)       // 1
  println(run-a(100000, fn(x){ x }).show)  // 0

  // 10: mismatched type variable counts (not fused, must stay correct)
  println(mixed-a(True, fn(y){ y }, 5).show)  // 1
  println(use-mixed-b(5).show)                // 0
  println(use-mixed-b(100000).show)           // 1

  // 11: one callee, two instantiations in the same driver
  println(pick-a(1).show)                                             // 42 (bool branch)
  println(pick-a(4).show)                                             // 0
  println(poly-b(7, fn(i : int){ i * 2 }, 0).show)                    // 14 (int base)
  println(poly-b(True, fn(t : bool){ if t then 42 else 7 }, 0).show)  // 42 (bool base)
  println(pick-a(100000).show)                                        // 0  (deep)

  // 12: differing effect rows (unfused, safe): 3+2+1 + innermost ask() = 7
  println(producer(3).show)             // 13

  // 13: shared handled-effect row (fused), deep under one outer handler
  with fun ask() 40
  println(pinger(4).show)               // 40 (even -> ends in pinger's ask())
  println(pinger(3).show)               // 41 (odd  -> ends in ponger's ask()+1)
  println(pinger(100000).show)          // 40 (deep, constant stack)

  // 14: override with matching rows (fused; layers must count exactly)
  println(ov-a(0).show)                 // 40 (no overrides)
  println(ov-a(4).show)                 // 42 (two layers: ov-b(3), ov-b(1))
  println(ov-a(100).show)               // 90 (fifty layers)
