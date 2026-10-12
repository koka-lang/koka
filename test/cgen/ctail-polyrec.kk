// TRMC on a polymorphically recursive function (the recursive call is at `(a,a)`),
// with an effect-polymorphic argument so the multi-resumption variant is generated too.
type nest<a>
  Done
  Layer(x : a, inner : nest<(a,a)>)

fun mk(n : int, x : a, f : a -> <div|e> a) : <div|e> nest<a>
  if n <= 0 then Done
  else Layer(f(x), mk(n - 1, (x,x), fn(p) (f(p.fst), f(p.snd))))

fun sum(t : nest<a>, g : a -> int) : int
  match t
    Done -> 0
    Layer(x, inner) -> g(x) + sum(inner, fn(p) g(p.fst) + g(p.snd))

effect choose
  ctl flip() : bool

fun all-choices(action : () -> <choose,div> a) : div list<a>
  with ctl flip() append(resume(True), resume(False))
  [action()]

fun main()
  val t = mk(6, 1, fn(i) i + 1)
  println(sum(t, id))
  val ts = all-choices { mk(3, 1, fn(i) if flip() then i + 1 else i * 10) }
  println(ts.map(fn(t') sum(t', id)).show)
