// Recursive groups: signatures keep their quantifiers when divergence is added

// the signature orders its quantifiers differently than they occur
fun swap-down(t : list<a>, f : a -> <div|e> b, acc : list<b>) : <div|e> list<b>
  match t
    Cons(x,xx) -> swap-down(xx, f, Cons(f(x), acc))
    Nil -> swap-up(acc, f, [])

fun swap-up(t : list<b>, f : a -> <div|e> b, acc : list<b>) : <div|e> list<b>
  match t
    Cons(y,yy) -> swap-up(yy, f, Cons(y, acc))
    Nil -> acc

// a divergent function keeps the effect variable of its signature
fun spin<e>(n : int) : <div|e> int
  if n <= 0 then 0 else spin(n)
