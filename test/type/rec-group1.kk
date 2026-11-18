// Recursive groups: members are generalized jointly and signatures keep their quantifiers

// mutual recursion without annotations
fun dup(x, n : int)
  if n <= 0 then [x] else dup2(x, n - 1)

fun dup2(y, n : int)
  match dup(y, n)
    Cons(z,_) -> [z,z]
    Nil -> []

// a signature in a group with unannotated members
fun pairs(xs : list<a>) : div list<(a,a)>
  match xs
    Cons(x,xx) -> Cons((x,x), more(xx))
    Nil -> Nil

fun more(ys)
  pairs(ys)

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
