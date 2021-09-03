/* Example of red-black tree<a> balanced insertion.
   See also the benchmarks in `test/bench/koka`
*/

type color
  Red
  Black

type tree<a>
  Leaf
  Node(color : color, left : tree<a>, key : int, value : a, right : tree<a>)

fun is-red(t : tree<a>) : bool
  match t
    Node(Red) -> True
    _         -> False

fun balance-left(l :tree<a>, k : int, v : a, r : tree<a>) : tree<a>
  match l
    Leaf -> Leaf
    Node(_, Node(Red, lx, kx, vx, rx), ky, vy, ry)
      -> Node(Red, Node(Black, lx, kx, vx, rx), ky, vy, Node(Black, ry, k, v, r))
    Node(_, ly, ky, vy, Node(Red, lx, kx, vx, rx))
      -> Node(Red, Node(Black, ly, ky, vy, lx), kx, vx, Node(Black, rx, k, v, r))
    Node(_, lx, kx, vx, rx)
      -> Node(Black, Node(Red, lx, kx, vx, rx), k, v, r)

fun balance-right(l : tree<a>, k : int, v : a, r : tree<a>) : tree<a>
  match r
    Leaf -> Leaf
    Node(_, Node(Red, lx, kx, vx, rx), ky, vy, ry)
      -> Node(Red, Node(Black, l, k, v, lx), kx, vx, Node(Black, rx, ky, vy, ry))
    Node(_, lx, kx, vx, Node(Red, ly, ky, vy, ry))
      -> Node(Red, Node(Black, l, k, v, lx), kx, vx, Node(Black, ly, ky, vy, ry))
    Node(_, lx, kx, vx, rx)
      -> Node(Black, l, k, v, Node(Red, lx, kx, vx, rx))

fun ins(t : tree<a>, k : int, v : a) : tree<a>
  match t
    Leaf -> Node(Red, Leaf, k, v, Leaf)
    Node(Red, l, kx, vx, r)
      -> if k < kx then Node(Red, ins(l, k, v), kx, vx, r)
         elif k == kx then Node(Red, l, k, v, r)
         else Node(Red, l, kx, vx, ins(r, k, v))
    Node(Black, l, kx, vx, r)
      -> if k < kx then (if is-red(l) then balance-left(ins(l,k,v), kx, vx, r)
                                      else Node(Black, ins(l, k, v), kx, vx, r))
         elif k == kx then Node(Black, l, k, v, r)
         elif is-red(r) then balance-right(l, kx, vx, ins(r,k,v))
         else Node(Black, l, kx, vx, ins(r, k, v))

fun set-black(t : tree<a>) : tree<a>
  match t
    Node(_, l, k, v, r) -> Node(Black, l, k, v, r)
    _ -> t

// Tree insertion
fun insert(t : tree<a>, k : int, v : a) : tree<a>
  if is-red(t)
    then set-black(ins(t, k, v))
    else ins(t, k, v)


// Fold the elements of a tree
fun fold(t : tree<a>, b : b, f : (int, a, b) -> b) : b
  match t
    Leaf                -> b
    Node(_, l, k, v, r) -> r.fold( f(k, v, l.fold(b, f)), f)

// Make a boolean tree
fun make-tree-aux(n : int, t : tree<bool>) : div tree<bool>
  if n <= 0 then t else
    val n1 = n - 1
    make-tree-aux(n1, insert(t, n1, n1 % 10 == 0))

fun make-tree(n : int) : div tree<bool>
  make-tree-aux(n, Leaf)


// Benchmark for `n` insertions
fun bench(n : int)
  val m = make-tree(n)
  val v = m.fold(0) fn(k,v,r){ if (v) then r + 1 else r }
  v.println


fun main()
  bench(4200000)

