type tree<a>
  Bin(l : tree<a>, x : a, r : tree<a>)
  Tip

fun tree(n : int)
  if n <= 0
    then Tip
    else
      val m = n - 1
      val l = tree( m / 2 )
      val r = tree( m - m / 2 )
      Bin(l, n, r)

fun tmap(t : tree<a>, f : a -> e b) : e tree<b>
  match t
    Tip -> Tip
    Bin(l, x, r) -> Bin(l.tmap(f), f(x), r.tmap(f))

fun tsum(t : tree<int>) : div int
  tsum'(t, 0)

fun tsum'(t : tree<int>, acc : int) : div int
  match t
    Tip -> acc
    Bin(l, x, r) -> tsum'(l, tsum'(r, acc + x))

fun test(n : int)
  val xs = tree(n)
  val x = fold-int(0,100000000/(if n<=0 then 1 else n),0) fn(i,acc)
            // acc + xs.mapxx(fn(x){ x+1 }).sum
            acc + xs.tmap(fn(x){ x+1 }).tsum
  println("total: " ++ x.show)