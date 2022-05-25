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
    Bin(l, x, r) -> Bin(l.tmap(f), f(x), r.tmap(f))
    Tip -> Tip
    

fun tmap1(t : tree<a>, f : a -> e b) : e tree<b>
  match t
    Tip -> Tip
    _   -> tmap(t,f)

fun tsum(t : tree<int>) : div int
  tsum'(t, 0)

fun tsum'(t : tree<int>, acc : int) : div int
  match t
    Tip -> acc
    Bin(l, x, r) -> tsum'(l, tsum'(r, acc + x))

fun test(n : int)
  val t = tree(n)
  val x = fold-int(0,100000000/(if n<=0 then 1 else n),0) fn(i,acc)
            acc + t.tmap(fn(x){ x+1 }).tsum
            //acc + t.tmap1(fn(x){ x+1 }).tsum
  println("total: " ++ x.show)