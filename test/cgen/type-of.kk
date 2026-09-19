type at<a,l>
  Local(v: a)
  Remote

fun unwrap(x: at<a,_>): a
  match x
    Local(v) -> v
    Remote -> impossible("invariant violation")

alias khor<k,e> = <khor-div<k,e>,div|e>
div effect khor-div<k,e>
  fun locally(loc1: k<l1>, khor: (forall<w> at<w,l1> -> w) -> khor<k,e> a): at<a,l1>

extern loc/unsafe-cast(x: k<l1>): k<l2> { inline "#1" }

fun epp1/epp(loc1: k<l1>, khor: () -> khor<k,e> a, ?(==): forall<x,y> (k<x>,k<y>) -> bool): <div|e> at<a,l1>
  with handler
    return(v) Local(v)
    fun locally(loc1', khor') match loc1 == loc1'
      False -> Remote
      True -> loc1.unsafe-cast.epp(fn() khor'(unwrap))
  khor()

fun main()
  println("runs")