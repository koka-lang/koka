// Bench mark to measure the effect of open floating
// Optimization todo: reduce the number of open call 
// from 4 to 1.

// Score (2021-12-04T09:12:07)
// Mac mini (2018); 3GHz 6-core intel core i5; 8GB 2667MHz DDR4
// Open floating :  
//   info: elapsed: 9.656s, user: 9.656s, sys: 0.002s, rss: 1020kb
// Plain :
//   info: elapsed: 9.999s, user: 9.998s, sys: 0.002s, rss: 1008kb

effect bar {
    fun bar_() : ()
}

fun k() : <bar> () { bar_() }

effect count {
    fun one_( a : int ) : int 
}

fun small( a : int ): count int {
    (if a.is-even then a + 1 else a).one_
    
}

fun f( n: int ) : _e int {
    var i := 0
    with fun bar_() { () }
    with handler {
        fun one_( a ) { i := a + 1; i }
        return(_) { i } 
    }
    while { i < n } 
    {
      k()                  // Use bar effect
      val a0 = i
      // optimization todo: float up opens to a restrict
      val a1 = small(a0)    // (open < bar, count, local<h> > small) a0
      val a2 = small(a1)
      val a3 = small(a2)
      val a4 = small(a3)
      val a5 = small(a4)
      val a6 = small(a5)
      val a7 = small(a6)
      val a8 = small(a7)
      val a9 = small(a8)
      small(a9)
      ()
    }
}

fun main(){
    f(1000000000)
    ()
}
