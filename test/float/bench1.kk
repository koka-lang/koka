// Bench mark to measure the effect of open floating
// Optimization todo: reduce the number of open call 
// from 4 to 1.

import std/os/env

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
      k()                  // Use bra effect
      val a0 = i
      // optimization todo: float up opens a restrict
      val a1 = small(a0)    // (open <bar, count> small_) a0
      val a2 = small(a1)    // (open <bar, count> small_) a1
      val a3 = small(a2)    // (open <bar, count> small_) a2
      val a4 = small(a3)    // (open <bar, count> small_) a2
      val a5 = small(a4)    // (open <bar, count> small_) a2
      val a6 = small(a5)    // (open <bar, count> small_) a2
      val a7 = small(a6)    // (open <bar, count> small_) a2
      val a8 = small(a7)    // (open <bar, count> small_) a2
      val a9 = small(a8)    // (open <bar, count> small_) a2
      small(a9)    // (open <bar, count> small_) a2
      ()
    }
}

fun main(){
    // val args : list<string> = get-args()
    // val n = args.head.maybe("100000").parse-int.unjust
    f(1000000)
    ()
}
