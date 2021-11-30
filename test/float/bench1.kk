// Bench mark to measure the effect of open floating
// Optimization todo: reduce the number of open call 
// from 4 to 1.

import std/os/env

effect bar {
    fun barra() : ()
}

fun k() : <bar> () { barra() }

effect count {
    fun one( a : int ) : int 
    // fun two( a : int ) : int 
}

fun one_( a : int ): count int {
    (if a.is-even then a + 1 else a).one
    
}

// fun g() : bar () { () }
// fun h() : <bar,count> () { () }
fun f(n) {
    var i : int := 0
    var c : int := 0
    var b := 0
    with fun barra() { b := i + c }
    with handler {
        fun one( a ) { i := a + 1; c := c + 1; i }
        // fun two( a ) { i := a + 2; c := c + 1; i }
        return(_) { c } 
    }
    while { c < n * 4 } 
    {
      k()                  // Use bra effect
      val a0 = i
      // optimization todo: float up opens a restrict
      val a1 = one_(a0)    // (open <bar, count> one_) a0
      val a2 = one_(a1)    // (open <bar, count> one_) a1
      val a3 = one_(a2)    // (open <bar, count> one_) a2
      one_(a3)             // (open <bar, count> one_) a3
      ()
    }
}

fun main(){
    val args : list<string> = get-args()
    val n = args.head.maybe("100000").parse-int.unjust
    f(n)
    ()
}
