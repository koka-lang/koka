// bench mark to measure the effect of open floating

effect bra {
    fun brara() : ()
}

fun k() : <bra> () { brara() }

effect count {
    fun one( a : int ) : int 
    fun two( a : int ) : int 
}

fun one_( a : int ) {
    (if a.is-even then a + 1 else a).one
    
}

fun two_( a : int ) {
    (if a.is-even then a + 2 else a).two
}

// fun fib( n : int ) {
//     if n < 2 then n else fib(n-1) + fib
// } 
fun f() {
    var i : int := 0
    var c : int := 0
    var b := 0
    with fun brara() { b := i + c }
    with handler {
        fun one( a ) { i := a + 1; c := c + 1; i }
        fun two( a ) { i := a + 2; c := c + 1; i }
        return(_) { c } 
    }
    while { i < 10000000 * 4 } 
    {
      k()
      val a0 = i
      val a1 = one_(a0)
      val a2 = two_(a1)
      one_(a2)
      ()
    }
}

fun main(){
    f()
}