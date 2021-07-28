effect foo {
    fun boo(x:int):int
}

fun test1(){
    with fun boo(x){x}
    boo(1)
}

fun f(): int {
    4 * 3
}

// Observe floating
// { boo(3) + open[...](f)(3) + open[...](f)(3) } ~~> { boo(3) + restrict[...](f() + f())}
fun g(): foo int {
    boo(3) + (f() + f())
}


fun main(){
    test1()
    1
}