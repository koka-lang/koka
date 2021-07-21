effect foo {
    fun boo(x:int):int
}

fun f(): int {
    4 * 3
}

fun g(): foo int {
    boo(3) + (f() + f())
}

fun main(){
    with fun boo(x){x}
    g()
}