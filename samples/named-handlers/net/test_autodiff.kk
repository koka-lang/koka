public module test_autodiff

import variable
import autodiff
import tensor
// import layers


//-------------------------------------------------------------------------------------------------
// unit test-autodiff
//-------------------------------------------------------------------------------------------------


fun test-autodiff-addE1() {
    val x = variable([[1.0]])
    val y = variable([[2.0]])
    val t = backprop { x + y }
    println(t)
    println(x)
    println(y)
}

fun test-autodiff-addE2() {
    val x = variable([[1.0, 2.0], [3.0, 4.0]])
    val y = variable([[2.0, 3.0], [4.0, 5.0]])
    val t = backprop { x + y }
    println(t)
    println(x)
    println(y)
}

fun test-autodiff-addE3() {
    val x = variable([[1.0, 2.0], [3.0, 4.0]])
    val y = variable([[2.0, 3.0]])
    val t = backprop { x + y }
    println(t)
    println(x)
    println(y)
}

fun test-autodiff-addE4() {
    val x = variable([[1.0, 2.0]])
    val y = variable([[2.0], [3.0]])
    val t = backprop { x + y }
    println(t)
    println(x)
    println(y)
}

fun test-autodiff-addE5() {
    val x = variable([[1.0, 2.0], [3.0, 4.0]])
    val y = variable([[2.0]])
    val t = backprop { x + y }
    println(t)
    println(x)
    println(y)
}

fun test-autodiff-subE1() {
    val x = variable([[1.0]])
    val y = variable([[2.0]])
    val t = backprop { x - y }
    println(t)
    println(x)
    println(y)
}

fun test-autodiff-subE2() {
    val x = variable([[1.0, 2.0], [3.0, 4.0]])
    val y = variable([[2.0, 3.0], [4.0, 5.0]])
    val t = backprop { x - y }
    println(t)
    println(x)
    println(y)
}

fun test-autodiff-subE3() {
    val x = variable([[1.0, 2.0], [3.0, 4.0]])
    val y = variable([[2.0, 3.0]])
    val t = backprop { x - y }
    println(t)
    println(x)
    println(y)
}

fun test-autodiff-subE4() {
    val x = variable([[1.0, 2.0]])
    val y = variable([[2.0], [3.0]])
    val t = backprop { x - y }
    println(t)
    println(x)
    println(y)
}

fun test-autodiff-subE5() {
    val x = variable([[1.0, 2.0], [3.0, 4.0]])
    val y = variable([[2.0]])
    val t = backprop { x - y }
    println(t)
    println(x)
    println(y)
}

fun test-autodiff-mulE1() {
    val x = variable([[1.0]])
    val y = variable([[2.0]])
    val t = backprop { x * y }
    println(t)
    println(x)
    println(y)
}

fun test-autodiff-mulE2() {
    val x = variable([[1.0, 2.0], [3.0, 4.0]])
    val y = variable([[2.0, 3.0], [4.0, 5.0]])
    val t = backprop { x * y }
    println(t)
    println(x)
    println(y)
}

fun test-autodiff-mulE3() {
    val x = variable([[1.0, 2.0], [3.0, 4.0]])
    val y = variable([[2.0, 3.0]])
    val t = backprop { x * y }
    println(t)
    println(x)
    println(y)
}

fun test-autodiff-mulE4() {
    val x = variable([[1.0, 2.0]])
    val y = variable([[2.0], [3.0]])
    val t = backprop { x * y }
    println(t)
    println(x)
    println(y)
}

fun test-autodiff-mulE5() {
    val x = variable([[1.0, 2.0], [3.0, 4.0]])
    val y = variable([[2.0]])
    val t = backprop { x * y }
    println(t)
    println(x)
    println(y)
}

fun test-autodiff-divE1() {
    val x = variable([[1.0]])
    val y = variable([[2.0]])
    val t = backprop { x / y }
    println(t)
    println(x)
    println(y)
}

fun test-autodiff-divE2() {
    val x = variable([[1.0, 2.0], [3.0, 4.0]])
    val y = variable([[2.0, 3.0], [4.0, 5.0]])
    val t = backprop { x / y }
    println(t)
    println(x)
    println(y)
}

fun test-autodiff-divE3() {
    val x = variable([[1.0, 2.0], [3.0, 4.0]])
    val y = variable([[2.0, 3.0]])
    val t = backprop { x / y }
    println(t)
    println(x)
    println(y)
}

fun test-autodiff-divE4() {
    val x = variable([[1.0, 2.0]])
    val y = variable([[2.0], [3.0]])
    val t = backprop { x / y }
    println(t)
    println(x)
    println(y)
}

fun test-autodiff-divE5() {
    val x = variable([[1.0, 2.0], [3.0, 4.0]])
    val y = variable([[2.0]])
    val t = backprop { x / y }
    println(t)
    println(x)
    println(y)
}

fun test-autodiff-expE1() {
    val x = variable([[1.0]])
    val t = backprop { exp(x) }
    println(x)
    println(t)
}

fun test-autodiff-expE2() {
    val x = variable([[1.0, 2.0], [3.0, 4.0]])
    val t = backprop { exp(x) }
    println(x)
    println(t)
}

fun test-autodiff-expE3() {
    val x = variable([[1.0, 2.0, 3.0, 4.0]])
    val t = backprop { exp(x) }
    println(x)
    println(t)
}

fun test-autodiff-logE1() {
    val x = variable([[1.0]])
    val t = backprop { log(x) }
    println(x)
    println(t)
}

fun test-autodiff-logE2() {
    val x = variable([[1.0, 2.0], [3.0, 4.0]])
    val t = backprop { log(x) }
    println(x)
    println(t)
}

fun test-autodiff-logE3() {
    val x = variable([[1.0, 2.0, 3.0, 4.0]])
    val t = backprop { log(x) }
    println(x)
    println(t)
}


fun test-autodiff-matmulE1() {
    val x = variable([[1.0]])
    val w = variable([[2.0]])
    val t = backprop { matmul(x, w) }
    println(t)
    println(x)
    println(w)
}

fun test-autodiff-matmulE2() {
    val x = variable([[1.0, 2.0]])
    val w = variable([[2.0, 3.0], [4.0, 5.0]])
    val t = backprop { matmul(x, w) }
    println(t)
    println(x)
    println(w)
}

fun test-autodiff-sumE1() {
    val x = variable([[1.0]])
    val t = backprop { sum(x) }
    println(t)
    println(x)
}

fun test-autodiff-sumE2() {
    val x =  variable([[2.0, 3.0], [4.0, 5.0]])
    val t = backprop { sum(x) }
    println(t)
    println(x)
}


fun test-autodiff-broadcast_toE1() {
    val x = variable([[1.0]])
    val t = backprop { broadcast_to(x, (1, 1)) }
    println(t)
    println(x)
}

fun test-autodiff-broadcast_toE2() {
    val x =  variable([[2.0], [3.0]])
    val t = backprop { broadcast_to(x, (2, 4)) }
    println(t)
    println(x)
}

fun test-autodiff-broadcast_toE3() {
    val x =  variable([[2.0, 3.0, 6.0]])
    val t = backprop { broadcast_to(x, (2, 3)) }
    println(t)
    println(x)
}

fun test-autodiff-sum_toE1() {
    val x = variable([[1.0, 2.0, 3.0], [4.0, 5.0, 6.0]])
    val t = backprop { sum_to(x, (2, 1)) }
    println(t)
    println(x)
}

fun test-autodiff-sum_toE2() {
    val x =  variable([[2.0], [3.0], [4.0]])
    val t = backprop { sum_to(x, (1, 1)) }
    println(t)
    println(x)
}

fun test-autodiff-sum_toE3() {
    val x =  variable([[2.0, 3.0, 6.0], [2.0, 3.0, 6.0]])
    val t = backprop { sum_to(x, (1, 3)) }
    println(t)
    println(x)
}



fun test-autodiff-all() : <st<h>, console, div, exn | e> () {
    println("add1"); test-autodiff-addE1()
    println("add2"); test-autodiff-addE2()
    println("add3"); test-autodiff-addE3()
    println("add4"); test-autodiff-addE4()
    println("add5"); test-autodiff-addE5()

    println("sub1"); test-autodiff-subE1()
    println("sub2"); test-autodiff-subE2()
    println("sub3"); test-autodiff-subE3()
    println("sub4"); test-autodiff-subE4()
    println("sub5"); test-autodiff-subE5()

    println("mul1"); test-autodiff-mulE1()
    println("mul2"); test-autodiff-mulE2()
    println("mul3"); test-autodiff-mulE3()
    println("mul4"); test-autodiff-mulE4()
    println("mul5"); test-autodiff-mulE5()
    
    println("div1"); test-autodiff-divE1()
    println("div2"); test-autodiff-divE2()
    println("div3"); test-autodiff-divE3()
    println("div4"); test-autodiff-divE4()
    println("div5"); test-autodiff-divE5()
    
    println("exp1"); test-autodiff-expE1()
    println("exp2"); test-autodiff-expE2()
    println("exp3"); test-autodiff-expE3()

    println("log1"); test-autodiff-logE1()
    println("log2"); test-autodiff-logE2()
    println("log3"); test-autodiff-logE3()

    println("matmul1"); test-autodiff-matmulE1()
    println("matmul2"); test-autodiff-matmulE2()

    println("sum1"); test-autodiff-sumE1()
    println("sum2"); test-autodiff-sumE2()

    println("broad_cast1"); test-autodiff-broadcast_toE1()
    println("broad_cast2"); test-autodiff-broadcast_toE2()
    println("broad_cast3"); test-autodiff-broadcast_toE3()

    println("sum_to1"); test-autodiff-sum_toE1()
    println("sum_to2"); test-autodiff-sum_toE2()
    println("sum_to3"); test-autodiff-sum_toE3()

}