public module test_tensor

import tensor

//-------------------------------------------------------------------------------------------------
// unit test-tensors
//-------------------------------------------------------------------------------------------------


fun test-tensor-add1() {
    val x = tensor([[1.0]])
    val y = tensor([[2.0]])
    val t = x + y
    println(t)
}

fun test-tensor-add2() {
    val x = tensor([[1.0, 3.0]])
    val y = tensor([[2.0, 4.0]])
    val t = x + y
    println(t)
}

fun test-tensor-add3() {
    val x = tensor([[1.0, 3.0], [5.0, 6.0]] )
    val y = tensor([[2.0, 4.0], [7.0, 8.0]])
    val t = x + y
    println(t)
}

fun test-tensor-add4() {
    val x = tensor([[1.0, 3.0, 9.0], [5.0, 6.0, 10.0], [11.0, 12.0, 13.0]])
    val y = tensor([[2.0, 4.0, 14.0], [7.0, 8.0, 15.0], [16.0, 17.0, 18.0]])
    val t = x + y
    println(t)
}

fun test-tensor-sub1() {
    val x = tensor([[1.0]])
    val y = tensor([[2.0]])
    val t = x - y
    println(t)
}

fun test-tensor-sub2() {
    val x = tensor([[1.0, 3.0]])
    val y = tensor([[2.0, 4.0]])
    val t = x - y
    println(t)
}

fun test-tensor-sub3() {
    val x = tensor([[1.0, 3.0], [5.0, 6.0]] )
    val y = tensor([[2.0, 4.0], [7.0, 8.0]])
    val t = x - y
    println(t)
}

fun test-tensor-sub4() {
    val x = tensor([[1.0, 3.0, 9.0], [5.0, 6.0, 10.0], [11.0, 12.0, 13.0]])
    val y = tensor([[2.0, 4.0, 14.0], [7.0, 8.0, 15.0], [16.0, 17.0, 18.0]])
    val t = x - y
    println(t)
}

fun test-tensor-mul1() {
    val x = tensor([[1.0]])
    val y = tensor([[2.0]])
    val t = x * y
    println(t)
}

fun test-tensor-mul2() {
    val x = tensor([[1.0, 3.0]])
    val y = tensor([[2.0, 4.0]])
    val t = x * y
    println(t)
}

fun test-tensor-mul3() {
    val x = tensor([[1.0, 3.0], [5.0, 6.0]] )
    val y = tensor([[2.0, 4.0], [7.0, 8.0]])
    val t = x * y
    println(t)
}

fun test-tensor-mul4() {
    val x = tensor([[1.0, 3.0, 9.0], [5.0, 6.0, 10.0], [11.0, 12.0, 13.0]])
    val y = tensor([[2.0, 4.0, 14.0], [7.0, 8.0, 15.0], [16.0, 17.0, 18.0]])
    val t = x * y
    println(t)
}

fun test-tensor-div1() {
    val x = tensor([[1.0]])
    val y = tensor([[2.0]])
    val t = x / y
    println(t)
}

fun test-tensor-div2() {
    val x = tensor([[1.0, 3.0]])
    val y = tensor([[2.0, 4.0]])
    val t = x / y
    println(t)
}

fun test-tensor-div3() {
    val x = tensor([[1.0, 3.0], [5.0, 6.0]] )
    val y = tensor([[2.0, 4.0], [7.0, 8.0]])
    val t = x / y
    println(t)
}

fun test-tensor-div4() {
    val x = tensor([[1.0, 3.0, 9.0], [5.0, 6.0, 10.0], [11.0, 12.0, 13.0]])
    val y = tensor([[2.0, 4.0, 14.0], [7.0, 8.0, 15.0], [16.0, 17.0, 18.0]])
    val t = x / y
    println(t)
}

fun test-tensor-exp1() {
    val x = tensor([1.0])
    val t = expt(x)
    println(x)
    println(t)
}

fun test-tensor-exp2() {
    val x = tensor([[1.0, 3.0]])
    val t = expt(x)
    println(x)
    println(t)
}

fun test-tensor-exp3() {
    val x = tensor([[1.0, 3.0], [5.0, 6.0]] )
    val t = expt(x)
    println(x)
    println(t)
}

fun test-tensor-exp4() {
    val x = tensor([[1.0, 3.0, 9.0], [5.0, 6.0, 10.0], [11.0, 12.0, 13.0]])
    val t = expt(x)
    println(x)
    println(t)
}

fun test-tensor-log1() {
    val x = tensor([1.0])
    val t = logt(x)
    println(x)
    println(t)
}

fun test-tensor-log2() {
    val x = tensor([[1.0, 3.0]])
    val t = logt(x)
    println(x)
    println(t)
}

fun test-tensor-log3() {
    val x = tensor([[1.0, 3.0], [5.0, 6.0]] )
    val t = logt(x)
    println(x)
    println(t)
}

fun test-tensor-log4() {
    val x = tensor([[1.0, 3.0, 9.0], [5.0, 6.0, 10.0], [11.0, 12.0, 13.0]])
    val t = logt(x)
    println(x)
    println(t)
}


fun test-tensor-zeros_like1() {
    val x = tensor([[1.0]])
    val t = zeros_like(x)
    println(t)
}

fun test-tensor-zeros_like2() {
    val x = tensor([[1.0, 3.0]])
    val t = zeros_like(x)
    println(t)
}

fun test-tensor-zeros_like3() {
    val x = tensor([[1.0, 3.0], [5.0, 6.0]] )
    val t = zeros_like(x)
    println(t)
}

fun test-tensor-zeros_like4() {
    val x = tensor([[1.0, 3.0, 9.0], [5.0, 6.0, 10.0], [11.0, 12.0, 13.0]])
    val t = zeros_like(x)
    println(t)
}

fun test-tensor-ones_like1() {
    val x = tensor([[1.0]])
    val t = ones_like(x)
    println(t)
}

fun test-tensor-ones_like2() {
    val x = tensor([[1.0, 3.0]])
    val t = ones_like(x)
    println(t)
}

fun test-tensor-ones_like3() {
    val x = tensor([[1.0, 3.0], [5.0, 6.0]] )
    val t = ones_like(x)
    println(t)
}

fun test-tensor-ones_like4() {
    val x = tensor([[1.0, 3.0, 9.0], [5.0, 6.0, 10.0], [11.0, 12.0, 13.0]])
    val t = ones_like(x)
    println(t)
}

fun test-tensor-sum1() {
    val x = tensor([[1.0]])
    val t = sumt(x)
    println(t)
}

fun test-tensor-sum2() {
    val x = tensor([[1.0, 3.0]])
    val t = sumt(x)
    println(t)
}

fun test-tensor-sum3() {
    val x = tensor([[1.0, 3.0], [5.0, 6.0]])
    val t = sumt(x)
    println(t)
}

fun test-tensor-sum4() {
    val x = tensor([[1.0, 3.0, 9.0], [5.0, 6.0, 10.0], [11.0, 12.0, 13.0]])
    val t = sumt(x)
    println(t)
}

fun test-tensor-matmul1() {
    val x = tensor([[1.0]])
    val y = tensor([[2.0]])
    val t = matmult(x, y)
    println(t)
}

fun test-tensor-matmul2() {
    val x = tensor([[1.0, 3.0]])
    val y = tensor([[1.0], [3.0]])
    val t = matmult(x, y)
    println(t)
}

fun test-tensor-matmul3() {
    val x = tensor([[1.0, 3.0], [5.0, 6.0]])
    val y = tensor([[1.0, 3.0], [5.0, 6.0]])
    val t = matmult(x, y)
    println(t)
}

fun test-tensor-matmul4() {
    val x = tensor([[1.0, 3.0, 9.0], [5.0, 6.0, 10.0], [11.0, 12.0, 13.0]])
    val y = tensor([[1.0], [5.0], [11.0]])
    val t = matmult(x, y)
    println(t)
}

fun test-tensor-broadcast_to1() {
    val x = tensor((1, 1), 0.0)
    val t = x.broadcast_tot((2, 3))
    println(t)
}

fun test-tensor-broadcast_to2() {
    val x = tensor((1, 3), 0.0)
    val t = x.broadcast_tot((5, 3))
    println(t)
}

fun test-tensor-broadcast_to3() {
    val x = tensor((3, 1), 0.0)
    val t = x.broadcast_tot((3, 5))
    println(t)
}

fun test-tensor-broadcast_to4() {
    val x = tensor((6, 7), 0.0)
    val t = x.broadcast_tot((6, 7))
    println(t)
}

fun test-tensor-sum_to1() {
    val x = tensor((1, 1), 1.0)
    val t = x.sum_tot((1, 1))
    println(t)
}

fun test-tensor-sum_to2() {
    val x = tensor((3, 3), 1.0)
    val t = x.sum_tot((1, 1))
    println(t)
}

fun test-tensor-sum_to3() {
    val x = tensor((2, 4), 1.0)
    val t = x.sum_tot((1, 4))
    println(t)
}

fun test-tensor-sum_to4() {
    val x = tensor((3, 3), 1.0)
    val t = x.sum_tot((3, 1))
    println(t)
}

fun test-tensor-linear1() {
    val w = tensor([[1.0]])
    val x = tensor([[2.0]])
    val b = tensor([[3.0]])
    val t = lineart(x, w, b)
    println(t)
}

fun test-tensor-linear2() {
    val w = tensor([[1.0], [3.0]])
    val x = tensor([[1.0, 3.0]])
    val b = tensor([[2.0]])
    val t = lineart(x, w, b)
    println(t)
}

fun test-tensor-linear3() {
    val w = tensor([[1.0, 3.0], [5.0, 6.0]])
    val x = tensor([[1.0, 3.0], [5.0, 6.0]])
    val b = tensor([[1.0, 3.0]])
    val t = lineart(x, w, b)
    println(t)
}

fun test-tensor-linear4() {
    val w = tensor([[1.0, 3.0, 9.0], [5.0, 6.0, 10.0], [11.0, 12.0, 13.0]])
    val x = tensor([[1.0, 5.0, 11.0]])
    val b = tensor([[1.0, 1.0, 1.0]])
    val t = lineart(x, w, b)
    println(t)
}

fun test-tensor-all() {
    println("add1"); test-tensor-add1()
    println("add2"); test-tensor-add2()
    println("add3"); test-tensor-add3()
    println("add4"); test-tensor-add4()

    println("sub1"); test-tensor-sub1()
    println("sub2"); test-tensor-sub2()
    println("sub3"); test-tensor-sub3()
    println("sub4"); test-tensor-sub4()

    println("mul1"); test-tensor-mul1()
    println("mul2"); test-tensor-mul2()
    println("mul3"); test-tensor-mul3()
    println("mul4"); test-tensor-mul4()

    println("div1"); test-tensor-div1()
    println("div2"); test-tensor-div2()
    println("div3"); test-tensor-div3()
    println("div4"); test-tensor-div4()

    println("exp1"); test-tensor-exp1()
    println("exp2"); test-tensor-exp2()
    println("exp3"); test-tensor-exp3()
    println("exp4"); test-tensor-exp4()

    println("log1"); test-tensor-log1()
    println("log2"); test-tensor-log2()
    println("log3"); test-tensor-log3()
    println("log4"); test-tensor-log4()

    println("zeros_like1"); test-tensor-zeros_like1()
    println("zeros_like2"); test-tensor-zeros_like2()
    println("zeros_like3"); test-tensor-zeros_like3()
    println("zeros_like4"); test-tensor-zeros_like4()

    println("ones_like1"); test-tensor-ones_like1()
    println("ones_like2"); test-tensor-ones_like2()
    println("ones_like3"); test-tensor-ones_like3()
    println("ones_like4"); test-tensor-ones_like4()

    println("sum1"); test-tensor-sum1()
    println("sum2"); test-tensor-sum2()
    println("sum3"); test-tensor-sum3()
    println("sum4"); test-tensor-sum4()

    println("matmul1"); test-tensor-matmul1()
    println("matmul2"); test-tensor-matmul2()
    println("matmul3"); test-tensor-matmul3()
    println("matmul4"); test-tensor-matmul4()

    println("broadcast_to1"); test-tensor-broadcast_to1()
    println("broadcast_to2"); test-tensor-broadcast_to2()
    println("broadcast_to3"); test-tensor-broadcast_to3()
    println("broadcast_to4"); test-tensor-broadcast_to4()

    println("sum_to1"); test-tensor-sum_to1()
    println("sum_to2"); test-tensor-sum_to2()
    println("sum_to3"); test-tensor-sum_to3()
    println("sum_to4"); test-tensor-sum_to4()

    println("linear1"); test-tensor-linear1()
    println("linear2"); test-tensor-linear2()
    println("linear3"); test-tensor-linear3()
    println("linear4"); test-tensor-linear4()
}