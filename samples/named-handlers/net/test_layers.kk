public module test_layers

import std/num/random //これがないとエラー
import tensor
import variable
import autodiff
import layers

fun test-layers-linearE1() {
    val layer = linear(1, 5)
    val x = ones((10, 1)).variable
    val t = backprop { layer.linearE(x) }
    val w = layer.get-weight
    val b = layer.get-bias
    println(t)
    println(w)
    println(b)
}

fun test-layers-linearE2() {
    val layer = linear(3, 5)
    val x = ones((1, 3)).variable
    val t = backprop { layer.linearE(x) }
    val w = layer.get-weight
    val b = layer.get-bias
    println(t)
    println(w)
    println(b)
}

fun test-layers-linearE3() {
    val layer = linear(3, 6)
    val x = ones((10, 3)).variable
    val t = backprop { layer.linearE(x) }
    val w = layer.get-weight
    val b = layer.get-bias
    println(t)
    println(w)
    println(b)
}



fun test-layers-all() {
    // linearE
    println("linearE1"); test-layers-linearE1()
    println("lenearE2"); test-layers-linearE2()
    println("linearE3"); test-layers-linearE3()
}