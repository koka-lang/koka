module testsuite

import std/num/int32
import std/time/timer

import binomial-heaps-okasaki
import leftist-heaps-okasaki
import pairing-heaps-okasaki
import pairing-heaps-tailcall
import splay-heaps-fbip
import splay-heaps-okasaki
import weight-biased-leftist-heaps-okasaki
import haskell-sort

struct test-config {
    testsize : int
    max-rand-number : int32
}

fun print-test-config(tc) {
    println("\nHeapsorting " ++ show(tc.testsize) ++ " random integers in the range [0, " ++ show(tc.max-rand-number) ++ "):")
}

// Wichmann-Hill random number generator
struct rand-state {
  s1 : int
  s2 : int
  s3 : int
}

fun rand(s : rand-state) {
    val s1 = (171 * s.s1) % 30269
    val s2 = (172 * s.s2) % 30307
    val s3 = (170 * s.s3) % 30323
    val r = (float64(s1)/30269.0 + float64(s2)/30307.0 + float64(s3)/30323.0) % 1.0
    (r, Rand-state(s1, s2, s3))
}

fun rand-list(tc : test-config) {
    fun go(st, n, l) {
        if(n <= 0) then l
        else {
            val (r, st') = rand(st)
            go(st', n - 1, Cons((tc.max-rand-number.float64 * r).int, l))
        }
    }

    val init = Rand-state(19379, 17844, 5912)
    go(init, tc.testsize, [])
}

fun eq(xs1 : list<int>, xs2 : list<int>) {
    match(xs1, xs2) {
        (Nil, Nil) -> True
        (Cons(x1, xx1), Cons(x2, xx2)) -> x1 == x2 && eq(xx1, xx2)
        _ -> False
    }
}

fun main() {
    // Test implementations:

    print("Testing consistency: ")
    val test = rand-list(Test-config(100, 10000.int32))
    val r1 = heapsort-leftist-heaps-okasaki(test)
    val r2 = heapsort-pairing-heaps-okasaki(test)
    val r3 = heapsort-pairing-heaps-tailcall(test)
    val r4 = heapsort-splay-heaps-fbip(test)
    val r5 = heapsort-splay-heaps-okasaki(test)
    val r6 = heapsort-weight-biased-leftist-heaps-okasaki(test)
    val r7 = merge-sort(test)
    // val r8 = heapsort-binomial-heaps-okasaki(input)

    print(eq(r1, r2))
    print(eq(r2, r3))
    print(eq(r3, r4))
    print(eq(r4, r5))
    print(eq(r5, r6))
    print(eq(r6, r7))
    println(eq(r7, r1))

    // Benchmark:

    val tcs = [ Test-config( 1000000, 100000000.int32)
              , Test-config( 5000000, 500000000.int32)
              , Test-config(10000000, 1000000000.int32)]

    tcs.map(fn(tc) {
        print-test-config(tc)
        val input = rand-list(tc)

        // print-elapsed(fn() { heapsort-binomial-heaps-okasaki(input)}, "Binomial Heaps: ")
        print-elapsed(fn() { merge-sort(input)}, "Merge sort: ")
        val r = print-elapsed(fn() { heapsort-weight-biased-leftist-heaps-okasaki(input)}, "Weight Biased Leftist Heaps: ")
        print-elapsed(fn() { heapsort-splay-heaps-fbip(input)}, "Splay Heaps (FBIP): ")
        print-elapsed(fn() { heapsort-splay-heaps-okasaki(input)}, "Splay Heaps: ")
        print-elapsed(fn() { heapsort-leftist-heaps-okasaki(input)}, "Leftist Heaps: ")
        print-elapsed(fn() { heapsort-pairing-heaps-okasaki(input)}, "Pairing Heaps: ")
        print-elapsed(fn() { heapsort-pairing-heaps-tailcall(input)}, "Pairing Heaps (tailcall): ")

        println("... and sorting the sorted list again:")
        print-elapsed(fn() { merge-sort(r)}, "Merge sort: ")
        // print-elapsed(fn() { heapsort-binomial-heaps-okasaki(r)}, "Binomial Heaps: ")
        print-elapsed(fn() { heapsort-weight-biased-leftist-heaps-okasaki(r)}, "Weight Biased Leftist Heaps: ")
        print-elapsed(fn() { heapsort-splay-heaps-fbip(r)}, "Splay Heaps (FBIP): ")
        print-elapsed(fn() { heapsort-splay-heaps-okasaki(r)}, "Splay Heaps: ")
        print-elapsed(fn() { heapsort-leftist-heaps-okasaki(r)}, "Leftist Heaps: ")
        print-elapsed(fn() { heapsort-pairing-heaps-okasaki(r)}, "Pairing Heaps: ")
        print-elapsed(fn() { heapsort-pairing-heaps-tailcall(r)}, "Pairing Heaps (tailcall): ")

        ()
    })
    ()
}