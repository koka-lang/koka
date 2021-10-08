module testsuite

import std/num/int32
import std/time/timer

import b-trees
import b-trees-pad-full
import b-trees-pad-separate
import b-trees-pad-separate-fast-path
import rbtree-fbip
import rbtree-okasaki
import skip-list-vector
// import skip-list-imperative
import splay-trees-fbip
import weight-balanced-data-set
import weight-balanced-fbip
import weight-balanced-fbip-int
import weight-balanced-hirai
import patricia-tree-big-endian-okasaki
import patricia-tree-data-intset
import patricia-tree-data-intset-clz
import patricia-tree-little-endian-okasaki

struct test-config {
    testsize : int
    max-rand-number : int32
}

fun print-test-config(tc) {
    println("\nCounting unique integers among " ++ show(tc.testsize) ++ " random integers in the range [0, " ++ show(tc.max-rand-number) ++ "):")
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
    val r = (double(s1)/30269.0 + double(s2)/30307.0 + double(s3)/30323.0) % 1.0
    (r, Rand-state(s1, s2, s3))
}

fun rand-list(tc : test-config) {
    fun go(st, n, l) {
        if(n <= 0) then l
        else {
            val (r, st') = rand(st)
            go(st', n - 1, Cons((tc.max-rand-number.double * r).int, l))
        }
    }

    val init = Rand-state(19379, 17844, 5912)
    go(init, tc.testsize, [])
}

fun main() {
    // Test implementations:

    print("Testing consistency: ")
    val test = rand-list(Test-config(100, 1000.int32))
    val r1  = count-b-trees(1, 1000, test)
    val r2  = count-b-trees-pad-full(1, 1000, test)
    val r3  = count-b-trees-pad-separate(1, 1000, test)
    val r4  = count-rbtree-fbip(1, 1000, test)
    val r5  = count-rbtree-okasaki(1, 1000, test)
    val r6  = count-skip-list-vector(1, 1000, test)
    val r7  = r1 // count-skip-list-imperative(1, 1000, test)
    val r8  = count-splay-trees-fbip(1, 1000, test)
    val r9  = count-weight-balanced-data-set(1, 1000, test)
    val r10 = count-weight-balanced-fbip(1, 1000, test)
    val r11 = count-weight-balanced-fbip-int(1, 1000, test)
    val r12 = count-weight-balanced-hirai(1, 1000, test)
    val r13 = count-patricia-tree-big-endian-okasaki(1, 1000, test)
    val r14 = count-patricia-tree-data-intset(1, 1000, test)
    val r15 = count-patricia-tree-data-intset-clz(1, 1000, test)
    val r16 = count-patricia-tree-little-endian-okasaki(1, 1000, test)

    print(r1 == r2)
    print(r2 == r3)
    print(r3 == r4)
    print(r4 == r5)
    print(r5 == r6)
    print(r6 == r7)
    print(r7 == r8)
    print(r8 == r9)
    print(r9 == r10)
    print(r10 == r11)
    print(r11 == r12)
    print(r12 == r13)
    print(r13 == r14)
    print(r14 == r15)
    print(r15 == r16)
    println(r16 == r1)

    // Benchmark:

    val tcs = [ Test-config( 1000000, 10000000.int32)
              , Test-config( 5000000, 50000000.int32)
              , Test-config(10000000, 100000000.int32)]

    tcs.map(fn(tc) {
        print-test-config(tc)
        val input = rand-list(tc)
        val start = 1
        val end = tc.testsize

        print-elapsed(fn() { count-b-trees-pad-separate-fast-path(start, end, input) }, "B Tree (t = 2, pad:separate fast-path):")
        print-elapsed(fn() { count-b-trees-pad-separate(start, end, input) }, "B Tree (t = 2, pad:separate):")
        print-elapsed(fn() { count-b-trees(start, end, input) }, "B Tree (t = 2):")
        print-elapsed(fn() { count-patricia-tree-data-intset-clz(start, end, input) }, "Patricia Tree (Data.IntSet CLZ):")
        print-elapsed(fn() { count-patricia-tree-data-intset(start, end, input) }, "Patricia Tree (Data.IntSet):")
        print-elapsed(fn() { count-b-trees-pad-full(start, end, input) }, "B Tree (t = 2, pad:full):")
        print-elapsed(fn() { count-weight-balanced-fbip-int(start, end, input) }, "WBT (FBIP Int):")
        print-elapsed(fn() { count-patricia-tree-big-endian-okasaki(start, end, input) }, "Patricia Tree (big endian):")
        print-elapsed(fn() { count-rbtree-okasaki(start, end, input) }, "Red Black Tree (Okasaki):")
        print-elapsed(fn() { count-rbtree-fbip(start, end, input) }, "Red Black Tree (FBIP):")
        print-elapsed(fn() { count-weight-balanced-fbip(start, end, input) }, "WBT (FBIP):")
        print-elapsed(fn() { count-weight-balanced-hirai(start, end, input) }, "WBT (Hirai):")
        print-elapsed(fn() { count-splay-trees-fbip(start, end, input) }, "Splay Trees (FBIP):")
        print-elapsed(fn() { count-patricia-tree-little-endian-okasaki(start, end, input) }, "Patricia Tree (little endian):")
        print-elapsed(fn() { count-weight-balanced-data-set(start, end, input) }, "WBT (Data.Set):")
        // print-elapsed(fn() { count-skip-list-vector(start, end, input) }, "Skip list (vector):")
        // print-elapsed(fn() { count-skip-list-imperative(start, end, input) }, "Skip list (imperative):")

        ()
    })
    ()
}