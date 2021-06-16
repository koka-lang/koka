module benchmark

import std/time
import std/num/ddouble

// This module is designed after the popular criterion library for Haskell.
// Unlike criterion, environments are managed by effects. This is important for two reasons:
//   1. We can use laziness to postpone the creation of the environment, so that we can
//      print the names of the benchmarks without creating the environment.
//      (similar to criterion, but more under-the-hood here)
//   2. We can hold on to the environment until after the last benchmark, so that
//      the last benchmark won't free the environment (since Koka uses reference-
//      counting and not garbage-collection like Haskell).

type benchmark<e> {
    Bench(pre : () -> e (), f : () -> e (), post : () -> e ())
    BGroup(name : string, bs : list<benchmark<e>>)
}

// For readability
fun no-pre() {}
fun no-post() {}

effect ask<a> {
    fun ask() : a
}

fun re-wrap(d : delayed<e,a>, b : benchmark<<ask<a>|e>>) : benchmark<e> {
    match(b) {
        BGroup(name, bs) {
            BGroup(name, bs.map(fn(b1) { re-wrap(d, unsafe-decreasing(b1)) }))
        }
        Bench(pre, f, post) {
            Bench(fn() {
                with fun ask() { force(d) }
                pre()
            }, fn() {
                with fun ask() { force(d) }
                f()
            }, fn() {
                with fun ask() { force(d) }
                post()
            })
        }
    }
}

// Set an environment that will only be evaluated once
// the first time it is requested.
fun set-environment(env : () -> e1 a, action : () -> e benchmark<<ask<a>|e1>>) : e benchmark<e1> {
    val d = delay fn() { env() }
    val b = action()
    re-wrap(d, b)
}

fun set-environment(env : () -> e1 a, action : () -> e list<benchmark<<ask<a>|e1>>>) : e list<benchmark<e1>> {
    val d = delay fn() { env() }
    val bs = action()
    bs.map(fn(b) { re-wrap(d, b) })
}

// Evaluate the environment
fun eval-env() : ask<a> () { val e = ask(); () }

fun list-benchmarks(b : benchmark<e>, name : string = "") : total list<string> {
    match(b) {
        BGroup(s, bs) {
            val ls = bs.map(fn(b1) { list-benchmarks(unsafe-decreasing(b1), name ++ "/" ++ s) }).concat
            Cons(name ++ "/" ++ s, ls)
        }
        Bench(_,_,_) { Nil }
    }
}

fun run-benchmarks(b : benchmark<<ndet|e>>, name : string = "") : <ndet|e> list<(string, duration)> {
    match(b) {
        BGroup(s, bs) {
            bs.map(fn(b1) { run-benchmarks(b1, name ++ "/" ++ s) }).concat
        }
        Bench(pre,f,post) {
            pre()
            val (d, _) = elapsed(fn() {
                f()
            })
            post()
            [(name, d)]
        }
    }
}

val test = BGroup("Sets", set-environment(fn() { println("Eval"); 2 }, fn() {
    [ BGroup("Insert", [Bench(eval-env, fn() { println("Bench"); val e : int = ask(); println(e) }, no-post)])
    , BGroup("Lookup", [Bench(eval-env, fn() { println("Bench"); val e : int = ask(); println(e) }, no-post)])
    ]}))

fun main() {
    test.list-benchmarks.unlines.println
    test.run-benchmarks.map(fn((s,d)) { s ++ ": " ++ d.show }).unlines.println
}

fun random-list(n) : random list<int32> {
    if(n <= 0.int32) {
        Nil
    } else {
        Cons(random-int32(), random-list(n-1))
    }
}