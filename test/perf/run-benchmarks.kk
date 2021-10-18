module run-benchmarks

import std/os/process
import std/os/env
import std/time

fun main() {
    val args = get-args()
    match(args) {
        Cons(from, Cons(to, Nil)) {
            val couldCheckout = ("git checkout " ++ from ++ "").run-system
            if(couldCheckout != 0) {
                "Failed to checkout the first branch.".println
                "It may be misspelled or you may have uncommitted changes (try 'git stash')".println
            } else {
                // Benchmark from
                ("git checkout " ++ to ++ "").run-system
                // Benchmark to
                ()
            }
        }
        _ {
            "Compare branches for performance regressions.".println
            "Give two branches as arguments (e.g. '-- master dev')".println
            "Then this program will give a summary of performance changes".println
            "from the first (e.g. master) to the second branch (e.g. dev)".println
        }
    }
}