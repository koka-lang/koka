fun repeatN(i, f : () -> _ _) {
    // map(list(1, i), fn(_) { f() })
    list(1, i, fn(_) f() )
}

noinline fun a(i, f) {
    repeatN(i, f)
}

fun large(f) : total _ {
    repeatN(10) {
        repeatN(10) {
            repeatN(10) {
                list(1, 10).map(f).sum
            }
        }
    }.concat.concat.sum
}

fun calls-large(f) {
    large(f).println
    large(f).println
}

fun main() {
    // large(inc)
    calls-large(inc)
}
