fun repeatN(i, f) {
    map(list(1, i), fn(_) { f() })
}

fun large(f) : total _ {
    // list(1, 10).map(f).sum
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
    // does not specialize
    calls-large() fn(x) {
        x + 1
    }
}