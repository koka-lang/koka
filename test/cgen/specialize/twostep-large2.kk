fun repeatN(i, f) {
    map(list(1, i), fn(_) { f() })
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
    // does not specialize, since large is not inline here
    // calls-large(inc)
    
    // also does not specialize; large is too big to inline here
    large(inc).println
}
