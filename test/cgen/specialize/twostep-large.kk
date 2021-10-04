fun large(f) : total _ {
    list(1, 10).map(f).sum
    // this version does not specialize; need to check if list is marked as specializable
    // list(1, 10, f).sum
}

fun calls-large(f) {
    large(f).println
    large(f).println
}

fun main() {
    calls-large() fn(x) {
        x + 1
    }
}