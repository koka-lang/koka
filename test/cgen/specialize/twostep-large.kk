fun large(f : int -> _ _) : total _ {
    list(1, 10, f).sum
}

fun calls-large(f) {
    large(f).println
    large(f).println
}

fun main() {
    calls-large() fn(x) {
        x + 2
    }
}