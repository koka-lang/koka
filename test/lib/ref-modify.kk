fun main() {
    val r = ref(0)
    r.modify fn(l) {
        l := 1
    }
    (!r).println
}