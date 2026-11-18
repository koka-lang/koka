fun main() {
  val r = ref(0)
  r.access( fn(l) l := 1 )
  (!r).println
}
