effect input {
  fun getstr() : string
}

fun foo2( fname : string = "hi") : input string {
  val s = bar(fname)
  s.to-upper
}


fun foo1( fnamex : maybe<string>) : input string {
  val fname = match(fnamex) {
    Just(x) -> x
    _ -> "hi"
  }
  val s = bar(fname)
  s.to-upper
}

fun bar( fname : string ) : input string {
  getstr()
}
