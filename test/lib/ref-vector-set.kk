fun main() {
  val v = vector(3, 'a')
  var w := v
  v[1].println
  w[1] := 'b'
  v[1].println
  w[1].println
}
