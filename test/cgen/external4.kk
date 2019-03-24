
external inline foo( x : int ) : exn int {
  inline "#1"
}

fun main() {
  zipWith([12],[30],(+)).head
}
