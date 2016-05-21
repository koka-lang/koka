
external inline foo( x : int ) : exn int {
  inline "#1"
}

function main() {
  zipWith([12],[30],(+)).head
}
