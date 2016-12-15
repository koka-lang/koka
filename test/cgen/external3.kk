
external inline foo( x : int ) : exn int {
  inline "#1"
}

function main() {
  [42].map(foo).head
}
