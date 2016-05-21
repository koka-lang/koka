
external inline foo( x : int ) : int {
  inline "#1"
}

function main() {
  apply(foo,42)
}
