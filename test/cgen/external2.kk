
external inline foo( x : int ) : exn int {
  inline "#1"
}

function main() {
  foo(42)
  print("done")
}
