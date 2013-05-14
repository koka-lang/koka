// inductive types cannot have a negative recursive occurrence
type recurse<a> {
  con In( out : recurse<a> -> a )
}
