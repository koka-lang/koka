rec type recurse<a> {
  con In( out : recurse<a> -> a )
}
