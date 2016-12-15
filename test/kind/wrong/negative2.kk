// inductive types cannot have a negative recursive occurrence
type foo<a> {
  con In( out : bar<a> -> a )
}

type bar<a> {
  con Mkfoo( :foo<a> )
  con Mknil
}
