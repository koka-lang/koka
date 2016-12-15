function bar( f : forall<c,a> ( x:c<a>,y:a) -> list<a> ) {
  f([2],1)
}

function foo( x:a,y:b ) : list<b> {
  [y]
}

function main() -> bar(foo)
