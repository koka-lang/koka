type eq<a,b> {
  con Eq( witness : forall<f> f<a> -> f<b> )
}

function idp( x : c<a> ) : c<a> { x }
function foo( x : eq<int,int> ) { 42 }

function main() -> foo(Eq(id))
