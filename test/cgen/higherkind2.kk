type eq<a,b> {
  con Eq( witness : forall<f> f<a> -> f<b> )
}

fun idp( x : c<a> ) : c<a> { x }
fun foo( x : eq<int,int> ) { 42 }

fun main() = foo(Eq(id))
