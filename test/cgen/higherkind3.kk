// The current C-sharp translation does not support 'nested' polymorphic higher kinds
fun foo( xs : list<c<a>> ) { 1 }

fun main() {
  foo(single(single(1)))
}