// test lifting of cps functions
// here the 'id' must first be lifted, but then is used 
// as non-cps

val hs : forall<e> list<int -> e int> = [id]

fun bar(x:int) : int {
  head(hs).maybe(id)(x)
}


fun main() {
  bar(42).show.println
}