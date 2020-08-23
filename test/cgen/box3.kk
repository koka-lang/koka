// nested boxing
// xs is a list of ()->char
// foo uses the functions in xs as boxed
// bar uses the functions in xs as plain char
// => functions in boxed position must unbox/box their arguments/result

val xs : list<()->char> = [{'x'}]

fun noinline foo( x : a, fs : list<() -> a> ) : a {
  match(fs) {
    Cons(f) -> f()
    _       -> x
  }
}

fun noinline bar( fs : list<() -> char> ) : char {
  match(fs) {
    Cons(f) -> f()
    _       -> 'b'
  }
}

fun test() : (char,char) {
  (foo('a', xs), bar(xs)) 
}

fun main() {
  val (c1,c2) = test()
  c1.println
  c2.println
}
