// Issue: https://github.com/koka-lang/koka/issues/836

fun foo(f) f()
val bar : forall<e> (() -> e ()) -> () = foo

fun main() : ()
  bar
    println("hello")