// --------------------------------------------------------
// Async demo: do concurrently
// --------------------------------------------------------
import std/async
import std/os/readline

effect state<s> {
  fun get() : s
  fun put( x : s ) : ()
}

val state = handler(s) {
  return x -> (x,s)
  get()    -> resume(s,s)
  put(s')  -> resume((),s')
}

fun show-int( i : int ) : string {
  show(i)
}

fun test() : <asyncx,console,state<int>> () {
  println("let's start")
  val (name,age) = interleaved(
    { put(1)
      yield()
      println("what is your name? state:" + get().show-int)
      val name = readline()
      println("name state:" + get().show-int)
      put(3)
      name
    },
    {
      put(2)
      yield()
      println("what is your age? state:" + get().show-int)
      val age = readline()
      println("age state:" + get().show-int)
      put(4)
      age
    }
  )
  println("hi " + name + ": " + age + ": state: " + get().show-int )
}

fun main() {
  state(0,test)
  0
}
