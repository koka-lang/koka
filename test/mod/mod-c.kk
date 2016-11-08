module mod-c

public import mod-b

public fun fun-c() {
  println("mod-c.c")
  fun-b()
}