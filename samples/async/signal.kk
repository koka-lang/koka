import std/os/signal

fun main()
  // TODO: This needs some work still (processes don't exit)
  println("Running main")
  signal-start-oneshot(sSIGINT, fn() println("Escape!"))
