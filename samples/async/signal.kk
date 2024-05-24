import uv/signal

fun main()
  with default-event-loop
  val n = ref(0)
  println("Running main")
  signal-start-oneshot(sSIGINT) fn() 
    println("Got SIGINT!")
    signal-start(sSIGINT) fn(s)
      if !n == 5 then
        s.signal-stop()
        ()
      else
        println("Got SIGINT! x " ++ (!n).show)
        n := !n + 1
    ()
  ()
