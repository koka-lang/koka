ambient<a> control send(x : a) : ()
ambient<a> control receive() : a

fun sends( xs : list<a> ) : send<a> () {
  xs.foreach(send)
}

fun catter() : <receive<string>,div> string {
  val s = receive()
  if (s=="") then "" else s + catter()
}

fun pipe( sender : () -> <send<a>,pure|e> (), receiver : () -> <receive<a>,pure|e> b) : <pure|e> b {
  with control receive() {
    pipe-await( sender, resume-shallow )
  }
  receiver()
}

fun pipe-await( sender : () -> <send<a>,pure|e> (), received : a -> <receive<a>,pure|e> b) : <pure|e> b {
  with control send(x) {
    pipe( { resume-shallow(()); () }, { received(x) } )
  }
  sender()
  error("nothing to send anymore")
}

fun example1() {
  pipe( { ["do", "be", "do", "be", "do", ""].sends }, catter)
}

fun spacer() {
  send(receive())
  send(" ")
  spacer()
}


fun example2() {
  pipe( { ["do", "be", "do", "be", "do", ""].sends }, { pipe( spacer, catter ) } )
}

fun example2a() {
  with pipe({ ["do", "be", "do", "be", "do", ""].sends})
  with pipe(spacer)
  catter()
}

fun main() {
  example1().println
  example2().println
  example2a().println
}
