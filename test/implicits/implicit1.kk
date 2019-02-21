implicit val width : int

fun f() {
  //with val width = 78
  val g = with val width = 80 in (fun(){ width + 1 })
  val h = fun() { with val width = 80 in width + 1 }
  g().println
  h().println
}

fun half(action) {
  with val width = width / 2
  action()
}

implicit val emitx : ((s:string) -> io ())

fun emit1(action) {
  with val emitx = fun( s : string ){ s.println }
  action()
}

implicit fun emit( s : string ) : ()

fun emit2(action) {
  try {
    with fun emit(s) { s.println }
    action()
  }
}

fun emit3(action) {
  var out := ""
  with fun emit(s) { out := out + s + "\n" } in action()
  out
}

implicit fun total() : int

fun emit4(action) {
  var out := ""
  with fun emit(s) { out := out + s + "\n" } in
   with fun total() { out.count } in action()
  out
}

implicit val cwd : string

fun is-home() {
  cwd == "/home"
}

fun test1() {
  with val cwd = "/"
  is-home()
}



fun main() {
  (with val width = 78 in f().println)
  (with val width = 78 in half(f).println)
  (with val cwd = "/" in is-home().println)
}
