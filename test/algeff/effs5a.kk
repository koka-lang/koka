type mybool {
  MyFalse
}
effect ctl foo() : mybool
effect val outer : string

fun main() {
  with val outer = "outer"
  with rcontrol foo(){ rcontext.resume(MyFalse) }
  match(foo()) {
    MyFalse -> println("false")
  }
}