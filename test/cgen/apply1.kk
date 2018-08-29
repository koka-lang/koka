// tests codegeneration for parametersless function (needs eta expansion in c# backend)
fun foo() {
  42
}

fun bar( action : () -> e int ) : e int {
  action()
}

fun main() {
  bar(foo)
}