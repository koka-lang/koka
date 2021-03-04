
import std/num/ddouble
// Changing this to double fixes problem, since (<) for doubles is always visible

alias number = ddouble

// I tried explicitly exposing it, but this cause problems with effects...
fun (<)(x : number, y : number) : bool {
  ddouble/(<)(x,y)
}

/**************
check  : model-using-effect
model-using-effect.kk(9, 5): error: effects do not match
  context        :     (<)
  term           :     (<)
  inferred effect: std/core/types/(<>)
  expected effect: <std/core/types/div|_e>
  because        : effect cannot be subsumed
*/
