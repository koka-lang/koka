// Parameters are "required" or "optional" (implicits are required, yet effectively optional until resolved)
// Arguments are "fixed" or "named"

// After matching the named arguments to formal parameters, the fixed arguments must match the ordering of the remaining parameters, with optional arguments staying optional.

// TODO: Change compiler terminology to "required" and "optional" for parameters instead of "fixed" and "named". 
// All formal parameters are named after all (except for higher order functions, which typically use only required arguments - can they use even use optional?).

fun f(required: int, trailingrequired: () -> int, optional: int = 1, ?an-implicit: int): console ()
  println(required.show ++ " + " ++ trailingrequired().show ++ " + " ++ optional.show ++ " + " ++ ?an-implicit.show)
  val result = required + optional + ?an-implicit + trailingrequired()
  println(result)

fun f2(required: int, fx: (int, ?c:int) -> int): int
  val c = 1
  fx(required) + fx(?c=2, required)

val basic/an-implicit = 3

fun main()
  // Normal ordering, using default values for optional parameters
  f(1, {0})
  // Pass an optional parameter as a fixed argument 
  f(1, {2}, 3)
  // Defer trailing lambda till after named arguments (this is the main use case for this feature)
  f(1, optional=2) fn() 1
  // Named argument first, including implicits
  f(?an-implicit=10, optional=0, 1, {2})
  // Use name for required argument to intentionally change order
  f(optional=0, ?an-implicit=30, trailingrequired={2}, 1)
  f(trailingrequired=fn() 1, -1)
  
