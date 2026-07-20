// Dot-implicits (`.?name : tp`) unpack a struct given by the TYPE ANNOTATION,
// so the parameter name need not be the type name. The unpacked fields become
// locally-qualified bindings (`name/field`) available for implicit resolution.
value struct env( indent : int, pre : string )

fun line( s : string, ?indent : int, ?pre : string ) : console ()
  println(pre ++ "  ".repeat(indent) ++ s)

// `.?fmt : env` unpacks env: fields resolve ?indent / ?pre inside the body
fun section( title : string, .?fmt : env ) : console ()
  line(title, ?indent = fmt/indent, ?pre = fmt/pre)
  line(title ++ " (implicit fields)")   // ?indent/?pre resolve via the unpack

// the annotation takes precedence over the parameter name: `env` names a type,
// but the parameter unpacks `wide` — its `post` field only exists there
value struct wide( indent : int, pre : string, post : string )

fun tagged( title : string, .?env : wide ) : console ()
  line(title ++ env/post, ?indent = env/indent, ?pre = env/pre)

fun main()
  section("a", ?fmt = Env(1, ">"))
  val fmt = Env(2, "#")
  section("b")                          // resolves ?fmt by name
  tagged("c", ?env = Wide(0, "$", "!"))
