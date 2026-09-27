// A single-field type that is recursive through a boxed position (here
// `maybe`, a user-defined option type, and a single-field type's type
// variable) must not be a value type: it was boxed as a value while
// represented as a heap pointer, so boxing and unboxing disagreed.
// Issues #775 and #937.

// recursive through `maybe`
struct node
  child : maybe<node>

fun node/depth( n : node ) : div int
  match n.child
    Just(c) -> 1 + c.depth
    Nothing -> 1

// recursive through a user-defined option type
type opt<a>
  None
  Some( x : a )

struct onode
  child : opt<onode>

fun onode/depth( n : onode ) : div int
  match n.child
    Some(c) -> 1 + c.depth
    None -> 1

// mutually recursive through a (non-recursive) single-field type's type variable
type assoc<k,v>
  Assoc( internal : list<(k,v)> )

struct env
  bindings : assoc<string,clos>

reference struct clos
  name : string
  rho : env

fun env/names( e : env ) : div list<string>
  match e.bindings
    Assoc(kvs) -> kvs.flatmap(fn((k, c)) Cons(k, Cons(c.name, c.rho.names)))

fun main()
  println(Node(Just(Node(Just(Node(Nothing))))).depth)
  println(Onode(Some(Onode(None))).depth)
  val inner = Env(Assoc([("y", Clos("g", Env(Assoc([]))))]))
  val envs = [Env(Assoc([("x", Clos("f", inner))])), inner]   // boxes each `env`
  println(envs.map(fn(e) e.names.join(",")).join(";"))
