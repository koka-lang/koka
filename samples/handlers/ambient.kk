/* Examples from the paper:
   "Programming with Implicit Values, Functions, and Control (or, Implicit Functions: Dynamic Binding with Lexical Scoping)"
   by Jonathan Brachthauser and Daan Leijen.

   Shows how effects can model typed dynamic binding/implicit parameters/ambient state.
*/
module handlers/ambient

// an ambient value `width : int`  (= dynamic binding of type int) (= implicit parameter of type int)
effect val width : int

fun f() 
  // with val width = 78   // done in `main`
  val g = with val width = 80 in (fn() width + 1)
  val h = fn() (with val width = 80 in width + 1)
  g().println   // 79
  h().println   // 81


fun half(action)
  with val width = width / 2
  action()


effect val emitx : ((s:string) -> io ())

fun emit1(action)
  with val emitx = fn(s : string) s.println
  action()


effect fun emit( s : string ) : ()

fun pretty-emit1(action)
  try
    with fun emit(s) s.println
    action()


// dynamic binding with local reasoning
fun pretty-emit2(action)
  with fun emit(s) s.truncate(width).println
  action()


// with lexically scoped state
fun pretty-emit(action)
  var out := ""
  with fun emit(s){ out := out ++ s ++ "\n" } in action()
  out


effect state<s>
  ctl get()      : s
  ctl set(x : s) : ()


fun state1(action)
  var s := 0
  handle(action)
    fun get()  s 
    fun set(x) s := x 


effect fun total() : int

fun emit4(action)
  var out := ""
  with return(x)   out
  with fun emit(s) out := out ++ s ++ "\n" 
  with fun total() out.count 
  action()


effect ctl abort() : ()

fun pretty-abort(action)
  with pretty-emit
  with ctl abort() ()
  action()


effect ctl choice() : bool

fun pretty-all(action)
  with ctl choice() append(resume(True), resume(False)) 
  [pretty-abort(action)]


fun pretty-all2(action) 
  with ctl choice() append(resume(True), resume(False))
  with return(x) [x] 
  pretty-abort(action)


fun state2(init,action) 
  with return(f) f(init) 
  with handler 
    ctl get()  (fn(s) resume(s)(s))
    ctl set(x) (fn(s) resume(())(x))
  val x = action()
  (fn(s) x)

effect val cwd : string

fun is-home()
  cwd == "/home"

fun test1() 
  with val cwd = "/"
  is-home()

fun gchildren( g : graph, v : vertex ) : list<vertex> 
  []

fun bound( g : graph ) 
  10

alias vertex = int;
struct graph()
struct rose(v : vertex, sub : list<rose> )

effect fun marked( v : vertex ) : bool
effect fun mark( v : vertex ) : ()
effect fun children( v : vertex ) : list<vertex>

fun dfs( g : graph, vs : list<vertex> ) : pure list<rose>
  var visited := vector(g.bound,False)
  with fun children(v) g.gchildren(v) 
  with fun marked(v)   visited[v] 
  with fun mark(v)     visited[v] := True 
  dfs-loop(vs)


fun dfs-loop( vs : list<vertex> ) 
  match vs
    Nil -> Nil
    Cons(v,vv) ->
      if marked(v) then dfs-loop(vv) else 
        mark(v)
        val sub = dfs-loop( children(v) )
        Cons( Rose(v,sub), dfs-loop(vv) )
      

pub fun main()
  with val width = 78 in f().println
  with val width = 78 in half(f).println
  with val cwd = "/" in is-home().println
