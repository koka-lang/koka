// non deteministic TRMC over binary trees 
// this tests the context copying code.
effect nondet 
  ctl flip() : bool
  ctl fail() : a

type tree 
  Bin(l:tree, r:tree)
  Tip(value:int)

fun tmap( t : tree, f : int -> e int ) : e tree 
  match t
    Bin(l,r) -> Bin(tmap(l,f),tmap(r,f))
    Tip(i)   -> Tip(f(i))
  

fun tshow( t : tree ) : string 
  match t
    Bin(l,r) -> "Bin(" ++ l.tshow ++ "," ++ r.tshow ++ ")"
    Tip(i)   -> "Tip(" ++ i.show ++ ")"
  
fun incs( t : tree ) : nondet tree 
  t.tmap fn(i)
    if flip() then i + 1 else i - 1


val solutions = handler
  return(x)  [x]
  ctl fail() []
  ctl flip() resume(True) ++ resume(False)

fun main() 
  val t  = Bin(Bin(Tip(1),Tip(2)),Bin(Tip(3),Tip(4)))
  val ts = solutions{ incs(t) }
  ts.show-list(tshow).println

