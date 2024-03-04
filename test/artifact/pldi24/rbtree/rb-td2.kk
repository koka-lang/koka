// 2023, Daan Leijen
module rb-td2

import std/num/random
import std/num/int32
import rbtree

fip fun is-red(^t : rbtree) : bool
  match t
    Node(Red) -> True
    _         -> False

fip fun set-black( t : rbtree ) : rbtree
  match t 
    Node(_,l,x,v,r) -> Node(Black,l,x,v,r)
    Leaf            -> Leaf


pub fun main()
  benchmain(access)

pub fun access(t : rbtree, k : key) : div rbtree
  find(t, k, from-key(k), empty-cursor)


// Derivative trees 
type dtree
  NodeR(color : color, lchild : rbtree, key : key, value : tval, dhole : dhole)
  NodeL(color : color, dhole : dhole, key : key, value : tval, rchild : rbtree)
  NodeNone   // for convenience

ref type dhole   // define as a reference type so the derivative tree constructors can reuse with Node's
  Hole
  
// append a `dtree` to a context `acc`.
fip fun (++)( acc : ctx<rbtree>, t : dtree ) : ctx<rbtree>
  match t
    NodeR(c,l,x,v,_) -> acc ++ ctx Node(c,l,x,v,_)
    NodeL(c,_,x,v,r) -> acc ++ ctx Node(c,_,x,v,r)
    NodeNone         -> acc
  
// plug an `rbtree`` into a `dtree`
fip fun (++.)( d : dtree, t : rbtree ) : rbtree
  match d
    NodeR(c,l,x,v,_) -> Node(c,l,x,v,t)
    NodeL(c,_,x,v,r) -> Node(c,t,x,v,r)
    NodeNone         -> t

fip fun is-red( ^t : dtree ) : bool
  match t
    NodeR(Red) -> True
    NodeL(Red) -> True
    _          -> False    



// A cursor maintains a parent, grand parent, and context above that
type cursor 
  Cursor( p : dtree, g : dtree, top : ctx<rbtree> )

val empty-cursor =
  Cursor( NodeNone, NodeNone, ctx _)

fip fun (++)( Cursor(p,g,top) : cursor, t : dtree ) : cursor
  Cursor( t, p, top ++ g )

fip fun (++.)( Cursor( p, g, top ) : cursor, t : rbtree ) : rbtree
  top ++. (g ++. (p ++. t))

fip fun parent-is-red( ^Cursor(p,_,_) : cursor ) : bool
  is-red(p)

fip fun fixup( Cursor(p,g,top) : cursor, t : dtree ) : cursor
  match (t,p,g)
    // single rotations:
    (t,NodeL(_,_,px,pv,pr),NodeL(_,_,gx,gv,gr)) 
      -> Cursor(t, NodeL(Black,Hole,px,pv,Node(Red,pr,gx,gv,gr)),top)
    (t,NodeR(_,pl,px,pv,_),NodeR(_,gl,gx,gv,_)) 
      -> Cursor(t, NodeR(Black,Node(Red,gl,gx,gv,pl),px,pv,Hole),top)
    // double rotations:
    (NodeL(_,_,x,v,r),NodeR(_,pl,px,pv,_),NodeL(_,_,gx,gv,gr)) 
      -> Cursor(NodeR(Red,pl,px,pv,Hole), NodeL(Black,Hole,x,v,Node(Red,r,gx,gv,gr)),top)  
    (NodeR(_,l,x,v,_),NodeR(_,pl,px,pv,_),NodeL(_,_,gx,gv,gr)) 
      -> Cursor(NodeL(Red,Hole,gx,gv,gr), NodeR(Black,Node(Red,pl,px,pv,l),x,v,Hole),top)
    (NodeL(_,_,x,v,r),NodeL(_,_,px,pv,pr),NodeR(_,gl,gx,gv,_)) 
      -> Cursor(NodeR(Red,gl,gx,gv,Hole), NodeL(Black,Hole,x,v,Node(Red,r,px,pv,pr)),top)
    (NodeR(_,l,x,v,_),NodeL(_,_,px,pv,pr),NodeR(_,gl,gx,gv,_)) 
      -> Cursor(NodeL(Red,Hole,px,pv,pr), NodeR(Black,Node(Red,gl,gx,gv,l),x,v,Hole),top)  
    // cannot happen:
    (t,p,g) -> Cursor(t,p,top ++ g)


// If a node is black with red children, then flip the colors
fip fun flip-color( t : rbtree ) : rbtree
  match t
    Node(Black, Node(Red,ll,lx,lv,lr), x, v, Node(Red,rl,rx,rv,rr))
      -> Node(Red, Node(Black,ll,lx,lv,lr), x, v, Node(Black,rl,rx,rv,rr))
    t -> t

// find and possibly insert `k`,`w` into a tree `t` with parent `p` and grandparent `g` under `acc`.
fip(1) fun find(t : rbtree, k : key, w : tval, c : cursor) : div rbtree
  match flip-color(t)
    Node(Red, l, x, v, r) | parent-is-red(c) && x != k ->  // rotate to fix red-red between t and p ?      
      if x < k then find(r, k, w, c.fixup( NodeR(Red,l,x,v,Hole) ) )
               else find(l, k, w, c.fixup( NodeL(Red,Hole,x,v,r) ) )
    Node(clr, l, x, v, r) ->
      if   x < k then find(r, k, w, c ++ NodeR(clr,l,x,v,Hole) )
      elif x > k then find(l, k, w, c ++ NodeL(clr,Hole,x,v,r) )
                 else c.plug( Node(clr,l,k,w,r) )
    Leaf -> c.plug( Node(Red,Leaf,k,w,Leaf) )

// Plug a node `t` into a cursor
fip fun plug( c : cursor, t : rbtree ) : rbtree
  match t 
    Node(Red,l,x,v,r) | parent-is-red(c)   // sometimes we need rotate to fix red-red between t and p
      -> c.fixup( NodeL(Red,Hole,x,v,r) ) ++. l
    t -> c ++. t


/*
// Anton's initial implementation:

type color = { Rd, Bk }
type tree = { E, T(color : color, lchild : tree, key : int32, value : bool, rchild : tree) }
type zipper
  L(color : color, lchild :   (), key : int32, value : bool, rchild : tree)
  R(color : color, lchild : tree, key : int32, value : bool, rchild :   ())

// Inline to expose TRMC opportunity
inline fun app(z : zipper, t : tree)
  match z
    L(c, (), k, v, r) -> T(c, t, k, v, r)
    R(c, l, k, v, ()) -> T(c, l, k, v, t)

fun is-red(z : zipper)
  match z
    L(Rd, _, _, _, _) -> True
    R(Rd, _, _, _, _) -> True
    _ -> False

fun flip-color(t : tree)
  match t 
    T(Bk, T(Rd, xll, xlk, xlv, xlr), xk, xv, T(Rd, xrl, xrk, xrv, xrr)) ->
      T(Rd, T(Bd, xll, xlk, xlv, xlr), xk, xv, T(Bd, xrl, xrk, xrv, xrr))
    _ -> t

// Return new x and p after rotation
fun rotate(x : zipper, p : zipper, g : zipper)
  match g, p, x
    L(_, (), gk, gv, gr), L(_, (), pk, pv, pr), x ->
      (x, L(Bk, (), pk, pv, T(Rd, pr, gk, gv, gr)))
    R(_, gl, gk, gv, ()), R(_, pl, pk, pv, ()), x ->
      (x, R(Bk, T(Rd, gl, gk, gv, pl), pk, pv, ()))
    L(_, (), gk, gv, gr), R(_, pl, pk, pv, ()), L(_, (), xk, xv, xr) ->
      (R(Rd, pl, pk, pv, ()), L(Bk, (), xk, xv, T(Rd, xr, gk, gv, gr)))
    L(_, (), gk, gv, gr), R(_, pl, pk, pv, ()), R(_, xl, xk, xv, ()) ->
      (L(Rd, (), gk, gv, gr), R(Bk, T(Rd, pl, pk, pv, xl), xk, xv, ()))
    R(_, gl, gk, gv, ()), L(_, (), pk, pv, pr), L(_, (), xk, xv, xr) ->
      (L(Rd, gl, gk, gv, ()), R(Bk, (), xk, xv, T(Rd, xr, pk, pv, pr)))
    R(_, gl, gk, gv, ()), L(_, (), pk, pv, pr), R(_, xl, xk, xv, ()) ->
      (R(Rd, (), pk, pv, pr), L(Bk, T(Rd, gl, gk, gv, xl), xk, xv, ()))

fun balance(k : int32, v : bool, t : tree, x : zipper, p : zipper, g : zipper)
  if is-red(x) && is-red(p) then
    val (x, p) = rotate(x, p, g)
    insert2(k, v, t, x, p)
  else app(g, insert2(k, v, t, x, p))

fun balance0(t : tree, x : zipper, p : zipper, g : zipper)
  if is-red(x) && is-red(p) then
    val (x, p) = rotate(x, p, g)
    app(p, app(x, t))
  else app(g, app(p, app(x, t)))

fun insert2(k : int32, v : bool, x : tree, p : zipper, g : zipper)
  val x = flip-color(x)
  match x
    T(xc, xl, xk, xv, xr) ->
      if(k == xk) then balance0(xl, L(xc, (), xk, xv, xr), p, g)
      else if(k < xk) then balance(k, v, xl, L(xc, (), xk, xv, xr), p, g)
      else balance(k, v, xr, R(xc, xl, xk, xv, ()), p, g)
    E -> balance0(E, L(Rd, (), k, v, E), p, g)

fun insert1(k : int32, v : bool, x : tree, p : zipper)
  val x = flip-color(x)
  match x
    T(xc, xl, xk, xv, xr) ->
      if(k == xk) then app(p, x)
      else if(k < xk) then insert2(k, v, xl, L(xc, (), xk, xv, xr), p)
      else insert2(k, v, xr, R(xc, xl, xk, xv, ()), p)
    E -> app(p, T(Rd, E, k, v, E))

fun insert(x : tree, k : int32, v : bool)
  val x = flip-color(x)
  match x
    T(_, xl, xk, xv, xr) ->
      if(k == xk) then T(Bk, xl, xk, xv, xr)
      else if(k < xk) then insert1(k, v, xl, L(Bk, (), xk, xv, xr))
      else insert1(k, v, xr, R(Bk, xl, xk, xv, ()))
    E -> T(Bk, E, k, v, E)

fun fold(t : tree, b : a, f: (int32, bool, a) -> a) : a
  match t
    T(_, l, k, v, r) -> r.fold( f(k, v, l.fold(b, f)), f)
    E                -> b


fun make-tree-aux(n : int32, t : tree) : div tree
  if n <= zero then t else
    val n1 = n.dec
    make-tree-aux(n1, insert(t, n1, n1 % 10.int32 == zero))

pub fun make-tree(n : int32) : div tree
  make-tree-aux(n, E)
  

pub fun main()
  val n = get-args().head("").parse-int.default(4200000).int32
  val t = make-tree(n)
  val v = t.fold(zero) fn(k,v,r:int32){ if (v) then r.inc else r }
  v.show.println
*/