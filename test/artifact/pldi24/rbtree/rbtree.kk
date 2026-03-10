module rbtree

import std/os/env
import std/num/random
import std/num/int32

/*
pub alias key = int32

pub fun to-key(i : int) : key
  i.int32

pub fun from-key(k : key) : int
  k.int
*/

pub alias key = int

pub fun to-key(i : int) : key
  i

pub fun from-key(k : key) : int
  k

pub alias tval = int

pub type color
  Red
  Black

pub type rbtree
  Leaf
  Node(color : color, left : rbtree, key : key, value : tval, right : rbtree)

pub type root
  Root(color : color, left : rbtree, key : key, value : tval, right : rbtree)


// ---------------------------------------------------------
// show and print trees

pub fun show( t : rbtree ) : string
  t.render.lines.unlines

pub fun showc( c : color ) : string
  match c
    Red -> "r"
    Black -> "b"

pub fun show-trees( ts : list<rbtree> ) : string
  ts.map-indexed( fn(i,t) "rbtree " ++ show(i+1) ++ ":\n\n" ++ t.show ).join("\n\n")

pub fun print( t : rbtree ) : io ()
  t.show.println

pub fun print-trees( ts : list<rbtree> ) : io ()
  ts.show-trees.println


// sequence of accesses
pub fun seq( t : rbtree, access : (rbtree,key) -> e rbtree, accesses : list<int> ) : e list<rbtree>
  match accesses
    Nil -> []
    Cons(i,is) -> val t1 = t.access(i.to-key) in Cons(t1,seq(t1,access,is))

// Show sequence of accesses
pub fun seqprint( t0 : rbtree, access : (rbtree,key) -> <io|e> rbtree, accesses : list<int> ) : <io|e> ()
  println("original:\n")
  t0.show.println
  zip(accesses,seq(t0,access,accesses)).foreach fn((i,t))
    println("\naccess " ++ i.show ++ ":\n")
    t.show.println


// --------------------------------------------------------
// Render nice trees

value struct render
  root  : int
  lines : list<string>

fun height(r : render ) -> r.lines.length
fun width(r : render )  -> max(1,r.lines.map(count).maximum)

fun pad-render( r : render, n : int ) : render
  Render(r.root, r.lines ++ replicate("",n - r.height))

fun pad-height( l : render, r : render ) : (render,render)
  val h = max(l.height,r.height)
  (pad-render(l,h),pad-render(r,h))

fun fill( n : int, c : char )
  pad-right("",n,c)

fun spaces(n : int)
  fill(n,' ')

fun half(i : int)
  if i.is-odd then i/2 + 1 else i/2

fun beside(l : render, head : string, r : render ) : render
  val (l1,r1) = pad-height(l,r)
  val wl  = l1.width
  val wr  = r1.width
  val lheader = spaces(l.root - 1) ++ "+" ++ fill(wl - l.root - 1,'-')
  val rheader = fill(r.root - 1,'-') ++ "+"
  val header  = lheader ++ head ++ rheader
  val sep = spaces(head.count)
  val under = zipwith(l1.lines,r1.lines,fn(ls,rs) ls.pad-right(wl) ++ sep ++ rs)
  val newroot = lheader.count + head.count - 1
  Render(newroot, Cons(header, under))

pub fun render( t : rbtree ) : render
  val minwidth = 3
  match t
    Leaf -> Render(0,[])
    Node(c,l,x,_,r) ->
      beside(l.render,"-" ++ (x.show ++ "@" ++ c.showc).pad-left(minwidth) ++ "-",r.render)


// --------------------------------------------------------
// Show latex for a tree

pub fun print-tex( t : rbtree ) : io ()
  t.show-tex.println

pub fun show-tex( t : rbtree ) : div string
  "~ begin snippet\n\\begin{tikzpicture}[bintree,branchone]\n\\begin{scope}\\draw\n" ++
   tex-node(t).indent(2).unlines ++
    ";\n\\end{scope}\n\\end{tikzpicture}\n~ end snippet\n"

fun tex-node( t : rbtree ) : div list<string>
  match t
    Leaf -> []
    Node(c,l,x,_,r) ->
      val node = ["node[color=" ++ (if is-red(c) then "red" else "black") ++ "]{" ++ x.show ++ "}"]
      val children = match (l,r)
                       (Leaf,Leaf) -> []
                       _           -> tex-child(l) ++ tex-child(r)
      node ++ children

fun tex-child( t : rbtree ) : div list<string>
  match t
    Leaf -> ["child[missing]"]
    _    -> ["child{"] ++ tex-node(t).indent(2) ++ ["}"]

fun indent( lines : list<string>, i : int ) : list<string>
  val sp = spaces(i)
  lines.map(fn(l) sp ++ l)


// --------------------------------------------------------------------------------------
// Benchmarking

fun top( t : rbtree ) : int
  match t
    Leaf -> 0
    Node(_,_,x,_,_) -> x

fun max-height(t : rbtree ) : int
  match t
    Leaf          -> 0
    Node(_,l,_,_,r) -> 1 + max(max-height(l),max-height(r))

fun min-height(t : rbtree ) : int
  match t
    Leaf          -> 0
    Node(_,l,_,_,r) -> 1 + min(min-height(l),min-height(r))

fun sum-acc( t : rbtree, acc : int ) : int
  match t
    Leaf -> acc
    Node(_,l,x,_,r) -> sum-acc(r, sum-acc(l,acc + x.from-key))

fun sum(t : rbtree) : int
  sum-acc( t, 0 )

/* linear congruence
value struct rndstate
  seed : int64

value struct rndres
  rnd   : int32
  rstate : rndstate

fun rnd-step( r : rndstate ) : rndres
  val s = (r.seed * 134775813.int64).inc
  val x = rotr(s,17).sar(32).int32
  Rndres(x,Rndstate(s))

fun rnd-init( s0 : int, s1 : int ) : rndstate
  val s  = s0 * s1
  val r0 = Rndstate(s.int64)
  fold-int(0,12,r0, fn(i,r) rnd-step(r).rstate)
*/

pub alias rndstate = sfc
alias rndres = sfc-result

fun rnd-step( r : rndstate ) : rndres
  sfc-step(r)

fun rnd-init( s0 : int, s1 : int ) : rndstate
  (sfc-init32(s0.int32,s1.int32))



fun bench-iter( i : int, n : int, access : (rbtree,key) -> div rbtree, rs : rndstate, tree : rbtree ) : div (int,rbtree)
  if (i > 0) then
    val step = rnd-step(rs)
    val t = tree.access((step.rnd.int % n).to-key)
    bench-iter( i - 1, n, access, step.rstate, t)
  else
    (rnd-step(rs).rnd.int, tree)


pub fun bench( n : int, iter : int, access : (rbtree,key) -> div rbtree ) : div (int,rbtree)
  bench-iter( n*iter, n, access, rnd-init(42,43), Leaf)

pub inline fun benchmain( access : (rbtree,key) -> div rbtree, scaledown : int = 1) : io ()
  val n = get-args().head("").parse-int.default(100000)
  //val n = 50000
  val (i,t) = bench(n / scaledown, 100 / scaledown, access)
  val tp = top(t)
  val maxh = max-height(t)
  val minh = min-height(t)
  println("sum: " ++ sum(t).show ++ ", height: " ++ maxh.show ++ "/" ++ minh.show ++ ", top: " ++ tp.show ++ ", final access: " ++ i.show) // ++ ", size: " ++ size(t).show)

  //t.print
