module tree

import std/os/env
import std/num/random
import std/num/int32
import std/num/int64

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

pub type tree 
  Leaf
  Node(left : tree, key : key, right : tree)

pub type root
  Root(left : tree, key : key, right : tree)  



// ---------------------------------------------------------
// show and print trees

pub fun show( t : tree ) : string
  t.render.lines.unlines

pub fun show-trees( ts : list<tree> ) : string
  ts.map-indexed( fn(i,t) "tree " ++ show(i+1) ++ ":\n\n" ++ t.show ).join("\n\n")

pub fun print( t : tree ) : io ()
  t.show.println

pub fun print-trees( ts : list<tree> ) : io ()
  ts.show-trees.println


// sequence of accesses
pub fun seq( t : tree, access : (tree,key) -> e tree, accesses : list<int> ) : e list<tree>
  match accesses
    Nil -> []
    Cons(i,is) -> val t1 = t.access(i.to-key) in Cons(t1,seq(t1,access,is))

// Show sequence of accesses
pub fun seqprint( t0 : tree, access : (tree,key) -> <io|e> tree, accesses : list<int> ) : <io|e> ()
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

pub fun render( t : tree ) : render
  val minwidth = 1
  match t
    Leaf -> Render(0,[])
    Node(l,x,r) ->
      beside(l.render,"-" ++ x.show.pad-left(minwidth) ++ "-",r.render)


// --------------------------------------------------------
// Show latex for a tree

pub fun print-tex( t : tree ) : io ()
  t.show-tex.println

pub fun show-tex( t : tree ) : div string
  "~ begin snippet\n\\begin{tikzpicture}[bintree,branchone]\n\\begin{scope}\\draw\n" ++ 
   tex-node(t).indent(2).unlines ++ 
    ";\n\\end{scope}\n\\end{tikzpicture}\n~ end snippet\n"

fun tex-node( t : tree ) : div list<string>
  match t
    Leaf -> []
    Node(l,x,r) -> 
      val node = ["node{" ++ x.show ++ "}"]
      val children = match (l,r)
                       (Leaf,Leaf) -> []
                       _           -> tex-child(l) ++ tex-child(r)
      node ++ children

fun tex-child( t : tree ) : div list<string>
  match t
    Leaf -> ["child[missing]"]
    _    -> ["child{"] ++ tex-node(t).indent(2) ++ ["}"]

fun indent( lines : list<string>, i : int ) : list<string> 
  val sp = spaces(i)
  lines.map(fn(l) sp ++ l)

// --------------------------------------------------------------------------------------
// Levy and Tarjan's test cases Fig 6. from "A New Path from Splay to Dynamic Optimality"

fun node( l : tree, i : int, r : tree ) : tree
  Node(l,i.to-key,r)

pub val tIV = node(Leaf,1,node(Leaf,2,node(Leaf,3,node(Leaf,4,Leaf))))
pub val tV  = node(node(Leaf,1,node(node(Leaf,2,Leaf),3,Leaf)),4,Leaf)
pub val tVI = node(Leaf,1,node(node(node(Leaf,2,Leaf),3,Leaf),4,Leaf))
pub val tXI = node(node(node(node(Leaf,1,Leaf),2,Leaf),3,Leaf),4,Leaf)
pub val tXII= node(Leaf,1,node(node(Leaf,2,node(Leaf,3,Leaf)),4,Leaf)) 

fun rotate-left( xs : list<a>, n : int ) : list<a>
  xs.drop(n) ++ xs.take(n)

fun rotate-right( xs : list<a>, n : int ) : list<a>
  xs.reverse.rotate-left(n).reverse

// Trees corresponding to Fig 6 in Levy & Tarjan
pub fun alltrees(access) 
  val ts = [tV] ++ tV.seq(access,[1,3,4,1,2,4]) ++ [tXII] ++ tXII.seq(access,[4,2,1,4,3,1])
  ts.rotate-right(4)

// Unlike bottom-up, we cannot transition to V,VI and XII,XII states 
pub fun test1(access) -> tIV.seqprint(access,[4,3,1])         // weird: IV -> II -> III -> IV
pub fun test2(access) -> tV.seqprint(access,[1,3,4,1,2,4])    // ok: V -> VI -> VII -> VIII -> IX -> X -> XI
pub fun test3(access) -> tXI.seqprint(access,[1,2,4])         // weird: XI -> IX -> X -> XI
pub fun test4(access) -> tXII.seqprint(access,[4,2,1,4,3,1])  // ok: XII -> XIII -> XIV -> I -> II -> III -> IV

pub fun test5(access) -> tIV.seqprint(access,[4,1,3,4,1,2,4]) // IV -> ... -> XI  (half a cycle)
pub fun test6(access) -> tXI.seqprint(access,[1,4,2,1,4,3,1]) // XI -> ... -> IV  (half a cycle)

pub fun tleft( n : int ) 
  list(1,n).reverse.foldl(Leaf, fn(t,i) node(Leaf,i,t))


// --------------------------------------------------------------------------------------
// Benchmarking

fun size-acc( t : tree, acc : int ) : int
  match t
    Leaf -> acc
    Node(l,_,r) -> size-acc(r, size-acc(l,acc + 1))

fun size(t : tree) : int
  size-acc( t, 0 )

fun sum-acc( t : tree, acc : int ) : int
  match t
    Leaf -> acc
    Node(l,x,r) -> sum-acc(r, sum-acc(l,acc + x.from-key))

fun sum(t : tree) : int
  sum-acc( t, 0 )

fun top( t : tree ) : int
  match t 
    Leaf -> 0
    Node(_,x,_) -> x

fun max-height(t : tree ) : int
  match t
    Leaf          -> 0
    Node(l,_,r) -> 1 + max(max-height(l),max-height(r))

fun min-height(t : tree ) : int
  match t
    Leaf          -> 0
    Node(l,_,r) -> 1 + min(min-height(l),min-height(r))



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



fun bench-iter( i : int, n : int, access : (tree,key) -> tree, rs : rndstate, tree : tree ) : div (int,tree)
  if (i > 0) then
    val step = rnd-step(rs)
    val t = tree.access((step.rnd.int % n).to-key)
    bench-iter( i - 1, n, access, step.rstate, t)
  else 
    (rnd-step(rs).rnd.int, tree)

pub fun bench( n : int, iter : int, access : (tree,key) -> tree ) : div (int,tree)
  bench-iter( n*iter, n, access, rnd-init(42,43), Leaf)

pub inline fun benchmain( access : (tree,key) -> tree, scaledown : int = 1) : io ()
  val n = get-args().head("").parse-int.default(100000)
  //val n = 50000
  val (i,t) = bench(n / scaledown, 100 / scaledown, access)
  val tp = top(t)
  val maxh = max-height(t)
  val minh = min-height(t)
  println("sum: " ++ sum(t).show ++ ", height: " ++ maxh.show ++ "/" ++ minh.show ++ ", top: " ++ tp.show ++ ", final access: " ++ i.show) // ++ ", size: " ++ size(t).show)
    //t.print
