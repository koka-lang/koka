module ziptree

import std/os/env
import std/num/random
import std/num/int32
import std/num/int64

/*
pub alias key = int32

fun to-key(i : int) : key
  i.int32

fun from-key(k : key) : int
  k.int
*/
pub alias rank = int
pub alias key = int

pub fip fun is-higher-rank( (r1,k1) : (rank,key), (r2,k2) : (rank,key) ) : bool
  (r1 > r2 || (r1 == r2 && k1 < k2))


fun to-key(i : int) : key
  i

fun from-key(k : key) : int
  k

pub type ztree
  Leaf
  Node(rank : rank, left : ztree, key : key, right : ztree)

pub type root
  Root(rank : rank, left : ztree, key : key, right : ztree)

// rank is hashed from the key so we can combine search and insert
pub fip fun rank-of1( k : key ) : rank
  // pseudo random shuffle by Chris Wellons, see: <https://nullprogram.com/blog/2018/07/31/>
  val x0 = k.int32.inc
  val x1 = xor(x0,shr(x0,15)) * 0x2c1b3c6d.int32
  val x2 = xor(x1,shr(x1,12)) * 0x297a2d39.int32
  val x  = xor(x2,shr(x2,15))
  x.ctz

pub fip fun rank-of( k : key ) : rank
  val x0 = k.int32.inc
  val x1 = xor(x0,shr(x0,16)) *  0x297a2d39.int32
  val x  = xor(x1,shr(x1,16))
  x.ctz


// ---------------------------------------------------------
// show and print trees

pub fun show( t : ztree ) : string
  t.render.lines.unlines

pub fun show-trees( ts : list<ztree> ) : string
  ts.map-indexed( fn(i,t) "ztree " ++ show(i+1) ++ ":\n\n" ++ t.show ).join("\n\n")

pub fun print( t : ztree ) : io ()
  t.show.println

pub fun print-trees( ts : list<ztree> ) : io ()
  ts.show-trees.println


// sequence of accesses
pub fun seq( t : ztree, access : (ztree,key) -> e ztree, accesses : list<int> ) : e list<ztree>
  match accesses
    Nil -> []
    Cons(i,is) -> val t1 = t.access(i.to-key) in Cons(t1,seq(t1,access,is))

// Show sequence of accesses
pub fun seqprint( t0 : ztree, access : (ztree,key) -> <io|e> ztree, accesses : list<int> ) : <io|e> ()
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

pub fun render( t : ztree ) : render
  val minwidth = 3
  match t
    Leaf -> Render(0,[])
    Node(rnk,l,x,r) ->
      beside(l.render,"-" ++ (x.show ++ "@" ++ rnk.show).pad-left(minwidth) ++ "-",r.render)


// --------------------------------------------------------
// Show latex for a tree

pub fun print-tex( t : ztree ) : io ()
  t.show-tex.println

pub fun show-tex( t : ztree ) : div string
  "~ begin snippet\n\\begin{tikzpicture}[bintree,branchone]\n\\begin{scope}\\draw\n" ++
   tex-node(t).indent(2).unlines ++
    ";\n\\end{scope}\n\\end{tikzpicture}\n~ end snippet\n"

fun tex-node( t : ztree ) : div list<string>
  match t
    Leaf -> []
    Node(rnk,l,x,r) ->
      val node = ["node[label={[ranklabel]below:{\\showrank{" ++ rnk.show ++ "}}}]{" ++ x.show ++ "}"]
      val children = match (l,r)
                       (Leaf,Leaf) -> []
                       _           -> tex-child(l) ++ tex-child(r)
      node ++ children

fun tex-child( t : ztree ) : div list<string>
  match t
    Leaf -> ["child[missing]"]
    _    -> ["child{"] ++ tex-node(t).indent(2) ++ ["}"]

fun indent( lines : list<string>, i : int ) : list<string>
  val sp = spaces(i)
  lines.map(fn(l) sp ++ l)

// --------------------------------------------------------------------------------------

fun node( l : ztree, i : int, r : ztree ) : ztree
  val k = i.to-key
  val rnk = rank-of(k)
  Node(rnk,l,k,r)


// --------------------------------------------------------------------------------------
// Benchmarking

fun top( t : ztree ) : int
  match t
    Leaf -> 0
    Node(_,_,x,_) -> x

fun max-height(t : ztree ) : int
  match t
    Leaf          -> 0
    Node(_,l,_,r) -> 1 + max(max-height(l),max-height(r))

fun min-height(t : ztree ) : int
  match t
    Leaf          -> 0
    Node(_,l,_,r) -> 1 + min(min-height(l),min-height(r))


fun sum-acc( t : ztree, acc : int ) : int
  match t
    Leaf -> acc
    Node(_,l,x,r) -> sum-acc(r, sum-acc(l,acc + x.from-key))

fun sum(t : ztree) : int
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


fun bench-iter( i : int, n : int, access : (ztree,key) -> ztree, rs : rndstate, tree : ztree ) : div (int,ztree)
  if (i > 0) then
    val step = rnd-step(rs)
    val t = tree.access((step.rnd.int % n).to-key)
    bench-iter( i - 1, n, access, step.rstate, t)
  else
    (rnd-step(rs).rnd.int, tree)


pub fun bench( n : int, iter : int, access : (ztree,key) -> ztree ) : div (int,ztree)
  bench-iter( n*iter, n, access, rnd-init(42,43), Leaf)
  /*
  val acc0 = Acc(Leaf, rnd-init(42,43))
  //println("init: " ++ acc0.sfc.sfc-step.rnd.int.show)
  val accN = fold-int(0,iter,acc0) fn(_,acc1)
               /*
               // doing ordered accesses at first degrades mtr performance exponentially (while splay trees are hardly affected)
               val t1   = fold-int(0,10000,acc1.tree) fn(i,t) t.access(i)
               val t2   = fold-int(0,10000,t1) fn(i,t) t.access(i)
               val acc2 = Acc(t2,acc1.rstate)
               */
               val acc2 = acc1
               val acc3 = fold-int(0,n,acc2) fn(_,acc : acc)
                            val step = rnd-step(acc.rstate)
                            val t = acc.tree.access((step.rnd.int % n).to-key) // todo: avoid conversion?
                            Acc(t,(step.rstate))
               acc3
  (rnd-step(accN.rstate).rnd.int, accN.tree)
  */

pub inline fun benchmain( access : (ztree,key) -> ztree, scaledown : int = 1) : io ()
  val n = get-args().head("").parse-int.default(100000)
  //val n = 50000
  val (i,t) = bench(n / scaledown, 100 / scaledown, access)
  val tp = top(t)
  val maxh = max-height(t)
  val minh = min-height(t)
  println("sum: " ++ sum(t).show ++ ", height: " ++ maxh.show ++ "/" ++ minh.show ++ ", top: " ++ tp.show ++ ", final access: " ++ i.show) // ++ ", size: " ++ size(t).show)
  //t.print
