/* Based on the paper:

    "A Functional Implementation of the Garsia–Wachs Algorithm", Jean-Christophe Filliatre,
    ML workshop 2008, pages 91--96. [slides](https://www.lri.fr/~filliatr/pub/wml08-slides.pdf)

  which describes a functional implementation of the tree minimization algorithm show in:

    "A new algorithm for minimum cost binary trees", Adriano M. Garsia and Michelle L. Wachs,
    SIAM Journal on Computing, 6(4):622–642, 1977.

  which was further explained by Donald E. Knuth in
  "The Art of Computer Programming -- Optimum binary search trees" (Vol. 3, Sec. 6.2.2).

  The algorithm is interesting since it uses mutable references shared between a list and tree but the
  side effects are not observable from outside. Koka automatically infers that the final algorithm is pure.
*/

pub fun main()
  example().println


//----------------------------------------------------
// Trees
//----------------------------------------------------

// A tree with all values in the leaves.
type tree<a>
  Leaf(value : a)
  Node(left : tree<a>, right : tree<a>)


fun tree/show( t : tree<string> ) : string
  match t
    Leaf(s)   -> s.show
    Node(l,r) -> "Node(" ++ l.show ++ "," ++ r.show ++ ")"


//----------------------------------------------------
// Non empty lists
//----------------------------------------------------

// A list with at least one element.
pub value type list1<a>
  // The head of a list with at least one element.
  Cons1( head : a, tail : list<a> )


fun map( xs, f )
  val Cons1(y,ys) = xs
  Cons1(f(y), core/list/map(ys,f))


fun zip( xs :list1<a>, ys :list1<b> ) : list1<(a,b)>
  Cons1( (xs.head, ys.head), zip(xs.tail, ys.tail))



//----------------------------------------------------
// Phase 1
//----------------------------------------------------

fun insert( after : list<(tree<a>,int)>, t : (tree<a>,int), before : list<(tree<a>,int)> ) : div tree<a>
  match before
    Nil -> extract( [], Cons1(t,after) )
    Cons(x,xs) ->
      if x.snd < t.snd then insert( Cons(x,after), t, xs ) else
        match xs
          Nil        -> extract( [], Cons1(x,Cons(t,after)) )
          Cons(y,ys) -> extract( ys, Cons1(y,Cons(x,Cons(t,after))) )


fun extract( before : list<(tree<a>,int)>, after : list1<(tree<a>,int)> ) : div tree<a>
  val Cons1(x as (t1,w1), xs ) = after
  match xs
    Nil -> t1
    Cons(y as (t2,w2), ys) -> match ys
      Nil -> insert( [], (Node(t1,t2), w1+w2), before )
      Cons((_,w3),_zs) ->
        if (w1 <= w3)
          then insert(ys, (Node(t1,t2), w1+w2), before)
          else extract(Cons(x,before), Cons1(y,ys))


fun balance( xs : list1<(tree<a>,int)> ) : div tree<a>
  extract( [], xs )


fun mark( depth : int, t : tree<(a,ref<h,int>)> ) : <write<h>> ()
  match t
    Leaf((_,d)) -> d := depth
    Node(l,r)   -> { mark(depth+1,l); mark(depth+1,r) }


fun build( depth : int, xs : list1<(a,ref<h,int>)> ) : <read<h>,div> (tree<a>,list<(a,ref<h,int>)>)
  if !xs.head.snd == depth return (Leaf(xs.head.fst), xs.tail)
  val l = build(depth+1, xs)
  match l.snd
    Nil -> (l.fst, Nil)
    Cons(y,ys) ->
      val r = build(depth+1, Cons1(y,ys))
      (Node(l.fst,r.fst), r.snd)


fun example()
  val wlist = Cons1(("a",3), [("b",2),("c",1),("d",4),("e",5)])
  val tree  = wlist.garsia-wachs()
  tree.show()


fun garsia-wachs( xs : list1<(a,int)> ) : div tree<a>
  val refs   = xs.map( fn((x,_)) (x,ref(0)) )
  val wleafs = zip( refs.map(Leaf), xs.map(snd) )
  val tree   = balance(wleafs)
  mark(0,tree)
  build(0,refs).fst

