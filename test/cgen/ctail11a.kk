
pub fun partition-ctx( xs : list<a>, ^pred : a -> e bool ) : e (list<a>,list<a>)
  partition-acc-ctx(xs, pred, ctx _, ctx _)

fun partition-acc-ctx(xs : list<a>, ^pred : a -> e bool, acc1 : ctx<list<a>>, acc2 : ctx<list<a>>): e (list<a>, list<a>)
  match xs
    Nil -> (acc1 ++. Nil, acc2 ++. Nil)
    Cons(x,xx) -> if pred(x)
      then partition-acc-ctx(xx,pred,acc1 ++ ctx Cons(x,_),acc2)
      else partition-acc-ctx(xx,pred,acc1,acc2 ++ ctx Cons(x,_))

effect fun select( i : int ) : bool

pub fun main()
  with fun select(i : int )
         println(i)
         i.is-odd
  val (odds,evens) = list(1,10).partition(select)
  odds.show.println
  evens.show.println