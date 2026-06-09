// Batched queue (see "Purely Functional Data Structures" by Chris Okasaki, Sect. 5.2)
// - worst-case  O(n), persistent O(n)
// - amortized   O(1), persistent O(n)
module queue/batched

import bench

// batched queue
struct queue<a>
  front : list<a>
  rear  : list<a>

val batched/empty = Queue( Nil, Nil )

fip fun is-empty( ^q : queue<a> ) : bool
  match q
    Queue(Nil,rear) -> is-empty(rear)
    _               -> False

fip fun balance( q : queue<a> ) : queue<a>
  match q
    Queue(Nil  ,rear) -> Queue( reverse(rear), Nil )
    Queue(front,rear) -> Queue( front, rear )

fip(1) fun snoc( Queue(front,rear) : queue<a>, x : a ) : queue<a>
  balance(Queue(front, Cons(x,rear)))

fbip fun uncons( q : queue<a> ) : div maybe2<a,queue<a>>
  match q
    Queue(Cons(x,front),rear) -> Just2(x,balance(Queue(front,rear)))
    Queue(Nil,_) -> Nothing2

pub fun main()
  benchmain(QueueI(batched/empty, snoc, uncons))