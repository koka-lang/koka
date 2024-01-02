
ref struct monad<m>
  pure : forall<a> a -> m<a>
  bind : forall<a,b> (m<a>, a -> m<b>) -> m<b>

ref struct monadplus<m>
  ??monad  : monad<m> // unpack ??
  ?plus  : forall<a> (m<a>,m<a>) -> m<a> // implicit constructor argument, no unpacking (can't unpack function types)

// Generates the constructor
fun monadplus/Monadplus(?plus, ??monad)
  monadplus(?monad, ?plus )

// And methods to unpack fields
fun monadplus/pure(?monad: monad<m>)
  monad.pure

fun monadplus/bind(?monad: monad<m>)
  monad.bind

val int/plus = int/(+)
val int/monadplus:monadplus<int> = Monadplus() // Automatically inferred parameters to the constructors

// So now extensible row types are just a special case of implicits, 
// and unpacking can be part of struct definitions. -- Potentially very powerful, and also supports inductive types

type xList<a>
  NilX
  ConsX(??element: list<a>, list: xList<a>)

// Defines the constructor
fun xList/ConsX(??element: list<a>, ?list: xList<a>)
  ConsX(?element, ?list)

// and 
fun xList/head(?element: list<a>)
  match element
    Cons(x,_) -> Just(x)
    _ -> Nothing

// and
fun xList/tail(?element: xList<a>)
  match element
    Cons(_,xs) -> Just(xs)
    _ -> Nothing

// For fields that are present on all constructors we would not use the maybe type as the return type
// Question would be whether we recursively unpack the fields of all nested unpacked fields or only unpack the top level?