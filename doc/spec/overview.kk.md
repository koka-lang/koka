
# Why Koka? { #why; }

There are many new languages being designed, but only few
bring fundamentally new concepts -- like Haskell with
pure versus monadic programming, or Rust with borrow checking.

Koka distinguishes itself through _effect typing_, _effect handlers_,
and _Perceus_ memory management:

* The core of Koka consists of a small set of well-studied language
  features, like first-class functions, a polymorphic type- and effect
  system, algebraic data types, and effect handlers. Each of these is
  composable and avoid the addition of
  "special" extensions by being as general as possible.

  [Read more about the _minimal but general_ design][#why-mingen]{.learn}

* Koka tracks the (side) _effects_ of every
  function in its type, where pure and effectful computations are
  distinguished. The precise effect typing gives Koka _rock-solid
  semantics_ backed by well-studied category theory, which makes Koka
  particularly easy to reason about for both humans and compilers.

  [Read more about effect types][#why-effects]{.learn}

* Effect handlers let you define advanced control abstractions,
  like exceptions, async/await, or probabilistic programs, 
  as a user library in a typed and composable way.

  [Read more about effect handlers][#why-handlers]{.learn}

* Perceus is an advanced compilation method for reference counting.
  This lets Koka compile directly to C code _without needing
  a garbage collector or runtime system_! This also gives Koka 
  excellent performance in practice.

  [Read more about Perceus reference counting][#why-perceus]{.learn}

* Through Perceus, Koka can do reuse analysis and optimize 
  functional-style programs to use in-place updates.

  [Read more about reuse analysis][#why-fbip]{.learn}

  &nbsp;


## Minimal but General { #why-mingen; }

Koka has a small core set of
orthogonal, well-studied language features -- but each of these is
as general and _composable_ as possible, such that we do not need further
"special" extensions. Core features include first-class functions,
a higher-rank impredicative polymorphic type- and effect system, 
algebraic data types, and effect handlers.

```{.aside}
fun hello-ten() {
  var i := 0
  while { i < 10 } {
    println("hello")
    i := i + 1
  }
}
```

As an example of the _min-gen_ design principle, Koka implements most
control-flow primitives as regular functions. An anonymous function can
be written as `fn(){ <body> }`; but as a syntactic convenience, any
function without arguments can be shortened further to use just braces,
as `{ <body> }`.

We can write a `while` loop now using regular
function calls as shown in the example,
where the call to `while` is desugared to
`while( fn(){ i < 10 }, fn(){ ... } )`. 

This also naturally leads to
_consistency_: an expression between _parenthesis_ is always evaluated
before a function call, whereas an expression between _braces_ (ah,
_suspenders_!) is suspended and may be never evaluated or more than once
(as in our example). This is inconsistent in most other languages where
often the predicate of a `while` loop is written in parenthesis but may
be evaluated multiple times.

[Learn more about basic syntax](#sec-basics)
{.learn}

## Effect Typing { #why-effects; }

Koka infers and tracks the effect of every function in its type -- 
and a function type has 3 parts: the argument types, the effect type, 
and the type of the result. For example: 
```unchecked
fun sqr    : (int)     -> total int       // mathematical total function    
fun divide : (int,int) -> exn int         // may raise an exception (partial)  
fun turing : (tape)    -> div int         // may not terminate (diverge)  
fun print  : (string)  -> console ()      // may write to the console  
fun rand   : ()        -> ndet int        // non-deterministic  
```

The precise effect typing gives Koka rock-solid semantics backed
by well-studied category theory, which makes Koka particularly easy to
reason about for both humans and compilers. (Given the importance of
effect typing, the name Koka was derived from the Japanese word for
_effective_
(&#x52B9;&#x679C;,[K&omacron;ka](https://translate.google.com/#view=home&op=translate&sl=auto&tl=en&text=%E5%8A%B9%E6%9E%9C))).

A function without any effect is called `:total` and corresponds to
mathematically total functions -- a good place to be. Then we have
effects for partial functions that can raise exceptions (`:exn`), and
potentially non-terminating functions as `:div` (divergent). The
combination of `:exn` and `:div` is called `:pure` as that corresponds to
Haskell's notion of purity. On top of that we find mutability (as `:st`)
up to full non-deterministic side effects in `:io`. 

Effects can be polymorphic as well. Consider mapping a function over
a list:
```unchecked
fun map( xs : list<a>, f : a -> e b ) : e list<b> {
  match(xs) {
    Cons(x,xx) -> Cons( f(x), map(xx,f) )
    Nil        -> Nil
  }
}
```
Single letter types are polymorphic (aka, _generic_), and Koka infers
that you map from a list of elements `:a` to a list of elements of
type `:b`. Since `map` itself has no intrinsic effect, the effect 
of applying `map` is exactly the effect of the function `f` that
is applied, namely `:e`. 

[Learn more about effect types][#sec-effect-types]
{.learn}

## Effect Handlers  { #why-handlers; }

Another example of the _min-gen_ design principle: instead of
various special language and compiler extensions to support exceptions,
generators, async/await etc., Koka has full support for 
algebraic effect handlers -- these lets you define advanced control
abstractions like async/await as a user library in a typed and 
composable way.

Here is an example of an effect definition with
one operation to yield `:int` values:
```
effect yield {
  control yield( i : int ) : bool
}
```
Once the effect is declared, we can use it 
for example to yield the elements of a list:
```
fun traverse( xs : list<int> ) : yield () {
  match(xs) {
    Cons(x,xx) -> if (yield(x)) then traverse(xx) else ()
    Nil        -> ()
  }
}
```
The `traverse` function calls `yield` and therefore gets the `:yield` effect in its type,
and if we want to use `traverse`, we need to _handle_ the `:yield` effect. 
This is much like defining an exception handler, except we can receive values (here an `:int`),
and we can _resume_ with a result (which determines if we keep traversing):
```
fun print-elems() : console () {
  with control yield(i){
    println("yielded " + i.show)
    resume(i<=2)
  }
  traverse([1,2,3,4])
}
```
The `with` statement binds the handler for `:yield` over the
rest of the scope, in this case `traverse([1,2,3,4])`. 
Note how the handler discharges the `:yield` effect -- and replaces
it with a `:console` effect. When we run the example, we get:
````
yielded: 1
yielded: 2
yielded: 3
```` 

[Learn more about `with` statements][#sec-with]
{.learn}

[Learn more about effect handlers][#sec-handlers]
{.learn}


## Perceus Optimized Reference Counting  { #why-perceus; }

[![perceus3]](https://en.wikipedia.org/wiki/Perseus_with_the_Head_of_Medusa)

[perceus3]: images/perceus3.jpg "Perseus by Benvenuto Cellini" { width:20%; float:right; margin:1em 0em 1em 2em; border:1px solid #888; }

[test-bench]: https://github.com/koka-lang/koka/tree/master/test/bench

Perceus is the compiler optimized reference counting technique that Koka
uses for automatic memory management [@Perceus:tech]. This (together
with evidence translation [@Xie:evidently])
enables Koka to compile directly to plain C code without needing a
garbage collector or runtime system.

Perceus uses extensive static analysis to aggressively optimize the
reference counts. Here the strong semantic foundation of Koka helps a
lot: inductive data types cannot form cycles, and potential sharing
across threads can be reliably determined.

Normally we need to make a fundamental choice when managing memory: 

- We either use manual memory management (C, C++, Rust) and we get 
  the best performance but at a significant programming burden,
- Or, we use garbage collection (OCaml, C#, Java, Go, etc.) but
  but now we need a runtime system and pay a price in performance,
  memory usage, and unpredictable latencies.

![perceus-perf]

With Perceus, we hope to cross this gap and our goal is to 
be within 2x of the performance of C/C++. Initial benchmarks are
encouraging and show Koka to be close to C performance on various
memory intensive benchmarks.

[perceus-perf]: images/perceus-perf-bw.png { border:1px solid #AAA; width:40%; float:right; margin: 0em 0em 0.25em 1em; }

[See benchmarks](https://github.com/koka-lang/koka#Benchmarks)
{.learn}

[Read the Perceus technical report][Perceus]
{.learn}

## Reuse Analysis { #why-fbip; }

Perceus also performs _reuse analysis_ as part of reference
counting analysis. This pairs pattern matches with constructors of the
same size and reuses them _in-place_ if possible. Take for example,
the `map` function over lists:
```unchecked 
fun map( xs : list<a>, f : a -> e b ) : e list<b> {
  match(xs) {
    Cons(x,xx) -> Cons( f(x), map(xx,f) )
    Nil        -> Nil
  }
}
```

Here the matched `Cons` can be reused by the new `Cons`
in the branch. This means if we map over a list that is not shared, 
like `list(1,100000).map(sqr).sum`,
then the list is updated _in-place_ without any extra allocation.
This is very effective for many functional style programs.

<!--
The corresponding C code that is generated for the fast path becomes similar to:

````cpp
list_t map( list_t xs, function_t f) {
  if (is_Cons(xs)) {
    if (is_unique(xs)) {
      xs->head = apply(dup(f),xs->head);
      xs->tail = map(xs->tail,f);
      return xs;
    }
    else { ... }
  }
  else {
    return Nil;
  }
}
````
-->
````cpp {.aside}
void map( list_t xs, function_t f, list_t* res) {
  while (is_Cons(xs)) {
    if (is_unique(xs)) {  // if xs is not shared..
      box_t y = apply(dup(f),xs->head);      
      if (yielding()) { ... } // if f yielded an effect operation..
      else {
        xs->head = y;      
        *res = xs;          // update previous node in-place
        res = &xs->tail;    // set the result address for the next node
        xs = xs->tail;      // .. and continue with the next node
      }
    }
    else { ... }          // slow path allocates fresh nodes
  }
  *res = Nil;  
}
````

Moreover, the Koka compiler also implements _tail-recursion modulo cons_ (TRMC)
and instead of using a recursive call, the function is eventually optimized 
into an in-place updating loop for the fast path, similar to 
the C code example on the right. 

Importantly, the reuse optimization is guaranteed
and a programmer can see when the optimization applies.
This leads to a new programming technique we call FBIP:
_functional but in-place_. Just like tail-recursion allows us
to express loops with regular function calls, reuse analysis 
allows us to express many imperative algorithms in a purely
functional style. 

[Learn more about FBIP][#sec-fbip]
{.learn}


[Read the Perceus report on reuse analysis][Perceus]
{.learn}


