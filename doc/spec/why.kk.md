
~ begin banners
~ banner { caption:"Minimal but General" }
The core of &koka; consists of a small set of well-studied language
features, like first-class functions, a polymorphic type- and effect
system, algebraic data types, and effect handlers. Each of these is
composable and avoid the addition of
"special" extensions by being as general as possible.

[Read more about the _minimal but general_ design &adown;][#why-mingen]{.learn}
~
~ banner { caption:"Effect Types"}
&koka; tracks the (side) _effects_ of every
function in its type, where pure and effectful computations are
distinguished. The precise effect typing gives &koka; _rock-solid
semantics_ backed by well-studied category theory, which makes &koka;
particularly easy to reason about for both humans and compilers.

[Read more about effect types &adown;][#why-effects]{.learn}
~
~ banner { caption:"Effect Handlers" }
Effect handlers let you define advanced control abstractions,
like exceptions, async/await, or probabilistic programs, 
as a user library in a typed and composable way.\
&nbsp;

[Read more about effect handlers &adown;][#why-handlers]{.learn}
~
~ banner { caption:"Perceus Reference Counting" }
Perceus is an advanced compilation method for reference counting.
This lets &koka; compile directly to C code _without needing
a garbage collector or runtime system_! This also gives &koka; 
excellent performance in practice.

[Read more about Perceus reference counting &adown;][#why-perceus]{.learn}
~
~ banner { caption:"Reuse Analysis" }
Through Perceus, &koka; can do reuse analysis and optimize 
functional-style programs to use in-place updates.

[Read more about reuse analysis &adown;][#why-fbip]{.learn}
~

<!--
This makes many functional algorithms behave
like their imperative counterparts on uniquely owned parameters while
degrading gracefully to use copying when persistence is required.
-->

<!--
~ banner { caption:"FBIP: Functional But In-Place"}
Reuse analysis leads to a new style of programming that we call _FBIP_.
Just like tail-recursion lets us write loops in terms of 
function calls, reuse analysis lets us write many imperative 
algorithms in a functional style.

[Read more about FBIP &adown;][#why-fbip]{.learn}
~
-->

~ end banners



## Minimal but General { #why-mingen; }

&koka; has a small core set of
orthogonal, well-studied language features -- but each of these is
as general and _composable_ as possible, such that we do not need further
"special" extensions. Core features include first-class functions,
a higher-rank impredicative polymorphic type- and effect system, 
algebraic data types, and effect handlers.

```{.aside}
fun hello-ten()
  var i := 0
  while { i < 10 }
    println("hello")
    i := i + 1
```

As an example of the _min-gen_ design principle, &koka; implements most
control-flow primitives as regular functions. An anonymous function can
be written as `fn(){ <body> }`; but as a syntactic convenience, any
function without arguments can be shortened further to use just braces,
as `{ <body> }`. Moreover, using [brace elision][#sec-layout], any
indented block automatically gets curly braces.

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

[Learn more about basic syntax &adown;](book.html#sec-basics)
{.learn}



## Effect Typing { #why-effects; }

&koka; infers and tracks the effect of every function in its type -- 
and a function type has 3 parts: the argument types, the effect type, 
and the type of the result. For example: 
```unchecked
fun sqr    : (int) -> total int       // total: mathematical total function    
fun divide : (int,int) -> exn int     // exn: may raise an exception (partial)  
fun turing : (tape) -> div int        // div: may not terminate (diverge)  
fun print  : (string) -> console ()   // console: may write to the console  
fun rand   : () -> ndet int           // ndet: non-deterministic  
```

The precise effect typing gives &koka; rock-solid semantics and deep 
safety guarantees backed
by well-studied category theory, which makes &koka; particularly easy to
reason about for both humans and compilers. (Given the importance of
effect typing, the name &koka; was derived from the Japanese word for
_effective_
(&#x52B9;&#x679C;, &#12371;&#12358;&#12363;, [K&omacron;ka](https://translate.google.com/#view=home&op=translate&sl=auto&tl=en&text=%E5%8A%B9%E6%9E%9C))).

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
fun map( xs : list<a>, f : a -> e b ) : e list<b> 
  match xs
    Cons(x,xx) -> Cons( f(x), map(xx,f) )
    Nil        -> Nil  
```
Single letter types are polymorphic (aka, _generic_), and &koka; infers
that you map from a list of elements `:a` to a list of elements of
type `:b`. Since `map` itself has no intrinsic effect, the effect 
of applying `map` is exactly the effect of the function `f` that
is applied, namely `:e`. 

[Learn more about effect types &adown;](book.html#sec-effect-types)
{.learn}


## Effect Handlers  { #why-handlers; }

Another example of the _min-gen_ design principle: instead of
various special language and compiler extensions to support exceptions,
generators, async/await etc., &koka; has full support for 
algebraic effect handlers -- these lets you define advanced control
abstractions like async/await as a user library in a typed and 
composable way.

Here is an example of an effect definition with
one _control_ (`ctl`) operation to yield `:int` values:
```
effect yield
  ctl yield( i : int ) : bool
```
Once the effect is declared, we can use it 
for example to yield the elements of a list:
```
fun traverse( xs : list<int> ) : yield () 
  match xs 
    Cons(x,xx) -> if yield(x) then traverse(xx) else ()
    Nil        -> ()
```
The `traverse` function calls `yield` and therefore gets the `:yield` effect in its type,
and if we want to use `traverse`, we need to _handle_ the `:yield` effect. 
This is much like defining an exception handler, except we can receive values (here an `:int`),
and we can _resume_ to the call-site with a result (here, with a boolean that determines if we keep traversing):
```
fun print-elems() : console () 
  with ctl yield(i)
    println("yielded " ++ i.show)
    resume(i<=2)
  traverse([1,2,3,4])
```
The `with` statement dynamically binds the handler for `yield` control operation over the
rest of the scope, in this case `traverse([1,2,3,4])`. 
Every time `yield` is called, our control handler is called, prints the current value,
and resumes to the call-site with a boolean result.
The dynamic binding here is quite safe since we still use static typing! Indeed,
the handler discharges the `:yield` effect -- and replaces
it with a `:console` effect (due to `println`). When we run the example, we get:
````
yielded: 1
yielded: 2
yielded: 3
```` 

[Learn more about `with` statements &adown;](book.html#sec-with)
{.learn}

[Learn more about effect handlers &adown;](book.html#sec-handlers)
{.learn}


## Perceus Optimized Reference Counting  { #why-perceus; }

[![perceus3]](https://en.wikipedia.org/wiki/Perseus_with_the_Head_of_Medusa)

[perceus3]: images/perceus3.jpg "Perseus by Benvenuto Cellini" { width:20%; float:right; margin:1em 0em 1em 2em; border:1px solid #222; }

[test-bench]: https://github.com/koka-lang/koka/tree/master/test/bench

Perceus is the compiler optimized reference counting technique that &koka;
uses for automatic memory management [@Reinking:perceus;@Lorenzen:reuse-tr]. This (together
with evidence passing [@Xie:evp;@Xie:evp-tr;@Xie:evidently])
enables &koka; to compile directly to plain C code without needing a
garbage collector or runtime system.

Perceus uses extensive static analysis to aggressively optimize the
reference counts. Here the strong semantic foundation of &koka; helps a
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
be within 2x of the inefficiency of C/C++. Initial benchmarks are
encouraging and show &koka; to be close to C performance on various
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
fun map( xs : list<a>, f : a -> e b ) : e list<b>
  match xs
    Cons(x,xx) -> Cons( f(x), map(xx,f) )
    Nil        -> Nil
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
void map( list_t xs, function_t f, 
          list_t* res) 
{
  while (is_Cons(xs)) {
    if (is_unique(xs)) {    // if xs is not shared..
      box_t y = apply(dup(f),xs->head);      
      if (yielding()) { ... } // if f yields to a general ctl operation..
      else {
        xs->head = y;      
        *res = xs;          // update previous node in-place
        res = &xs->tail;    // set the result address for the next node
        xs = xs->tail;      // .. and continue with the next node
      }
    }
    else { ... }            // slow path allocates fresh nodes
  }
  *res = Nil;  
}
````

Moreover, the &koka; compiler also implements _tail-recursion modulo cons_ (TRMC) [@Leijen:trmc;@Leijen:trmc-tr]
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


[Learn more about FBIP &adown;](book.html#sec-fbip)
{.learn}

[Read the paper on fully in-place functional programming][fip-paper]
{.learn}

[Read the paper on generalized tail-recursion modulo cons][trmc-paper]
{.learn}


## Specialization

As another example of the effectiveness of Perceus and the strong semantics
of the Koka core language, we can
look at the [red-black tree][rbtree] example and look at the code generated 
when folding a binary tree. The red-black tree is defined as:
```
type color  
  Red
  Black

type tree<k,a> 
  Leaf
  Node(color : color, left : tree<k,a>, key : k, value : a, right : tree<k,a>)
```

We can generically fold over a tree `t` with a function `f` as:

```
fun fold(t : tree<k,a>, acc : b, f : (k, a, b) -> b) : b
  match t
    Node(_,l,k,v,r) -> r.fold( f(k,v,l.fold(acc,f)), f)
    Leaf            -> acc
```

This is used in the example to count all the `True` values in 
a tree `t : tree<k,bool>` as:

```unchecked
val count = t.fold(0, fn(k,v,acc) if v then acc+1 else acc)
```

This may look quite expensive where we pass a polymorphic 
first-class function that uses arbitrary precision integer arithmetic.
However, the Koka compiler first _specializes_ the `fold` definition 
to the passed function, then simplifies the resulting monomorphic code,
and finally applies Perceus to insert reference count instructions.
This results in the following internal core code:

```unchecked
fun spec-fold(t : tree<k,bool>, acc : int) : int
  match t
    Node(_,l,k,v,r) -> 
      if unique(t) then { drop(k); free(t) } else { dup(l); dup(r) } // perceus inserted
      val x = if v then 1 else 0
      spec-fold(r, spec-fold(l,acc) + x) 
    Leaf -> 
      drop(t)
      acc

val count = spec-fold(t,0)
```

When compiled via the C backend, the generated assembly instructions
on arm64 become:

````arm64
spec_fold:
  ...                       
  LOOP0:   
    mov  x21, x0              ; x20 is t, x21 = acc (x19 = koka context _ctx)
  LOOP1:                      ; the "match(t)" point
    cmp  x20, #9              ; is t a Leaf?
    b.eq LBB15_1              ;   if so, goto Leaf brach
  LBB15_5:                    ;   otherwise, this is the Node(_,l,k,v,r) branch
    mov  x23, x20             ; load the fields of t:
    ldp  x22, x0, [x20, #8]   ;   x22 = l, x0 = k   (ldp == load pair)
    ldp  x24, x20, [x20, #24] ;   x24 = v, x20 = r  
    ldr  w8, [x23, #4]        ;   w8 = reference count (0 is unique)
    cbnz w8, LBB15_11         ; if t is not unique, goto cold path to dup the members
    tbz  w0, #0, LBB15_13     ; if k is allocated (bit 0 is 0), goto cold path to free it
  LBB15_7:                   
    mov  x0, x23              ; call free(t)  
    bl   _mi_free
  LBB15_8:              
    mov  x0, x22              ; call spec_fold(l,acc,_ctx)
    mov  x1, x21              
    mov  x2, x19
    bl   spec_fold
    cmp  x24, #1              ; boxed value is False? 
    b.eq LOOP0                ;   if v is False, the result in x0 is the accumulator
    add  x21, x0, #4          ; otherwise add 1 (as a small int 4*n)
    orr  x8, x21, #1          ;   check for bigint or overflow in one test 
    cmp  x8, w21, sxtw        ;   (see kklib/include/integer.h for details)
    b.eq LOOP1                ; and tail-call into spec_fold if no overflow or bigint
    mov  w1, #5               ; otherwise, use generic bigint addition              
    mov  x2, x19
    bl   _kk_integer_add_generic
    b    LOOP0   
  ...
````

The polymorphic `fold` with its higher order parameter
is eventually compiled into a tight loop with close to optimal 
assembly instructions. 

~ advanced
Here we see too that the node `t` is freed explicitly as soon as it is
no longer live. This is usually earlier than scope-based deallocation 
(like RAII) and therefore Perceus can guarantee to be _garbage-free_ 
where in a (cycle-free) program objects are always immediatedly
deallocated as soon as they become unreachable [@Reinking:perceus;@Lorenzen:reuse-tr;@Lorenzen:fip;@Lorenzen:fip-tr].
Moreover, it is deterministic and behaves just like regular 
malloc/free calls.
Reference counting may still seem expensive compared to trace-based garbage collection
which only (re)visits all live objects and never needs to free objects
explicitly. However, Perceus usually frees an object right after its
last use (like in our example), and thus the memory is still in the
cache reducing the cost of freeing it. Also, Perceus never (re)visits
live objects arbitrarily which may trash the caches especially if the 
live set is large. As such, we think the deterministic behavior of 
Perceus together with the garbage-free property may work out better
in practice.
~

[Read the technical report on garbage-free and frame-limited reuse][reusetech]
{.learn}

[Read the technical report on fully in-place functional programming][fip-paper]
{.learn}
