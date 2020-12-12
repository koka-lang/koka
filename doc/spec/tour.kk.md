
~ MathDefs
\newcommand{\pdv}[1]{\frac{\partial{}}{\partial{#1}}}
~

~ hidden
```
import std/num/random
```
~

# A Tour of &koka; { #tour }

This is a short introduction to the &koka; programming language.

&koka; is a _function-oriented_ language that separates pure values from
side-effecting computations (The word 'k&omacron;ka' (or &#x52B9;&#x679C;) means
"effect" or "effective" in Japanese). &koka; is also
flexible and `fun`: &koka; has many features that help programmers to easily
change their data types and code organization correctly even in large-scale
programs, while having a small strongly-typed language core with a familiar
brace syntax.

## Basics { #sec-basics }

### Hello world

As usual, we start with the familiar _Hello world_ program:<span id=`examplemain`></span>
```
fun main() {
  println("Hello world!") // println output
}
```
&koka; uses familiar curly-braces syntax where `//` starts a line
comment. Functions are declared using the `fun` keyword (and anonymous functions with `fn`).

Here is another short example program that encodes a string using the
_Caesar cipher_, where each lower-case letter in a string is replaced by the letter
three places up in the alphabet:

```
fun main() { println(caesar("koka is fun")) }
////
fun encode( s : string, shift : int )
{
  fun encode-char(c) {
    if (c < 'a' || c > 'z') return c
    val base = (c - 'a').int
    val rot  = (base + shift) % 26
    (rot.char + 'a')
  }
  s.map(encode-char)
}

fun caesar( s : string ) : string {
  s.encode( 3 )
}
```

In this example, we declare a local function `encode-char` which encodes a
single character `c`. The final statement `s.map(encode-char)` applies the
`encode-char` function to each character in the string `s`, returning a
new string where each character is Caesar encoded. The result of the final
statement in a function is also the return value of that function, and you can
generally leave out an explicit `return` keyword.

### Dot selection {#sec-dot}

&koka; is a _function-oriented_ language where _functions_ and _data_ form the
core of the language (in contrast to objects for example). In particular, the
expression `s.encode(3)` does _not_ select the `encode` method from the
`:string` object, but it is simply syntactic sugar for the function call
`encode(s,3)` where `s` becomes the first argument. Similarly, `c.int`
converts a character to an integer by calling `int(c)` (and both expressions
are equivalent). The dot notation is intu&iuml;tive and quite convenient to
chain multiple calls together, as in:

```
fun showit( s : string ) { s.encode(3).count.println }
```

for example (where the body desugars as `println(length(encode(s,3)))`). An
advantage of the dot notation as syntactic sugar for function calls is that it
is easy to extend the 'primitive' methods of any data type: just write a new
function that takes that type as its first argument. In most object-oriented
languages one would need to add that method to the class definition itself
which is not always possible if such class came as a library for example.

### Type Inference

&koka; is also strongly typed. It uses a powerful type inference engine to
infer most types, and types generally do not get in the way. In
particular, you can always leave out the types of any local variables.
This is the case for example for ``base`` and ``rot`` values in the
previous example; hover with the mouse over the example to see the types
that were inferred by &koka;. Generally, it is good practice though to
write type annotations for function parameters and the function result
since it both helps with type inference, and it provides useful
documentation with better feedback from the compiler.

For the `encode` function it is actually essential to give the type of
the `s` parameter: since the `map` function is defined for both `:list`
and `:string` types and the program is ambiguous without an annotation.
Try to load the example in the editor and remove the annotation to see
what error &koka; produces.

### Anonymous Functions and Trailing Lambdas {#sec-anon}

&koka; also allows for anonymous function expressions using the `fn` keyword.
For example, instead of
declaring the `encode-char` function, we can also pass it directly to
the `map` function as a function expression:

```
fun encode2( s : string, shift : int )
{
  s.map( fn(c) {
    if (c < 'a' || c > 'z') return c
    val base = (c - 'a').int
    val rot  = (base + shift) % 26
    (rot.char + 'a')
  });
}
```

It is a bit annoying we had to put the final right-parenthesis after the last
brace in the previous example. As a convenience, &koka; allows anonymous functions to _follow_
the function call instead -- this is also known as _trailing lambdas_. 
For example, here is how we can print the numbers
``1`` to ``10``:

```
fun main() { print10() }
////
fun print10()
{
  for(1,10) fn(i) {
    println(i)
  }
}
```

which is desugared to `for( 1, 10, fn(i){ println(i) } )`. (In fact, since we
pass the `i` argument directly to `println`, we could have also passed the function itself
directly, and write `for(1,10,println)`.)

Anonymous functions without any arguments can be shortened further by leaving
out the `fn` keyword as well and just use braces directly. Here is an example using
the `repeat` function:

```
fun main() { printhi10() }
////
fun printhi10()
{
  repeat(10) {
    println("hi")
  }
}
```

where the body desugars to `repeat( 10, fn(){println(``hi``)} )`. The is
especially convenient for the `while` loop since this is not a built-in
control flow construct but just a regular function:

```
fun main() { print11() }
////
fun print11() {
  var i := 10
  while { i >= 0 } {
    println(i)
    i := i - 1
  }
}
```

Note how the first argument to `while` is in braces instead of the usual
parenthesis. In &koka;, an expression between _parenthesis_ is always evaluated
before a function call, whereas an expression between _braces_ (ah,
_suspenders_!) is suspended and may be never evaluated or more than once
(as in our example). 


### With Statements { #sec-with; }

To the best of our knowledge, &koka; was the first language to have
generalized _dot notation_ and _trailing lambdas_. Another novel 
syntactical feature is the `with` statement.
With the ease of passing a function block as a parameter, these
often become nested. For example:
```
fun twice(f) {
  f()
  f()
}

fun test-twice() {
  twice( fn(){
    twice( fn(){
      println("hi")
    })
  })
}
```
where `"hi"` is printed four times. Using the `with` statement 
we can write this more concisely as:
```
public fun test-with1() {
  with twice
  with twice
  println("hi")
}
```

The `with` statement essentially puts all statements that follow it into 
an anynomous function block and passes that as the last parameter. In general:

~ translate
```unchecked
with f(e1,...,eN)
<body>
```
&mapsto;
```unchecked
f(e1,...,eN, fn(){ <body> })
```
~

Moreover, a `with` statement can also bind a variable parameter as:

~ translate
```unchecked
with x = f(e1,...,eN)
<body>
```
&mapsto;
```unchecked
f(e1,...,eN, fn(x){ <body> })
```
~

Here is an examply using `foreach` to span over the rest of the function body:

```
public fun test-with2() {
  with x = list(1,10).foreach
  println(x)
}
```

which desugars to `list(1,10).foreach( fn(x){ println(x) } )`. 
This is a bit reminiscent of Haskell ``do`` notation. 
Using the `with` statement this way may look a bit strange at first
but is very convenient in practice -- it helps thinking of `with` as
a closure over the rest of the lexical scope. 

#### With Finally

As another example, the `finally` function takes as it first argument a
function that is run when exiting the scope -- either normally, 
or through an "exception" (&ie; when an effect operation does not resume).
Again, `with` is a natural fit:

```
fun test-finally() {
  with finally{ println("exiting..") }
  println("entering..")
  throw("oops") + 42
}
```
which desugars to `finally(fn(){ println(...) }), fn(){ println("entering"); throw("oops") + 42 })`,
and prints:

````
entering..
exiting..
uncaught exception: oops
````

This is another example of the _min-gen_ principle: many languages have
have special built-in support for this kind of pattern, like a ``defer`` statement, but in &koka;
it is all just function applications with minimal syntactic sugar.


#### With Handlers  { #sec-with-handlers; }

The `with` statement is especially useful in combination with 
effect handlers. Generally, a `handler{ <ops> }` expression takes 
as its last argument a function block so it can be used directly with `with`. 
Here is an example of an effect handler for emitting messages:
```
effect fun emit(msg : string) : ()

fun hello() {
  emit("hello world!")
}

public fun emit-console1() {
  with handler{ fun emit(msg){ println(msg) } }
  hello()
}
```
In this example, the `with` desugars to `(handler{ fun emit(msg){ println(msg) })( fn(){ hello() } )`.

Moreover, as a convenience, we can leave out the `handler` keyword 
for effects that define just one operation (like `:emit`):

~ translate
```unchecked
with val op = <expr> 
with fun op(x){ <body> }
with except op(x){ <body> }
with control op(x){ <body> }
```
&mapsto;
```unchecked
with handler{ val op = <expr> }
with handler{ fun op(x){ <body> } }
with handler{ except op(x){ <body> } }
with handler{ control op(x){ <body> } }
```
~

Using this, we can write the previous example in a more concise and natural way as:

```
public fun emit-console2() {
  with fun emit(msg){ println(msg) }
  hello()
}
```

Intuitively, we can view the handler `with fun emit` as a dynamic binding of the function `emit`
over the rest of the scope.

[Read more about effect handlers][#sec-handlers]
{.learn}

[Read more about `val` operations][#sec-opval]
{.learn}


### Optional and Named Parameters  { #sec-default; }

Being a function-oriented language, &koka; has powerful support for function
calls where it supports both optional and named parameters. For example, the
function `replace-all` takes a string, a pattern (named ``pattern``), and
a replacement string (named ``repl``):

```
fun main() { println(world()) }
////
fun world() {
  replace-all("hi there", "there", "world")  // returns "hi world"
}
```

Using named parameters, we can also write the function call as:

```
fun main() { println(world2()) }
////
fun world2() {
  "hi there".replace-all( repl="world", pattern="there" )
}
```

Optional parameters let you specify default values for parameters that do not
need to be provided at a call-site.  As an example, let's define a function
`sublist` that takes a list, a ``start`` position, and the length ``len`` of the desired
sublist.  We can make the ``len`` parameter optional and by default return all
elements following the ``start`` position by picking the length of the input list by
default:

```
fun main() { println( ['a','b','c'].sublist(1).string ) }
////
fun sublist( xs : list<a>, start : int, len : int = xs.length ) : list<a> {
  if (start <= 0) return xs.take(len)
  match(xs) {
    Nil        -> Nil
    Cons(_,xx) -> xx.sublist(start - 1, len)
  }
}
```

Hover over the `sublist` identifier to see its full type, where the ``len``
parameter has gotten an optional `:int` type signified by the question mark:
`:?int`.


### A larger example: cracking Caesar encoding


```
fun main() { test-uncaesar() }

fun encode( s : string, shift : int )
{
  function encode-char(c) {
    if (c < 'a' || c > 'z') return c
    val base = (c - 'a').int
    val rot  = (base + shift) % 26
    (rot.char + 'a')
  }

  s.map(encode-char)
}
////
// The letter frequency table for English
val english = [8.2,1.5,2.8,4.3,12.7,2.2,
               2.0,6.1,7.0,0.2,0.8,4.0,2.4,
               6.7,7.5,1.9,0.1, 6.0,6.3,9.1,
               2.8,1.0,2.4,0.2,2.0,0.1]

// Small helper functions
fun percent( n : int, m : int ) {
  100.0 * (n.double / m.double)
}

fun rotate( xs, n ) {
  xs.drop(n) + xs.take(n)
}

// Calculate a frequency table for a string
fun freqs( s : string ) : list<double> {
  val lowers = list('a','z')
  val occurs = lowers.map( fn(c){ s.count(c.string) })
  val total  = occurs.sum
  occurs.map( fn(i){ percent(i,total) } )
}

// Calculate how well two frequency tables match according
// to the _chi-square_ statistic.
fun chisqr( xs : list<double>, ys : list<double> ) : double {
  zipwith(xs,ys, fn(x,y){ ((x - y)^2.0)/y } ).foldr(0.0,(+))
}

// Crack a Caesar encoded string
fun uncaesar( s : string ) : string {
  val table  = freqs(s)                   // build a frequency table for `s`
  val chitab = list(0,25).map( fn(n) {    // build a list of chisqr numbers for each shift between 0 and 25
                  chisqr( table.rotate(n), english )
               })
  val min    = chitab.minimum()           // find the mininal element
  val shift  = chitab.index-of( fn(f){ f == min } ).negate  // and use its position as our shift
  s.encode( shift )
}

fun test-uncaesar() {
  println( uncaesar( "nrnd lv d ixq odqjxdjh" ) )
}
```

The `val` keyword declares a static value. In the example, the value `english`
is a list of floating point numbers (of type `:double `) denoting the average
frequency for each letter. The function `freqs` builds a frequency table for a
specific string, while the function `chisqr` calculates how well two frequency
tables match. In the function `crack` these functions are used to find a
`shift` value that results in a string whose frequency table matches the
`english` one the closest -- and we use that to decode the string. 
You can try out this example directly in the interactive environment:
````
> :l samples/basic/caesar.kk
````


## Effect types

A novel part about &koka; is that it automatically infers all the _side effects_
that occur in a function. The absence of any effect is denoted as `:total` (or
`<>`) and corresponds to pure mathematical functions. If a function can raise
an exception the effect is `:exn`, and if a function may not terminate the
effect is `:div` (for divergence). The combination of `:exn` and `:div` is
`:pure` and corresponds directly to Haskell's notion of purity. Non-
deterministic functions get the `:ndet` effect. The 'worst' effect is `:io`
and means that a program can raise exceptions, not terminate, be non-
deterministic, read and write to the heap, and do any input/output operations.
Here are some examples of effectful functions:

```
fun square1( x : int ) : total int   { x*x }
fun square2( x : int ) : console int { println( "a not so secret side-effect" ); x*x }
fun square3( x : int ) : div int     { x * square3( x ) }
fun square4( x : int ) : exn int     { throw( "oops" ); x*x }
```

When the effect is `:total` we usually leave it out in the type annotation.
For example, when we write:

```
fun square5( x : int ) : int { x*x }
```

the assumed effect is `:total`. Sometimes, we write an effectful
function, but are not interested in explicitly writing down its effect type.
In that case, we can use a _wildcard type_ which stands for some inferred
type. A wildcard type is denoted by writing an identifier prefixed with an
underscore, or even just an underscore by itself:

```
fun square6( x : int ) : _e int {
  println("I did not want to write down the \"console\" effect")
  x*x
}
```

Hover over `square6` to see the inferred effect for `:_e`

### Semantics of effects

The inferred effects are not just considered as some extra type information on
functions. On the contrary, through the inference of effects, &koka; has a very
strong connection to its denotational semantics. In particular, _the full type
of a &koka; functions corresponds directly to the type signature of the
mathematical function that describes its denotational semantics_. For example,
using &#x301A;`:t`&#x301B; to translate a type `:t` into its corresponding
mathematical type signature, we have:

|~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~| ~~~~~~~~~~~~~~| ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~|
|&#x301A;`:int -> total int `&#x301B;        | =&nbsp;&nbsp; | $\mathbb{Z} \rightarrow \mathbb{Z}$                              |
|&#x301A;`:int -> exn int `&#x301B;          | =             | $\mathbb{Z} \rightarrow (\mathbb{Z} + 1)$ |
|&#x301A;`:int -> pure int `&#x301B;         | =             | $\mathbb{Z} \rightarrow (\mathbb{Z} + 1)_\bot$ |
|&#x301A;`:int -> <st<h>,pure> int `&#x301B; | =             | $(\mathbb{Z} \times \mathbb{H}) \rightarrow ((\mathbb{Z} + 1) \times \mathbb{H})_\bot$ |

In the above translation, we use $1 + \tau$ as a sum
where we have either a unit $1$ (i.e. exception) or a type $\tau$, and we use
$\mathbb{H}\times \tau$ for a product consisting of a pair of a
heap and a type $\tau$. From the above correspondence, we can immediately see that
a `:total` function is truly total in the mathematical sense, while a stateful
function (`:st<h> `) that can raise exceptions or not terminate (`:pure`)
takes an implicit heap parameter, and either does not terminate ($\bot$) or
returns an updated heap together with either a value or an exception ($1$).

We believe that this semantic correspondence is the true power of full effect
types and it enables effective equational reasoning about the code by a
programmer. For almost all other existing programming languages, even the most
basic semantics immediately include complex effects like heap manipulation and
divergence. In contrast, &koka; allows a layered semantics where we can easily
separate out nicely behaved parts, which is essential for many domains, like
safe LINQ queries, parallel tasks, tier-splitting, sand-boxed mobile code,
etc.


### Combining effects

Often, a function contains multiple effects, for example:

```
fun combine-effects() {
  val i = srandom-int() // non-deterministic
  throw("oops")         // exception raising
  combine-effects()     // and non-terminating
}
```

The effect assigned to `combine-effects` are `:ndet`, `:div`, and `:exn`. We
can write such combination as a _row_ of effects as `: <div,exn,ndet> `. When
you hover over the `combine-effects` identifiers, you will see that the type
inferred is really `: <pure,ndet> ` where `:pure` is a type alias defined as

```unchecked
alias pure = <div,exn>
```


### Polymorphic effects

Many functions are polymorphic in their effect. For example, the
`map:forall<a,b,e> (xs : list<a>, f : (a) -> e b) -> e list<b> ` function
applies a function `f` to each element of a (finite) list. As such, the effect
depends on the effect of `f`, and the type of `map` becomes:

```unchecked
map : (xs : list<a>, f : (a) -> e b) -> e list<b>
```

We use single letters (possibly followed by digits) for polymorphic types.
Here, the `map` functions takes a list with elements of some type `:a`, and a
function ``f`` that takes an element of type `:a` and returns a new element of
type `:b`. The final result is a list with elements of type `:b`. Moreover,
the effect of the applied function `:e` is also the effect of the `map`
function itself; indeed, this function has no other effect by itself since it
does not diverge, nor raises exceptions.

We can use the notation `: <l|e>` to extend an effect `:e` with another effect
`:l`. This is used for example in the `while` function which has type:
`while : ( pred : () -> <div|e> bool, action : () -> <div|e> () ) -> <div|e> ()`.
The `while` function takes a
predicate function and an action to perform, both with effect `: <div|e>`.
Indeed, since while may diverge depending on the predicate its effect must
include divergence.

The reader may be worried that the type of `while` forces the predicate and
action to have exactly the same effect `: <div|e>`, which even includes
divergence. However, when effects are inferred at the call-site, both the
effects of predicate and action are extended automatically until they match.
This ensures we take the union of the effects in the predicate and action.
Take for example the following loop

```
fun looptest() {
  while { is-odd(srandom-int()) } {
    throw("odd")
  }
}
```

&koka; infers that the predicate ``odd(srandom-int())`` has
effect `: <ndet|e1> ` while the action has effect `: <exn|e2> ` for some `:e1` and `:e2`.
When applying `while`, those
effects are unified to the type `: <exn,ndet,div|e3> ` for some `:e3`.

### Local Mutable Variables  { #sec-var; }

The Fibonacci numbers are a sequence where each subsequent Fibonacci number is
the sum of the previous two, where `fib(0) == 0` and `fib(1) == 1`. We can
easily calculate Fibonacci numbers using a recursive function:

```
fun main() { println(fib(10)) }
////
fun fib(n : int) : div int {
  if (n <= 0)   then 0
  elif (n == 1) then 1
  else fib(n - 1) + fib(n - 2)
}
```

Note that the type inference engine is currently not powerful enough to
prove that this recursive function always terminates, which leads to
inclusion of the divergence effect `:div` in the result type.


Here is another version of the Fibonacci function but this time
implemented using local mutable variables. 
We use the `repeat` function to iterate `n` times:

```
fun main() { println(fib2(10)) }
////
fun fib2(n) {
  var x := 0
  var y := 1
  repeat(n) {
    val y0 = y
    y := x+y
    x := y0
  }
  x
}
```
In contrast to a `val` declaration that binds an immutable value (as in `val y0 = y`),
a `var` declaration declares a _mutable_ variable, where the `(:=)` operator 
can assign a new value to the variable. Internally, the `var` declarations use 
a _state_ effect handler which ensures
that the state has the proper semantics even if resuming multiple times.

<!--
However, that also means that mutable local variables are not quite first-class
and we cannot pass them as parameters to other functions for example (as they
are always dereferenced). You will also get a type error if a local variable
escapes through a function expression, for example:
```unchecked
fun wrong() : (() -> console ()) {
  var x := 1
  (fn(){ x := x + 1; println(x) })
}
```
is statically rejected as the reference to the local variable escapes its scope.
-->

[Read more about state and multiple resumptions][#sec-multi-resume]
{.learn}


### Reference Cells and Isolated state {#sec-runst}

&koka; also has heap allocated mutable reference cells. 
A reference to an
integer is allocated using `val r = ref(0)` (since the reference itself is
actually a value!), and can be dereferenced using the bang operator, as ``!r``.
We can write the Fibonacci function using reference cells as:

```
fun main() { println(fib3(10)) }
////
fun fib3(n) {
  val x = ref(0)
  val y = ref(1)
  repeat(n) {
    val y0 = !y
    y := !x + !y
    x := y0
  }
  return !x
}
```

As we can see, using `var` declarations are generally preferred as these 
behave better under multiple resumptions, but also are syntactically more
concise as they do not need a dereferencing operator. (Nevertheless, we 
still need reference cells as those are first-class while `var` variables
cannot be passed to other functions.)

When we look at the types inferred for the references, we see that `x` and `y`
have type `:ref<h,int> ` which stands for a reference to a mutable value of
type `:int` in some heap `:h`. The effects on heaps are allocation as
`:heap<h>`, reading from a heap as `:read<h>` and writing to a heap as
`:write<h>`. The combination of these effects is called stateful and denoted
with the alias `:st<h>`.

Clearly, the effect of the body of `fib3` is `:st<h> `; but when we hover over
`fib3`, we see the type inferred is actually the `:total` effect: `:(n:int) -> int`.
Indeed, even though `fib3` is stateful inside, its side-effects can
never be observed. It turns out that we can safely discard the `:st<h> `
effect whenever the heap type `:h` cannot be referenced outside this function,
i.e. it is not part of an argument or return type. More formally, the &koka;
compiler proves this by showing that a function is fully polymorphic in the
heap type `:h` and applies the `run` function (corresponding to ``runST`` in
Haskell) to discard the `:st<h> ` effect.

The Garsia-Wachs algorithm is nice example where side-effects are used
internally across function definitions and data structures, but where the
final algorithm itself behaves like a pure function, see the
[``samples/basic/garsia-wachs.kk``][garsia-wachs].

[garsia-wachs]: https://github.com/koka-lang/koka/tree/master/samples/basic/garsia-wachs.kk {target='_top'}


## Data Types


### Structs

An important aspect of a function-oriented language is to be able to define
rich data types over which the functions work. A common data type is that of a
_struct_ or _record_. Here is an example of a struct that contains information
about a person:

```
struct person { 
  age : int
  name : string
  realname : string = name
}

val brian = Person( 29, "Brian" )
```

Every `struct` (and other data types) come with constructor functions to
create instances, as in `Person(19,"Brian")`. Moreover, these
constructors can use named arguments so we can also call the constructor
as `Person( name = "Brian", age = 19, realname = "Brian H. Griffin" )`
which is quite close to regular record syntax but without any special rules;
it is just functions all the way down!

Also, &koka; automatically generates accessor functions for each field in a
struct (or other data type), and we can access the `age` of a `:person` as
`brian.age` (which is of course just syntactic sugar for `age(brian)`).

### Copying

By default, all structs (and other data types) are _immutable_. Instead of
directly mutating a field in a struct, we usually return a new struct where
the fields are updated. For example, here is a `birthday` function that
increments the `age` field:

```
fun main() { println( brian.birthday.age ) }

struct person { 
  age : int
  name : string
  realname : string = name 
}

val brian = Person( 29, "Brian" )
////
fun birthday( p : person ) : person {
  p( age = p.age + 1 )
}
```

Here, `birthday` returns a fresh `:person` which is equal to `p` but with the
`age` incremented. The syntax ``p(...)`` is syntactic sugar for calling the copy constructor of
a `:person`. This constructor is also automatically generated for each data
type, and is internally generated as:

```
fun main() { println( brian.copy().age ) }

struct person( age : int, name : string, realname : string = name )

val brian = Person( 29, "Brian" )
////
fun copy( p, age = p.age, name = p.name, realname = p.realname ) {
  return Person(age, name, realname)
}
```

When arguments follow a data value, as in ``p( age = age + 1)``, it is expanded to call this
copy function, as in `p.copy( age = p.age+1 )`. In adherence with the _min-gen_ principle,
there are no special rules for record updates but using plain function calls with optional
and named parameters.

### Alternatives (or Unions)  { #sec-union; }

&koka; also supports algebraic data types where there are multiple alternatives.
For example, here is an enumeration:

```unchecked
type colors {
  Red
  Green
  Blue
}
```

Special cases of these enumerated types are the `:void` type which has no
alternatives (and therefore there exists no value with this type), the unit
type `:()` which has just one constructor, also written as `()` (and
therefore, there exists only one value with the type `:()`, namely `()`), and
finally the boolean type `:bool` with two constructors `True` and `False`.

```unchecked
type void
type () {
  ()
}
type bool {
  False
  True
}
```

Constructors can have parameters. For example, here is how to create a
`:number` type which is either an integer or the infinity value:

```unchecked
type number {
  Infinity
  Integer( i : int )
}
```

We can create such number by writing `Integer(1)` or `Infinity`. Moreover,
data types can be polymorphic and recursive. Here is the definition of the
`:list` type which is either empty (`Nil`) or is a head element followed by a
tail list (`Cons`):

```unchecked
type list<a> {
  Nil
  Cons{ head : a; tail : list<a> }
}
```

&koka; automatically generates accessor functions for each named parameter. For
lists for example, we can access the head of a list as `Cons(1,Nil).head`.

We can now also see that `struct` types are just syntactic sugar for regular a
`type` with a single constructor of the same name as the type:

~ translate
```unchecked
struct tp { <fields> }
```
&mapsto;
```unchecked
type tp { 
  con Tp { <fields> }
}
```
~

For example,
our earlier `:person` struct, defined as

```unchecked
struct person{ age : int; name : string; realname : string = name }
```

desugars to:

```unchecked
type person {
  Person{ age : int; name : string; realname : string = name }
}
```

### Matching

Todo

### Extensible Data Types

Todo

### Inductive, Co-inductive, and Recursive Types

For the purposes of equational reasoning and termination checking, a `type`
declaration is limited to finite inductive types. There are two more
declarations, namely `co type` and `rec type` that allow for co-inductive types,
and arbitrary recursive types respectively.


### Value Types

Value types are (non-recursive) data types that are not heap allocated
but passed on the stack as a value. Since data types are immutable, semantically
these types are equivalent but value types can be more efficient as they
avoid heap allocation and reference counting (or more expensive as they need copying 
instead of sharing a reference). 

By default, any non-recursive inductive datatype of a size upto 3 machine words (= 24 bytes 
on a 64-bit platform) is treated as a value type. For example, tuples and 3-tuples
are passed and returned by value. Usually, that means that such tuples are for
example returned in registers when compiling with optimization.

We can also force a type to be compiled as a value type by using the `value` keyword
in front of a `type` or `struct` declaration:
```
value struct argb{ alpha: int; red: int; green: int; blue: int }
```

~ begin advanced

#### Boxing

To support generic polymorphism, sometimes value types are _boxed_. For example, a list
is polymorpic in its elements. That means that if we construct a list of tuples, like
`[(1,True)]`, that the element `(1,2)` will be boxed and heap allocated -- essentially 
the compiler transforms this expression into `[Box((1,True)]` internally.

Note that for regular data types and `:int`'s boxing is free (as in isomorphic). Moreover, value types
up to 63 bits (on a 64-bit platform) are boxed in-place and do not require heap allocation
(like `:int32`). The `:double` type is also specialized; by default the Koka compiler
only heap allocates negative doubles for boxing while positive doubles are boxed in-place.
(this can be configured though to only heap allocate doubles for boxing when their absolute value is
outside of the range 2^-511^ up to 2^512^).

For performance sensitive code we may specialize certain polymorphic datatypes to
reduce allocations due to boxing. For example:

```
type mylist{ 
  con MyCons{ head1: int; head2: bool; mytail: mylist }
  con MyNil
}
```

Our previous example becomes `MyCons(1,True,MyNil)` now and is more efficient as it only needs
one allocation for the `MyCons` without an indirection to a tuple.
In the future we hope to extend Koka to perform specialization automatically or by
using special directives.

~ end advanced


## Effect Handlers { #sec-handlers }

Effect handlers [@Pretnar:handlers;@Leijen:algeff] are a novel way to 
define control-flow abstractions and dynamic binding as user defined 
handlers -- no need anymore to add special compiler extensions for
exceptions, iterators, async-await, probabilistic programming, etc. 
Moreover, these handlers can be composed freely so the interaction between,
say, async-await and exceptions as well-defined. 

### Handling 

Let's start with defining an exception effect of our own. The `effect`
declaration defines a new type together with _operations_, for now
we use a `control` operation:
```
effect raise {
  control raise( msg : string ) : a
}
```

This defines an effect type `:raise` together with an operation
`raise` of type `:(msg : string) -> raise a`. With the effect signature 
declared, we can already use the operations:
```
fun safe-divide( x : int, y : int ) : raise int {
  if (y==0) then raise("div-by-zero") else x / y
}
```
where we see that the `safe-divide` function gets the `:raise` effect
(since we use the `raise` operation in the body). Such an effect
type means that we can only evaluate the function in a context
where `:raise` is _handled_ (in other words, where it is "dynamically bound", or
where we &ldquo;have the `:raise` capability&rdquo;).

We can _handle_ the effect by giving a concrete definition for the `raise` operation.
For example, we may always return a default value:
```
fun raise-const() : int {
  with handler {
    control raise(msg){ 42 }
  } 
  8 + safe-divide(1,0)
}
```
The call `raise-const()` evaluates to `42` (not `50`).
When a `raise` is called (in `safe-divide`), it will _yield_ to its innermost handler, unwind
the stack, and only then evaluate the operation definition -- in this case just directly
returning `42` from the point where the handler is defined. 
Now we can see why it is called a _control_
operation as `raise` changes the regular linear control-flow and yields right 
back to its innermost handler from the original call site. 
Also note that `raise-const` is `:total` again and the handler discharged the
`:raise` effect.

The `handler{ <ops> }` expression is a function that itself expects a function 
argument over which the handler is scoped, as in `(handler{ <ops> })(action)`.
This works well in combination with the `with` statement of course.
As a syntactic convenience, for single operations we can leave out the `handler` keyword 
which is translated as:
~ translate
```unchecked
with control op(<args>){ <body> }
```
&mapsto;
```unchecked
with handler { 
  control op(<args>){ <body> }
}
```
~

With this translation, we can write the previous example more concisely as:

```
fun raise-const1() : int {
  with control raise(msg){ 42 }
  8 + safe-divide(1,0)
}
```

which eventually expands to `(handler{ control raise(msg){ 42 } })(fn(){ 8 + safe-divide(1,0) })`.

We have a similar syntactic convenience for effects with one operation where the 
name of the effect and operation are the same. We can define such an effect by just declaring 
its operation which implicitly declares an effect type of the same name:
~ translate
```unchecked
effect control op(<parameters>) : <result-type>
```
&mapsto;
```unchecked
effect op {
  control op(<parameters>) : <result-type>
}
```
~

That means we can declare our `:raise` effect signature also more concisely as:
```unchecked
effect control raise( msg : string ) : a
```

&bigskip;

[Read more about the `with` statement][#sec-with]
{.learn}


### Resuming  { #sec-resume; }

The power of effect handlers is not just that we can _yield_ to the innermost
handler, but that we can also _resume_ back to the call site with a result.

Let's define a `:ask<a>` effect that allows us to get a contextual value of type `:a`:

```
effect ask<a> {                  // or: effect<a> control ask() : a
  control ask() : a
}

fun add-twice() : ask<int> int {
  ask() + ask()
}
```

The `add-twice` function can ask for numbers but it is unaware of how these
are provided -- the effect signature just specifies an contextual API.
We can handle it by always resuming with a constant for example:

```
fun ask-const() : int {
  with control ask(){ resume(21) }
  add-twice()
}
```

where `ask-const()` evaluates to `42`. Or by returning random values, like:

```
fun ask-random() : random int {
  with control ask(){ resume(random-int()) }
  add-twice()
}
```

where `ask-random()` now handled the `:ask<int>` effect, but itself now has
`:random` effect (see `module std/num/random`).
The `resume` function is implicitly bound by a `control` operation and resumes
back to the call-site with the given result. 

As we saw in the exception example, we do
not need to call `resume` and can also directly return into our handler scope. For example, we 
may only want to handle a `ask` once, but after that give up:

```
fun ask-once() : int {
  var count := 0
  with control ask() { 
    count := count + 1
    if (count <= 1) then resume(42) else 0 
  }
  add-twice()
}
```

Here `ask-once()` evaluates to `0` since the second call to `ask` does not resume,
(and returns directly `0` in the `ask-once` context). This pattern can for example
be used to implement the concept of _fuel_ in a setting where a computation is 
only allowed to take a limited amount of steps.


[Read more about `var` mutable variables][#sec-var]
{.learn}


### Tail-Resumptive Operations { #sec-opfun; }

A `control` operation is one of the most general ways to define operations since
we get a first-class `resume` function. However, almost all operations in practice turn out
to be _tail-resumptive_: that is, they resume exactly _once_ with their final result
value. To make this more convenient, we can declare `fun` operations that do this
by construction, &ie;
~ translate
```unchecked
with fun op(<args>){ <body> }
```
&mapsto;
```unchecked
with control op(<args>){ val f = fn(){ <body> }; resume( f() ) }
```
~

(The translation is defined via an intermediate function so `return` works correctly).

With this syntactic sugar, we can write our earlier `ask-const` example 
using a `fun` operation instead:

```
fun ask-const2() : int {
  with fun ask(){ 21 }
  add-twice()
}
```

This also conveys better that even though `ask` is dynamically bound, it behaves
just like a regular function without changing the control-flow. 

Moreover, operations declared as `fun` are much more efficient than general
`control` operations. The &koka; compiler uses (generalized) _evidence translation_  [@Xie:evidently]
to pass down handler information to each call-site. At the call to `ask` in `add-twice`,
it selects the handler from the evidence vector and when the operation is
a tail-resumptive `fun`, it calls it directly as a regular function (except with an adjucted evidence
vector for its context). Unlike a general `control` operation, there is no need to yield upward
to the handler, capture the stack, and eventually resume again. 
This gives `fun` (and `val`) operations a performance cost very similar to _virtual method calls_ 
which can be very efficient.

For even a bit more performance, you can also declare upfront that any operation
definition must be tail-resumptive, as:

```unchecked
effect ask<a> {
  fun ask() : a
}
```

This restricts all handler definitions for the `:ask` effect to use `fun` definitions
for the `ask` operation. However, it increases the ability to reason about the code,
and the compiler can optimize such calls a bit more as it no longer needs to check at
run-time if the handler happens to define the operation as tail-resumptive.


#### Value Operations { #sec-opval; }

A common subset of operations always tail-resume with a single value; these are
essentially dynamically bound variables (but statically typed!). Such operations
can be declared as a `val` with the following translation:

~ translate
```unchecked
with val v = <expr>
```
&mapsto;
```unchecked
val x = <expr>
with fun v(){ x }
```
&mapsto;
```unchecked
val x = <expr>
with control v(){ resume(x) }
```
~

For an example of the use of value operations, consider a 
pretty printer that produces pretty strings from documents:

~ hidden
```
alias doc = string
fun pretty( d : doc ) : width string {
  d.truncate(width)
}
```
~

```unchecked
fun pretty( d : doc ) : string
```

Unfortunately, it has a hard-coded maximum display width of `40` deep
down in the code of `pretty`:

```unchecked
fun pretty-internal( line : string ) : string {
  line.truncate(40)
}
```

To abstract over the width we have a couple of choices: we 
could make the width a regular parameter but now we need to 
explicitly add the parameter to all functions in the library
and manually thread them around. Another option is a global
mutable variable but that leaks side-effects and is non-modular.

Or, we can define it as a value operation instead:

```
effect val width : int
```

This also allows us to refer to the `width` operation as if is a 
regular value (even though internally it invokes the operation).
So, the check for the width in the pretty printer can be written as:

```
fun pretty-internal( line : string ) : width string {
  line.truncate(width)
}
```

When using the pretty printer we can bind the `width` as a
regular effect handler:

```
fun pretty-thin(d : doc) : string {
  with val width = 40
  pretty(d)
}
```

Note that we did not need to change the structure of the 
original library functions. However the types of the functions
still change to include the `:width` effect as these now
require the `width` value to be handled at some point.
For example, the type of `pretty` becomes:
```unchecked
fun pretty( d : doc ) : width string
```
as is requires the `:width` effect to be handled (aka,
the "dynamic binding for `width : int` to be defined", 
aka, the "`:width` capability").


### Abstracting Handlers

As another example, a _writer_ effect is quite common where
values are collected by a handler. For example, we can
define an `:emit` effect to emit messages:
```unchecked
effect fun emit( msg : string ) : ()
```
```
fun ehello() : emit () {
  emit("hello")
  emit("world")
}
```

We can define for example a handler that prints the 
emitted messages directly to the console:

```
fun ehello-console() : console () {
  with fun emit(msg){ println(msg) }
  ehello()
}
```

Here the handler is define directly, but we can also abstract the handler for
emitting to the console into a separate function:

```
fun emit-console( action ) {
  with fun emit(msg){ println(msg) }
  action()
} 
```

where `emit-console` has the inferred type `:(action : () -> <emit,console|e> a) -> <console|e> a` (hover 
over the source to see the inferred types) where
the action can have use the effects `:emit`, `:console`, and any other effects `:e`, 
and where the final effect is just `: <console|e>` as the `:emit` effect
is discharged by the handler.

Note, we could have written the above too as:
```
val emit-console2 = handler {
  fun emit(msg){ println(msg) }
}
```
since a `handler{ ... }` expression is a function itself (and thus a _value_). 
Generally we prefer the earlier definition though as it allows further parameters
like an initial state.

Since `with` works generally, we can use the abstracted handlers just like
regular handlers, and our earlier example can be written as:

```
fun ehello-console2() : console () {
  with emit-console
  ehello()
}
```
(which expands to `emit-console( fn(){ ehello() } )`).
Another useful handler may collect all emitted messages as a list of lines:

```
fun emit-collect( action : () -> <emit|e> () ) : e string {
  var lines := []
  with handler {
    return(x){ lines.reverse.join("\n") }
    fun emit(msg){ lines := Cons(msg,lines) }
  }
  action()
}

fun ehello-commit() : string {
  with emit-collect
  ehello()
}
```

This is a total handler and only discharges the `:emit` effect.

[Read more about the `with` statement][#sec-with]
{.learn}

[Read more about `var` mutable variables][#sec-var]
{.learn}

As another example, consider a generic `catch` handler that
applies an handling function when `:raise` is called on our
exception example:

```
fun catch( hnd : (string) -> e a, action : () -> <raise|e> a ) : e a {
  with control raise(msg){ hnd(msg) }
  action()
}
```

We can use it now conveniently with a `with` statement to handle
exceptional situations:

```
fun catch-example() {
  with catch fn(msg){ println("error: " + msg); 42 }
  safe-divide(1,0)
}
```

~ advanced
The `catch` handler has an interesting type where the action can
have a `:raise` effect (`: () -> <raise|e> a`) and maybe further effects `:e`,
while the handling function `hnd` only has effect `:e`. Now consider
supplying a handing function that itself calls `raise`: in that case, the 
type of `catch` would be instantiated to: `: (hnd: (string) -> <raise> a, action : () -> <raise, raise> a ) : <raise> a`.
This is correct: the (outer) `:raise` effect of `action` is handled and discharged, but since
the handling function `hnd` can still cause `raise` to be called, the final effect still contains `:raise`.

Here we see that &koka; allows _duplicate_ effect labels [@Leijen:scopedlabels] where `action` has
an instantiated `: <raise,raise>` effect type.
These kind of types occur naturally in the presence of polymorphic effects, and there is a natural correspondence
to the structure of the evidence vectors at runtime (with entries for each nested effect handler). 
Intuitively, the `action` effect expresses that 
its outer (left-most) `:raise` is handled, but that there may be other exceptions that are not handled -- in this 
case from the handling function `hnd`, but they can also be _masked_ exceptions (as described in Section [#sec-mask]).
~


### Return Operations { #sec-return; }

In the previous `emit-collect` example we saw the use of 
a `return` operation. Such operation changes the final
result of the action of a handler. 
For example, consider our earlier used-defined exception effect `:raise`.
We can define a general handler that transforms any exceptional
action into one that returns a `:maybe` type:

```
fun raise-maybe( action : () -> <raise|e> a ) : e maybe<a> {
  with handler {
    return(x){ Just(x) }           // normal return: wrap in Just
    control raise(msg){ Nothing }  // exception: return Nothing directly
  }
  action()
}

fun div42() {
  (raise-maybe{ safe-divide(1,0) }).default(42)
}
```
(where the body of `div42` desugars to `default( raise-maybe(fn(){ safe-divide(1,0) }), 42 )`).

[Read more about function block expressions][#sec-anon]
{.learn}

[Read more about _dot_ expressions][#sec-dot]
{.learn}



#### A State Effect { #sec-state; }

For more examples of the use of `return` operations, we look at a the state effect.
In its most general form it has just a `set` and `get` operation:

```
effect state<a> {
  fun get() : a
  fun set( x : a ) : ()
}

fun sumdown( sum : int = 0 ) : <state<int>,div> int {
  val i = get()
  if (i <= 0) then sum else {
    set( i - 1 )
    sumdown( sum + i )
  }
}
```

We can define a generic state handler most easily by using `var` declarations:

```
fun state( init : a, action : () -> <state<a>|e> b ) : e b {
  var st := init
  with handler {
    fun get(){ st }
    fun set(i){ st := i }
  }
  action()
}
```

where `state(10){ sumdown() }` evaluates to `55`. 

[Read more about default parameters][#sec-default]
{.learn}

[Read more about _trailing lambdas_][#sec-anon]
{.learn}

[Read more about `var` mutable variables][#sec-var]
{.learn}


Building on the previous state example, suppose we also like
to return the final state. A nice way to do this is to 
use a return operation again to pair the final result with the final state: 

```
fun pstate( init : a, action : () -> <state<a>|e> b ) : e (b,a) {
  var st := init
  with handler {
    return(x){ (x,st) }      // pair with the final state
    fun get(){ st }
    fun set(i){ st := i }
  }
  action()
}
```
where `pstate(10){ sumdown() }` evaluates to `(55,0)`.  

~ advanced
It is even possible to have a handler that only
contains a single `return` operation: such handler handles no effect
at all but only transforms the final result of a function.
For example, we can define the previous example also with
a separate `return` handler as:

```
fun pstate2( init : a, action : () -> <state<a>|e> b ) : e (b,a) {
  var st := init
  with return(x){ (x,st) }
  with handler {
    fun get(){ st }
    fun set(i){ st := i }
  }
  action()
}
```

Here it as a bit contrived but it can make certain
programs more concise in their definition, see 
for example [@Lindley:liberate].
~

### Combining Handlers { #sec-combine; }

~ advanced
What makes effect handlers a good control-flow abstraction? There are three fundamental advantages 
with regard to other approaches:

1. _Effect handlers can have simple (Hindley-Milner) types_. This unlike `shift`/`reset` for example as that needs 
   type rules with _answer_ types (as the type of `shift` depends on the context of its matching `reset`).
2. _The scope of an effect handler is delimited_ by the handler definition. This is just like `shift`/`reset` 
   but unlike ``call/cc``. Delimiting the scope of a resumption has various good properties, like efficient
   implementation strategies, but also that it allows for modular composition 
   (see also Oleg Kiselyov's ["against call/cc"](http://okmij.org/ftp/continuations/against-callcc.html)).
3. _Effect handlers can be composed freely_. This is unlike general _monads_ which need monad transformers to 
   compose in particular ways. Essentially effect handlers can compose freely because every effect handler
   can be expressed eventually as an instance of a _free monad_ which _do_ compose. This also means means that
   some monads cannot be expressed as an effect handler (namely the non-algebraic ones). A particular example
   of this is the continuation monad (which can express ``call/cc``).

The &koka; compiler internally uses monads and `shift`/`reset` to compile effect handlers though, and
it compiles handlers into to an internal free monad based on multi-prompt delimited control [@Gunter:mprompt]. 
By inlining the monadic _bind_ we are able to generate efficient C code that only allocates continuations 
in the case one is actually yielding up to a general `control` operation.
~

A great property of effect handlers is that they can be freely composed together. 
For example, suppose we have a function 
that calls `raise` if the state is an odd number:

```
fun no-odds() : <raise,state<int>> int {
  val i = get()
  if (i.is-odd) then raise("no odds") else {
    set(i / 2)
    i
  }
}
```

then we can compose a `pstate` and `raise-maybe` handler together
to handle the effects:

```
fun state-raise(init) : (maybe<int>,int) {
  with pstate(init)  
  with raise-maybe  
  no-odds()
}
```

where both the `:state<int>` and `:raise` effects are discharged by the respective handlers.
Note the type reflects that we always return a pair with as a first element either
`Nothing` (if `raise` was called) or a `Just` with the final result, and as the second element
the final state. This corresponds to how we usually combine state and exceptions where the 
state (or heap) has set to the state at the point the exception happened.

However, if we combine the handlers in the opposite order, we get a form of transactional
state where we either get an exception (and no final state), or we get a pair of the 
result with the final state:

```
fun raise-state(init) : maybe<(int,int)> {
  with raise-maybe  
  with pstate(init)  
  no-odds()
}
```

### Masking Effects { #sec-mask; }

Similar to masking signals in Unix, we can mask effects to not be handled by
their innermost effect handler. The expression `mask<eff>(action)` modularly masks
any effect operations in `:eff` inside the `action`. For example,
consider two nested handlers for the `emit` operation:

```
fun mask-emit() {
  with fun emit(msg){ println("outer:" + msg) }
  with fun emit(msg){ println("inner:" + msg) }
  emit("hi")
  mask<emit>{ emit("there") }
}
```
If we call `mask-emit()` it prints:
````
inner: hi
outer: there
````
The second call to `emit` is masked and therefore it skips the innermost
handler and is handled subsequently by the outer handler (&ie; mask only
masks an operation once for its innermost handler). 

The type of `mask<l>` for some effect label `:l` is `: (action: () -> e a) -> <l|e> a`
where it injects the effect `:l` in the final effect result `:<l|e>` (even
thought the `mask` itself never
actually performs any operation in `:l` -- it only masks any operations
of `:l` in `action`).

This type usually leads to duplicate effect labels, for example,
the effect of `mask<emit>{ emit("there") }` is `: <emit,emit>` signifying
that there need to be two handlers for `:emit`: in this case, one to skip 
over, and one to subsequently handle the masked operation.


#### Effect Abstraction

The previous example is not very useful, but generally we can 
use `mask` to hide internal effect handling from higher-order functions.
For example, consider the following function that needs to handle
internal exceptions:

```
fun mask-print( action : () -> e int ) : e int {
  with control raise(msg){ 42 }
  val x = mask<raise>(action)
  if (x.is-odd) then raise("wrong")   // internal exception
  x
}  
```

Here the type of `mask-print` does not expose at all that we handle the `:raise`
effect internally for specific code and it is fully abstract -- even if the action itself would call `raise`,
it would neatly skip the internal handler due to the `mask<raise>` expression.

If we would leave out the `mask`, and call `action()` directly, then the inferred
type of `action` would be `: () -> <raise|e> int` instead, showing that the `:raise`
effect would be handled. 
Note that this usually the desired behaviour since in the majority of cases 
we _want_ to handle the effects in a particular way when defining handler abstractions. 
The cases where `mask` is needed are much less common in our experience.


~ advanced

#### State as a Combined Effect

Another nice use-case for `mask` occurs when modeling state directly using
effect handlers without using mutable local variables [@Biernacki:care]. We can do this
using two separate operations `peek` and `poke`:

```
effect<a> val peek : a                 // get the state
effect<a> control poke( x : a ) : ()   // set the state to x
```

We can now define a generic state handler as:

```
fun ppstate( init : a, action : () -> <peek<a>,poke<a>|e> b ) : e b {
  with val peek = init
  with control poke( x ) {
    mask<peek> {
      with val peek = x
      resume(())
    }
  }
  action()
}
```
In the handler for `poke` we resume under a fresh handler for `peek` that
is bound to the new state. This means though there will be an ever increasing
"stack" of handlers for `peek`. To keep the type from growing infinitely, we
need to mask out any potential operation to a previous handler of `peek` which
is why the `mask` is needed. (Another way of looking at this is to just follow
the typing: `action` has a `:peek` effect, and unifies with the effect of
the `poke` operation definition. Since it handles its own `:peek` effect, it needs
to be injected back in with a `mask`.)

(Note: since the handler stack grows indefinitely on every `poke` this example
is mostly of theoretical interest. However, we are looking into a _stack smashing_
technique where we detect at runtime that a `mask` can discard a handler frame
from the stack.)

~

### Overriding Handlers

A common use for masking is to override handlers. For example, consider
overriding the behavour of `emit`:

```
fun emit-quoted1( action : () -> <emit,emit|e> a ) : <emit|e> a {
  with fun emit(msg){ emit("\"" + msg + "\"" ) }
  action()
}
```

Here, the handler for `emit` calls itself `emit` to actually emit the newly
quoted string. The effect type inferred for `emit-quoted` is `: (action : () -> <emit,emit|e> a) -> <emit|e> a`.
This is not the nicest type as it exposes that `action` is evaluated under (at least) two
`:emit` handlers (and someone could use `mask` inside `action` to use the outer `:emit` handler).

The `override` keyword keeps the type nice and fully overrides the
previous handler which is no longer accessible from `action`:

```
fun emit-quoted2( action : () -> <emit|e> a ) : <emit|e> a {
  with override fun emit(msg){ emit("\"" + msg + "\"" ) }
  action()
}
```

This of course applies to any handler or value, for example,
to temporarily increase the `width` while pretty printing, 
we can override the `width` as:
```
fun extra-wide( action ) {
  with override val width = 2*width
  action()
}
```


~ advanced

#### Mask Behind

Unfortunately, we cannot modularly define overriding with just `mask`; if we 
add `mask` outside of the `emit` handler, the `emit` call inside the operation 
definition would get masked and skip our intended handler. On the other hand,
if we add `mask` just over `action` all its `emit` calls would be masked for
our intended handler! 

For this situation, there is another primitive that only "masks the masks".
The expression `mask behind<l>` has type `: (() -> <l|e> a) -> <l,l|e> a`
and only masks any masked operations but not the direct ones. The `override`
keyword is defined in terms of this primitive:

~~ translate
```unchecked
with override handler<l> { <ops> }
<body>
```
&mapsto;
```unchecked
(handler<l> { <ops> })(mask behind<l>{ <body> })
```
~~

This ensures any operation calls in ``<body>`` go the newly defined
handler while any masked operations are masked one more level and skip
both of the two innermost handlers.
~


### Side-effect Isolation { #sec-isolate; }

### Resuming more than once { #sec-multi-resume; }

### Initially and Finally { #sec-resource; }

#### Raw Control { #sec-rcontrol; }

~ Todo
Use `rcontrol` for raw control operations which do not automatically
finalize; this gives true first-class resumption contexts (as `rcontext`) but need
to be used with care. With `rcontrol` one can use the implicitly bound
resumption context `rcontext` to either resume (as `rcontext.resume(x)`),
or to finalize a resumption (as `rcontext.finalize`) which runs all
`finally` handlers to clean up resources.
~

### Linear Effects { #sec-linear; }

~ Todo
Use `linear effect` to declare effects whose operations are always tail-resumptive
and use only linear effects themselves
(and thus resume exactly once). This removes monadic translation for such effects and
can make code that uses only linear effects more compact and efficient.
~

### Named and Scoped Handlers { #sec-namedh; }



## FBIP: Functional but In-Place { #sec-fbip; }

With [Perceus][#why-fbip] reuse analysis we can
write algorithms that dynamically adapt to use in-place mutation when
possible (and use copying when used persistently). Importantly,
you can rely on this optimization happening, &eg; see
the `match` patterns and pair them to same-sized constructors in each branch.

This style of programming leads to a new paradigm that we call FBIP:
"functional but in place". Just like tail-call optimization lets us
describe loops in terms of regular function calls, reuse analysis lets us
describe in-place mutating imperative algorithms in a purely functional
way (and get persistence as well).

~ Note
FBIP is still active research. In particular we'd like to add ways to add 
annotations to ensure reuse is taking place.
~

### Tree Rebalancing

As an example, we consider
insertion into a red-black tree [@guibas1978dichromatic]. 
A polymorphic version of this example is part of the [``samples``][samples] directory when you have
installed &koka; and can be loaded as ``:l`` [``samples/basic/rbtree``][rbtree].
We define red-black trees as:
```unchecked
type color { 
  Red 
  Black 
}

type tree {
  Leaf
  Node(color: color, left: tree, key: int, value: bool, right: tree)
}
```
The red-black tree has the invariant that the number of black nodes from
the root to any of the leaves is the same, and that a red node is never a
parent of red node. Together this ensures that the trees are always
balanced. When inserting nodes, the invariants need to be maintained by
rebalancing the nodes when needed. Okasaki's algorithm [@Okasaki:rbtree]
implements this elegantly and functionally:
```unchecked
fun balance-left( l : tree, k : int, v : bool, r : tree ): tree {
  match(l) {
    Node(_, Node(Red, lx, kx, vx, rx), ky, vy, ry)
      -> Node(Red, Node(Black, lx, kx, vx, rx), ky, vy, Node(Black, ry, k, v, r))
    ...
}

fun ins( t : tree, k : int, v : bool ): tree {
  match(t) {
    Leaf -> Node(Red, Leaf, k, v, Leaf)
    Node(Red, l, kx, vx, r)             
      -> if (k < kx) then Node(Red, ins(l, k, v), kx, vx, r)
         ...
    Node(Black, l, kx, vx, r)
      -> if (k < kx && is-red(l)) then balance-left(ins(l,k,v), kx, vx, r)
         ...
}
```

The &koka; compiler will inline the `balance-left` function. At that point,
every matched `Node` constructor in the `ins` function has a corresponding `Node` allocation --
if we consider all branches we can see that we either match one `Node`
and allocate one, or we match three nodes deep and allocate three. Every 
`Node` is actually reused in the fast path without doing any allocations!
When studying the generated code, we can see the Perceus assigns the 
fields in the nodes in the fast path _in-place_ much like the 
usual non-persistent rebalancing algorithm in C would do.

Essentially this means that for a unique tree, the purely functional
algorithm above adapts at runtime to an in-place mutating re-balancing
algorithm (without any further allocation). Moreover, if we use the tree
_persistently_ [@Okasaki:purefun], and the tree is shared or has
shared parts, the algorithm adapts to copying exactly the shared _spine_
of the tree (and no more), while still rebalancing in place for any
unshared parts.


### Morris Traversal

As another example of FBIP, consider mapping a function `f` over
all elements in a binary tree in-order as shown in the `tmap-inorder` example:

```
type tree {
  Tip
  Bin( left: tree, value : int, right: tree )
}

fun tmap-inorder( t : tree, f : int -> int ) : tree {
  match(t) {
    Bin(l,x,r) -> Bin( l.tmap-inorder(f), f(x), r.tmap-inorder(f) )
    Tip        -> Tip 
  }
}
```

This is already quite efficient as all the `Bin` and `Tip` nodes are
reused in-place when `t` is unique. However, the `tmap` function is not
tail-recursive and thus uses as much stack space as the depth of the
tree.

````cpp {.aside}
void inorder( tree* root, void (*f)(tree* t) ) {
  tree* cursor = root;
  while (cursor != NULL /* Tip */) {
    if (cursor->left == NULL) {
      // no left tree, go down the right
      f(cursor->value);
      cursor = cursor->right;
    } else {
      // has a left tree
      tree* pre = cursor->left;  // find the predecessor
      while(pre->right != NULL && pre->right != cursor) {
        pre = pre->right;
      }
      if (pre->right == NULL) {
        // first visit, remember to visit right tree
        pre->right = cursor;
        cursor = cursor->left;
      } else {
        // already set, restore
        f(cursor->value);
        pre->right = NULL;
        cursor = cursor->right;
      } 
    } 
  } 
}
````

In 1968, Knuth posed the problem of visiting a tree in-order while using
no extra stack- or heap space [@Knuth:aocp1] (For readers not familiar
with the problem it might be fun to try this in your favorite imperative
language first and see that it is not easy to do). Since then, numerous
solutions have appeared in the literature. A particularly elegant
solution was proposed by @Morris:tree. This is an in-place mutating
algorithm that swaps pointers in the tree to "remember" which parts are
unvisited. It is beyond this tutorial to give a full explanation, but a C
implementation is shown here on the side. The traversal
essentially uses a _right-threaded_ tree to keep track of which nodes to
visit. The algorithm is subtle, though. Since it transforms the tree into
an intermediate graph, we need to state invariants over the so-called
_Morris loops_ [@Mateti:morris] to prove its correctness.

We can derive a functional and more intuitive solution using the FBIP
technique. We start by defining an explicit _visitor_ data structure
that keeps track of which parts of the tree we still need to visit. In
&koka; we define this data type as `:visitor`:
```
type visitor {
  Done
  BinR( right:tree, value : int, visit : visitor )
  BinL( left:tree, value : int, visit : visitor )
}
```

(As an aside, 
Conor McBride [@Mcbride:derivative] describes how we can
generically derive a _zipper_ [@Huet:zipper] visitor for any
recursive type $\mu x. F$ as a list of the derivative of that type,
namely $@list (\pdv{x} F\mid_{x =\mu x.F})$.
In our case, the algebraic representation of the inductive `:tree`
type is $\mu x. 1 + x\times int\times x  \,\cong\, \mu x. 1 + x^2\times int$.
Calculating the derivative $@list (\pdv{x} (1 + x^2\times int) \mid_{x = tree})$
and by further simplification, 
we get $\mu x. 1 + (tree\times int\times x) + (tree\times int\times x)$,
which corresponds exactly to our `:visitor` datatype.)

We also keep track of which `:direction` in the tree 
we are going, either `Up` or `Down` the tree.

```
type direction { 
  Up 
  Down 
}
```

We start our traversal by going downward into the tree with an empty
visitor, expressed as `tmap(f, t, Done, Down)`:

```
fun tmap( f : int -> int, t : tree, visit : visitor, d : direction ) {
  match(d) {
    Down -> match(t) {    // going down a left spine
      Bin(l,x,r) -> tmap(f,l,BinR(r,x,visit),Down) // A
      Tip        -> tmap(f,Tip,visit,Up)           // B
    }
    Up -> match(visit) {  // go up through the visitor
      Done        -> t                             // C
      BinR(r,x,v) -> tmap(f,r,BinL(t,f(x),v),Down) // D
      BinL(l,x,v) -> tmap(f,Bin(l,x,t),v,Up)       // E
    } 
  } 
}
```
 
The key idea is that we
are either `Done` (`C`), or, on going downward in a left spine we
remember all the right trees we still need to visit in a `BinR` (`A`) or,
going upward again (`B`), we remember the left tree that we just
constructed as a `BinL` while visiting right trees (`D`). When we come
back up (`E`), we restore the original tree with the result values. Note
that we apply the function `f` to the saved value in branch `D` (as we
visit _in-order_), but the functional implementation makes it easy to
specify a _pre-order_ traversal by applying `f` in branch `A`, or a
_post-order_ traversal by applying `f` in branch `E`.

Looking at each branch we can see that each `Bin` matches up with a
`BinR`, each `BinR` with a `BinL`, and finally each `BinL` with a `Bin`.
Since they all have the same size, if the tree is unique, each branch
updates the tree nodes _in-place_ at runtime without any allocation,
where the `:visitor` structure is effectively overlaid over the tree
nodes while traversing the tree. Since all `tmap` calls are tail calls,
this also compiles to a tight loop and thus needs no extra stack- or heap
space.

Finally, just like with re-balancing tree insertion, the algorithm as
specified is still purely functional: it uses in-place updating when a
unique tree is passed, but it also adapts gracefully to the persistent
case where the input tree is shared, or where parts of the input tree are
shared, making a single copy of those parts of the tree.

[Read the Perceus technical report][Perceus]
{.learn}