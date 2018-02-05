# An overview of Koka 

This is a short introduction to the Koka programming language meant for
programmers familiar with languages like C++, C#, or JavaScript.

Koka is a _function-oriented_ language that separates pure values from
side-effecting computations (The word 'koka' (or &#x52B9;&#x679C;) means
"effect" or "effective" in Japanese). Koka is also
flexible and `fun`: Koka has many features that help programmers to easily
change their data types and code organization correctly even in large-scale
programs, while having a small strongly-typed language core with a familiar
JavaScript like syntax.

## Hello world

As usual, we start with the familiar _Hello world_ program:<span id=`examplemain`></span>
```
fun main() {
  println("Hello world!") // println output
}
```
Koka uses familiar curly-braces syntax where `//` starts a line
comment. Functions are declared using the `fun` keyword. 

If you are reading this on [Rise4Fun], you can click the _load in editor_
button in the upper right corner of the example to load it into the
editor and run the program.

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
Similarly, Koka's grammar is constructed in such a way that no semi-colons
are needed to separate statements.

## Dot selection {#sec-dot}

Koka is a _function-oriented_ language where _functions_ and _data_ form the
core of the language (in contrast to objects for example). In particular, the
expression `s.encode(3)` does _not_ select the `encode` method from the
`:string` object, but it is simply syntactic sugar for the function call
`encode(s,3)` where `s` becomes the first argument. Similarly, `c.int`
converts a character to an integer by calling `int(c)` (and both expressions
are equivalent). The dot notation is intu&iuml;tive and quite convenient to
chain multiple calls together, as in:

```
fun showit( s : string ) -> s.encode(3).count.println
``` 

for example (where the body desugars as `println(length(encode(s,3)))`). An
advantage of the dot notation as syntactic sugar for function calls is that it
is easy to extend the 'primitive' methods of any data type: just write a new
function that takes that type as its first argument. In most object-oriented
languages one would need to add that method to the class definition itself
which is not always possible if such class came as a library for example.

## Types

Koka is also strongly typed. It uses a powerful type inference engine to
infer most types, and types generally do not get in the way. In
particular, you can always leave out the types of any local variables.
This is the case for example for ``base`` and ``rot`` values in the
previous example; hover with the mouse over the example to see the types
that were inferred by Koka. Generally, it is good practice though to
write type annotations for function parameters and the function result
since it both helps with type inference, and it provides useful
documentation with better feedback from the compiler.

For the `encode` function it is actually essential to give the type of
the `s` parameter: since the `map` function is defined for both `:list`
and `:string` types and the program is ambiguous without an annotation.
Try to load the example in the editor and remove the annotation to see
what error Koka produces.

## Anonymous functions {#sec-anon}

Koka also allows for anonymous function expressions. For example, instead of
declaring the `encode-char` function, we could just have passed it directly to
the `map` function as a function expression:

```
fun encode2( s : string, shift : int )
{
  s.map( fun(c) {
    if (c < 'a' || c > 'z') return c
    val base = (c - 'a').int
    val rot  = (base + shift) % 26
    (rot.char + 'a')
  });
}
```

It is a bit annoying we had to put the final right-parenthesis after the last
brace. As a convenience, Koka allows anonymous functions to _follow_
the function call instead. For example, here is how we can print the numbers
``1`` to ``10``:

```
fun main() { print10() }
////
fun print10() 
{
  for(1,10) fun(i) {
    println(i)
  }
}
```

which is desugared to `for( 1, 10, fun(i){ println(i) } )`. In fact, since we
pass the `i` argument directly to `println`, we can also the function itself
directly, and write `for(1,10,println)`.

Anonymous functions without any arguments can be shortened further by leaving
out the `fun` keyword and just using braces directly. Here is an example using
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

where the body desugars to `repeat( 10, fun(){println(``hi``)} )`. The is
especially convenient for the `while` loop since this is not a built-in
operator in Koka but just a regular function:

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
parenthesis: Koka makes it always explicit whether code is evaluated 
before a function is called (in between parenthesis), or whether 
code is evaluated (potentially
multiple times) by the called function instead (in between braces).

## Effect types

A novel part about Koka is that it automatically infers all the _side effects_
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
fun square1( x : int ) : total int
{
  return x*x
}

fun square2( x : int ) : io int
{
  println( "a not so secret side-effect" )
  return x*x
}

fun square3( x : int ) : div int
{
  square3( x )
  return x*x
}

fun square4( x : int ) : exn int
{
  error( "oops" )
  return x*x
}
```

When the effect is `:total` we usually leave it out in the type annotation.
For example, when we write:

```
fun square5( x : int ) : int { x*x }
```

Then the assumed effect is `:total`. Sometimes, we write an effectful
function, but are not interested in explicitly writing down its effect type.
In that case, we can use a _wildcard type_ which stands for some inferred
type. A wildcard type is denoted by writing an identifier prefixed with an
underscore, or even just an underscore by itself:

```
fun square6( x : int ) : _e int
{
  println("I did not want to write down the io effect")
  return x*x
}
```

Hover over `square6` to see the inferred effect for `:_e`

## Semantics of effects

The inferred effects are not just considered as some extra type information on
functions. On the contrary, through the inference of effects, Koka has a very
strong connection to its denotational semantics. In particular, _the full type
of a Koka functions corresponds directly to the type signature of the
mathematical function that describes its denotational semantics_. For example,
using &#x301A;`:t`&#x301B; to translate a type `:t` into its corresponding
mathematical type signature, we have:

|--------------------------------------------| --------------| ---------------------------------------------------------------------------|
|&#x301A;`:int -> total int `&#x301B;        | =&nbsp;&nbsp; | &#8484;~32~ &rarr; &#8484;~32~                                             |
|&#x301A;`:int -> exn int `&#x301B;          | =             | &#8484;~32~ &rarr; (1 + &#8484;~32~)                                       |
|&#x301A;`:int -> pure int `&#x301B;         | =             | &#8484;~32~ &rarr; (1 + &#8484;~32~)~&#8869;~                              |
|&#x301A;`:int -> <st<h>,pure> int `&#x301B; | =             | (_Heap_ &times; &#8484;~32~) &rarr; (_Heap_ &times; (1 + &#8484;~32~))~&#8869;~|

In the above translation, we use _1 + t_ as a sum
where we have either a unit 1 (i.e. exception) or a type _t_, and we use 
_Heap &times; t_ for a product consisting of a pair of a
heap and a type _t_. From the above correspondence, we can immediately see that
a `:total` function is truly total in the mathematical sense, while a stateful
function (`:st<h> `) that can raise exceptions or not terminate (`:pure`)
takes an implicit heap parameter, and either does not terminate (&#8869;) or
returns an updated heap together with either a value or an exception (`1`).

We believe that this semantic correspondence is the true power of full effect
types and it enables effective equational reasoning about the code by a
programmer. For almost all other existing programming languages, even the most
basic semantics immediately include complex effects like heap manipulation and
divergence. In contrast, Koka allows a layered semantics where we can easily
separate out nicely behaved parts, which is essential for many domains, like
safe LINQ queries, parallel tasks, tier-splitting, sand-boxed mobile code,
etc.

## Combining effects

Often, a function contains multiple effects, for example:

```
fun combine-effects() 
{
  val i = random-int() // non-deterministic
  error("hi")          // exception raising
  combine-effects()    // and non-terminating
}
```

The effect assigned to `combine-effects` are `:ndet`, `:div`, and `:exn`. We
can write such combination as a _row_ of effects as `: <div,exn,ndet> `. When
you hover over the `combine-effects` identifiers, you will see that the type
inferred is really `: <pure,ndet> ` where `:pure` is a type alias defined as

```unchecked
alias pure = <div,exn>
```

## Polymorphic effects

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
fun main() { looptest() }
////
fun looptest() 
{
  while { odd?(random-int()) } 
  {
    error("<b>")
  }
}
```

In the above program, Koka infers that the predicate `odd(random-int())` has
effect `: <ndet|e1> ` while the action has effect `: <exn|e2> ` for some `:e1` and `:e2`. 
When applying `while`, those
effects are unified to the type `: <exn,ndet,div|e3> ` for some `:e3` (which can
be seen by hovering over the `looptest` identifier)


## Isolated state {#sec-runst}

The Fibonacci numbers are a sequence where each subsequent Fibonacci number is
the sum of the previous two, where `fib(0) == 0` and `fib(1) == 1`. We can
easily calculate Fibonacci numbers using a recursive function:

```
fun main() { println(fib(10)) }
////
fun fib(n : int) : div int
{
  if (n <= 0)   then 0
  elif (n == 1) then 1
  else fib(n - 1) + fib(n - 2)
}
```

Note that the type inference engine is currently not powerful enough to
prove that this recursive function always terminates, which leads to
inclusion of the divergence effect `:div` in the result type. 


Here is another version of the Fibonacci function but this time
implemented using local state. We use the `repeat` function to 
iterate `n` times:

```
fun main() { println(fib2(10)) }
////
fun fib2(n) 
{
  var x := 0
  var y := 1
  repeat(n) {
    val y0 = y
    y := x+y
    x := y0
  }
  return x
}
```

The `var` declaration declares a variable that can be assigned too using the
`(:=)` operator. In contrast, a regular equality sign, as in `y0 = y`
introduces an immutable value `y0`. For clarity, one can actually write `val y0 = y` 
for such declaration too but we usually leave out the `val` keyword.

Local variables declared using `var` are actually syntactic sugar for
allocating explicit references to mutable cells. A reference to a mutable
integer is allocated using `r = ref(0)` (since the reference itself is
actually a value!), and can be dereferenced using the bang operator, as ``!r``.
The desugared version of our previously Fibonacci function can be written
using explicit references as

```
fun main() { println(fib3(10)) }
////
fun fib3(n) 
{
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

As we can see, using `var` declarations is quite convenient since such
declaration automatically adds a dereferencing operator to all occurrences
except on the left-hand side of an assignment.

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
i.e. it is not part of an argument or return type. More formally, the Koka
compiler proves this by showing that a function is fully polymorphic in the
heap type `:h` and applies the `run` function (corresponding to ``runST`` in
Haskell) to discard the `:st<h> ` effect.

The Garsia-Wachs algorithm is nice example where side-effects are used
internally across function definitions and data structures, but where the
final algorithm itself behaves like a pure function, see the
``lib/demo/garsiaWachs.kk`` example in the distribution.

  [garsiaWachs]: http://www.rise4fun.com/koka/garsiaWachs {target='_top'}

## A larger example: cracking Caesar encoding

Enough about effects and imperative updates. Let's look at some more functional examples :-) 
For example, cracking Caesar encoded strings:

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
fun freqs( s : string ) : list<double>
{
  val lowers = list('a','z')
  val occurs = lowers.map( fun(c){ s.count(c.string) })
  val total  = occurs.sum
  occurs.map( fun(i){ percent(i,total) } )
}

// Calculate how well two frequency tables match according 
// to the _chi-square_ statistic.
fun chisqr( xs : list<double>, ys : list<double> ) : double
{
  zipwith(xs,ys, fun(x,y){ ((x - y)^2.0)/y } ).foldr(0.0,(+))
}

// Crack a Caesar encoded string
fun uncaesar( s : string ) : string
{
  val table  = freqs(s)                   // build a frequency table for `s`
  val chitab = list(0,25).map( fun(n) {   // build a list of chisqr numbers for each shift between 0 and 25
                  chisqr( table.rotate(n), english ) 
               })
  val min    = chitab.minimum()           // find the mininal element
  val shift  = chitab.index-of( fun(f){ f == min } ).negate  // and use its position as our shift
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
`english` one the closest -- and we use that to decode the string. Let's try
it out in the editor!

## Optional and named parameters

Being a function-oriented language, Koka has powerful support for function
calls where it supports both optional and named parameters. For example, the
function `replace-all` takes a string, a ``pattern`` pattern, and 
a replacement string ``repl``:

```
fun main() { println(world()) }
////
fun world() 
{
  replace-all("hi there", "there", "world")  // returns "hi world"
}
```

Using named parameters, we can also write the function call as:

```
fun main() { println(world2()) }
////
fun world2() 
{
  return "hi there".replace-all( repl="world", pattern="there" )
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
fun sublist( xs : list<a>, start : int,
                  len : int = xs.length ) : list<a>
{
  if (start <= 0) return xs.take(len)
  match(xs) {
    Nil -> Nil
    Cons(_,xx) -> xx.sublist(start - 1, len)
  }
}
```

Hover over the `sublist` identifier to see its full type, where the ``len``
parameter has gotten an optional `:int` type signified by the question mark:
`:?int`.

## Structs

An important aspect of a function-oriented language is to be able to define
rich data types over which the functions work. A common data type is that of a
_struct_ or _record_. Here is an example of a struct that contains information
about a person:

```
struct person( age : int,
               name : string,
               realname : string = name )

val gaga = Person( 25, "Lady Gaga" )
```

Every `struct` (and other data types) come with constructor functions to
create instances, as in `Person(25,``Gaga``)`. Moreover, these
constructors can use named arguments so we can also call the constructor
as `Person( name = "Lady Gaga", age = 25, realname = "Stefani Joanne Angelina Germanotta" )`
which is quite close regular record syntax but without any special rules;
it is just functions all the way down!

Also, Koka automatically generates accessor functions for each field in a
struct (or other data type), and we can access the `age` of a `:person` as
`gaga.age` (which is of course just syntactic sugar for `age(gaga)`).

## Copying

By default, all structs (and other data types) are _immutable_. Instead of
directly mutating a field in a struct, we usually return a new struct where
the fields are updated. For example, here is a `birthday` function that
increments the `age` field:

```
fun main() { println( gaga.birthday.age ) }

struct person( age : int, name : string, realname : string = name )

val gaga = Person( 25, "Lady Gaga" )
////
fun birthday( p : person ) : person  
{
  return p( age = p.age + 1 )
}
```

Here, `birthday` returns a fresh `:person` which is equal to `p` but with the
`age` incremented. The syntax ``p(...)`` is sugar for calling the copy constructor of
a `:person`. This constructor is also automatically generated for each data
type, and is basically defined as:

```
fun main() { println( gaga.copy().age ) }

struct person( age : int, name : string, realname : string = name )

val gaga = Person( 25, "Lady Gaga" )
////
fun copy( p, age = p.age, name = p.name,
               rname = p.realname ) 
{
  return Person(age, name, rname) 
}
```

When arguments follow a data value, as in ``p( age = age + 1)``, it is desugared to call this
copy function, as in `p.copy( age = p.age+1 )`. Again, there are no special
rules for record updates and everything is just function calls with optional
and named parameters.

## More data types

Koka also supports algebraic data types where there are multiple alternatives.
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

We can create such number by writing `integer(1)` or `infinity`. Moreover,
data types can be polymorphic and recursive. Here is the definition of the
`:list` type which is either empty (`Nil`) or is a head element followed by a
tail list (`Cons`):

```unchecked
type list<a> {
  Nil
  Cons( head : a, tail : list<a> )
}
```

Koka automatically generates accessor functions for each named parameter. For
lists for example, we can access the head of a list as `Cons(1,Nil).head`.

We can now also see that `struct` types are just syntactic sugar for regular a
`type` with a single constructor of the same name as the type. For example,
our earlier `:person` struct, defined as

```unchecked
struct person( age : int, name : string,
               realname : string = name )
```

desugars to:

```unchecked
type person { 
  Person( age : int, name : string, realname : string = name ) 
}
```

## Matching

Todo

## Inductive, co-inductive, and recursive types

For the purposes of equational reasoning and termination checking, a `type`
declaration is limited to finite inductive types. There are two more
declarations, namely `cotype` and `rectype` that allow for co-inductive types,
and arbitrary recursive types respectively.

