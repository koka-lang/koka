Title         : Overview of Koka
Heading Base  : 2

Css           : http://fonts.googleapis.com/css?family=Noto+Sans:400,400italic,700,700italic
Css           : http://fonts.googleapis.com/css?family=Noto+Serif:400,400italic,700,700italic
Css           : http://fonts.googleapis.com/css?family=Roboto+Mono:400,500,700,400italic
Css           : http://fonts.googleapis.com/css?family=Roboto+Slab:300,400,700
Css           : styles/koka.css
Css           : styles/risekoka.css 

body {
  font-family: 'Noto Serif','Cambria', "Times New Roman", "Liberation Serif", "Times", serif;
}

Css Header:
  .madoko pre, .madoko code {
    font-size: 1em;
    font-family: Consolas, 'Roboto Mono', monospace;
  }
  body { 
    margin-left: 2em;
    margin-right: 2em;
  }
  span.logo { 
    font-family: `Century Gothic`;
    font-weight: normal;
  }

Html Header   : 
  <!-- NO_CLICK_TRACKING -->
  <!--
    Copyright 2012 Microsoft Corporation.
   
    This is free software; you can redistribute it and/or modify it under the
    terms of the Apache License, Version 2.0. A copy of the License can be
    found in the file `license.txt` at the root of this distribution.
  -->


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

For more background information, see:

* The draft [language specification][langspec] or browse the [library documentation][libraries].
* The [Koka research page][kokaproject] and the [slides] of a talk presented Lang.Next (April 2012).
* The [source code][codeplex] of the Koka compiler.

[langspec]: http://research.microsoft.com/en-us/um/people/daan/koka/doc/kokaspec.html  {target='_top'}
[libraries]: http://research.microsoft.com/en-us/um/people/daan/koka/doc/toc.html {target='_top'}
[slides]: http://research.microsoft.com/en-us/projects/koka/2012-overviewkoka.pdf {target='_top'}
[codeplex]: http://koka.codeplex.com {target='_top'} 
[kokaproject]: http://research.microsoft.com/en-us/projects/koka {target='_top'}

## Hello world

As usual, we start with the familiar _Hello world_ program:<span id=`examplemain`></span>
```
function main() 
{
  println("Hello world!") // println output
}
```

Functions are declared using the `function` or `fun` keyword. It is customary
to use `function` for top-level functions, and `fun` for anonymous function
expressions.

Now click on the _load in editor_ button in the upper right corner of the
_hello world_ example to load it into the editor on the right-hand side and
run the program to see its output.

## Caesar encoding

Here is another short example program that encodes a string using the
_Caesar cipher_, where each lower-case letter in a string is replaced by the letter
three places up in the alphabet:

```
function main() { println(caesar("koka is fun")) }
////
function encode( s : string, shift : int )
{
  function encodeChar(c) {
    if (c < 'a' || c > 'z') return c
    val base = (c - 'a').int 
    val rot  = (base + shift) % 26
    (rot.char + 'a')
  }

  s.map(encodeChar)
}

function caesar( s : string ) : string
{
  s.encode( 3 )
}
```

In this example, we declare a local function `encodeChar` which encodes a
single character `c`. The final statement `s.map(encodeChar)` applies the
`encodeChar` function to each character in the in the string `s`, returning a
new string where each character is Caesar encoded. The result of the final
statement in a function is also the return value of that function, and you can
generally leave out an explicit `return` keyword.

As we can see in the example, we can leave out semi-colons at the end of each
statement or declaration. Koka uses a simple layout rule where semi-colons are
automatically inserted for any statements and declarations that are aligned
between braces (``{`` and ``}``). Long statements can be continued on the next
line simply by using more indentation. Of course, if required, you can always
use explicit semi-colons as well, for example to put multiple statements on a
single line.

Koka is a _function-oriented_ language where _functions_ and _data_ form the
core of the language (in contrast to objects for example). In particular, the
expression `s.encode(3)` does _not_ select the `encode` method from the
`:string` object, but it is simply syntactic sugar for the function call
`encode(s,3)` where `s` becomes the first argument. Similarly, `c.int`
converts a character to an integer by calling `int(c)` (and both expressions
are equivalent). The dot notation is intu&iuml;tive and quite convenient to
chain multiple calls together, as in:

```
function showit( s : string ) -> s.encode(3).length.println
``` 

for example (where the body desugars as `println(length(encode(s,3)))`). An
advantage of the dot notation as syntactic sugar for function calls is that it
is easy to extend the 'primitive' methods of any data type: just write a new
function that takes that type as its first argument. In most object-oriented
languages one would need to add that method to the class definition itself
which is not always possible if such class came as a library for example.

## Types

Koka is also strongly typed. It uses a powerful type inference engine to infer
most types, and types generally do not get in the way. In particular, you can
always leave out the types of any local variables. This is the case for
example for ``base`` and ``rot``. You can hover with your mouse over ``base`` and ``rot`` in the
example to see the types that were inferred by Koka. Generally, we do write
type annotations for function parameters and the function result since it both
helps with type inference, and it provides useful documentation with better
feedback from the compiler.

For the `encode` function it is actually essential to give the type of the `s`
parameter: since the `map` function is defined for both `:list` and `:string`
types, the program is ambiguous without an annotation. Try to load the example
in the editor and remove the annotation to see what error Koka produces.

## Anonymous functions

Koka also allows for anonymous function expressions. For example, instead of
declaring the `encodeChar` function, we could just have passed it directly to
the `map` function as a function expression:

```
function encode2( s : string, shift : int )
{
  s.map( fun(c) {
    if (c < 'a' || c > 'z') return c
    val base = (c - 'a').int 
    val rot  = (base + shift) % 26
    return (rot.char + 'a')
  });
}
```

It is a bit annoying we had to put the final right-parenthesis after the last
brace. As a convenience, Koka allows anonymous functions to _follow_
the function call instead. For example, here is how we can print the numbers
``1`` to ``10``:

```
function main() { print10() }
////
function print10() 
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
function main() { printhi10() }
////
function printhi10()
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
function main() { print11() }
////
function print11() {
  var i := 10
  while { i >= 0 } { 
    println(i)
    i := i - 1 
  }
}
```

In particular, Koka makes it always explicit when code is evaluated before a
function is called (in between parenthesis), or evaluated (potentially
multiple times) by the called function (in between braces).

## Effects

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
function square1( x : int ) : total int
{
  return x*x
}

function square2( x : int ) : io int
{
  println( "a not so secret side-effect" )
  return x*x
}

function square3( x : int ) : div int
{
  square3( x )
  return x*x
}

function square4( x : int ) : exn int
{
  error( "oops" )
  return x*x
}
```

When the effect is `:total` we usually leave it out in the type annotation.
For example, when we write:

```
function square5( x : int ) : int  -> x*x
```

Then the assumed effect is `:total`. Sometimes, we write an effectful
function, but are not interested in explicitly writing down its effect type.
In that case, we can use a _wildcard type_ which stands for some inferred
type. A wildcard type is denoted by writing an identifier prefixed with an
underscore, or even just an underscore by itself:

```
function square6( x : int ) : _e int
{
  println("I did not want to write down the io effect")
  return x*x
}
```

Hover over `square6` to see the inferred effect for `:_e`

## Semantics

The inferred effects are not just considered as some extra type information on
functions. On the contrary, through the inference of effects, Koka has a very
strong connection to its denotational semantics. In particular, _the full type
of a Koka functions corresponds directly to the type signature of the
mathematical function that describes its denotational semantics_. For example,
using &#x301A;`:t`&#x301B; to translate a type `:t` into its corresponding
mathematical type signature, we have:

| | 
--- | ---- | ---
&#x301A;`:int -> total int `&#x301B;        | =&nbsp;&nbsp; | <span class=`math`>&#8484;~32~ &rarr; &#8484;~32~</span>
&#x301A;`:int -> exn int `&#x301B;          | = | <span class=`math`>&#8484;~32~ &rarr; (1 + &#8484;~32~)</span>
&#x301A;`:int -> pure int `&#x301B;         | = | <span class=`math`>&#8484;~32~ &rarr; (1 + &#8484;~32~)~&#8869;~</span>
&#x301A;`:int -> <st<h>,pure> int `&#x301B; | = | <span class=`math`>(Heap &times; &#8484;~32~) &rarr; (Heap &times; (1 + &#8484;~32~))~&#8869;~</span>

In the above translation, we use <span class=`math`>1 + t</span> as a sum
where we have either a unit 1 (i.e. exception) or a type t, and we use 
<span class=`math`>Heap &times; t</span> for a product consisting of a pair of a
heap and a type t. From the above correspondence, we can immediately see that
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
function combineEffects() 
{
  val i = randomInt() // non-deterministic
  error("hi")         // exception raising
  combineEffects()    // and non-terminating
}
```

The effect assigned to `combineEffects` are `:ndet`, `:div`, and `:exn`. We
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
function main() { looptest() }
////
function looptest() 
{
  while { odd(randomInt()) } 
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


## Fibonacci numbers and imperative updates

The fibonacci numbers are a sequence where each subsequent fibonacci number is
the sum of the previous two, where `fib(0) == 0` and `fib(1) == 1`. We can
easily calculate fibonacci numbers using a recursive function:

```
function main() { println(fib(10)) }
////
function fib(n : int) : div int
{
  if (n <= 0)   then 0
  elif (n == 1) then 1
  else fib(n - 1) + fib(n - 2)
}
```

Note that the type inference engine is currently not poweful enough to prove
that this recursive function always terminates, which leads to inclusion of
the divergence effect `:div` in the result type. We can avoid this by using
the `repeat` function and some imperative updates, and increase the efficiency
too:

```
function main() { println(fib2(10)) }
////
function fib2(n) 
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

## References

Local variables declared using `var` are actually syntactic sugar for
allocating explicit references to mutable cells. A reference to a mutable
integer is allocated using `r = ref(0)` (since the reference itself is
actually a value!), and can be derefenced using the bang operator, as `!r`.
The desugared version of our previously fibonacci function can be written
using explicit references as

```
function main() { println(fib3(10)) }
////
function fib3(n) 
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

When we look at the types inferred for references, we see that `x` and `y`
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
internally but where the algorithm itself behaves like a pure function, see
the [garsiaWachs] sample online.

  [garsiaWachs]: http://www.rise4fun.com/koka/garsiaWachs {target='_top'}

## Cracking Caesar

Enough about effects and imperative updates. Let's look at some more functional examples :-) 
For example, cracking Caesar encoded strings:

```
function main() { println( crack( "nrnd lv d ixq odqjxdjh" ) ) }
////
function encode( s : string, shift : int )
{
  function encodeChar(c) {
    if (!(c.isLower)) return c
    val base = (c - 'a').int 
    val rot  = (base + shift) % 26
    return (rot.char + 'a')
  }

  s.map(encodeChar)
}
////
// The letter frequency table for English
val english = [8.2,1.5,2.8,4.3,12.7,2.2,
               2.0,6.1,7.0,0.2,0.8,4.0,2.4,
               6.7,7.5,1.9,0.1, 6.0,6.3,9.1,
               2.8,1.0,2.4,0.2,2.0,0.1]

// Small helper functions
function percent( n : int, m : int ) {
  100.0 * (n.double / m.double)
}

function rotate( xs, n ) {
  xs.drop(n) + xs.take(n)
}

// Calculate a frequency table for a string
function freqs( s : string ) : list<double>
{
  val n      = s.count( isLower )   // count of lowercase letters in `s`
  val lowers = list('a','z')         // list of the lower-case letters 
  lowers.map( fun(c) { percent( s.count(c), n )  } )
}

// Calculate how well two frequency tables match according 
// to the _chi-square_ statistic.
function chisqr( xs : list<double>, ys : list<double> ) : double
{
  return zipWith(xs,ys, fun(x,y){ ((x - y)^2.0)/y } ).sum()
}

// Crack a Caesar encoded string
function crack( s : string ) : string
{
  val table  = freqs(s)                   // build a frequency table for `s`
  val chitab = list(0,25).map( fun(n) {   // build a list of chisqr numbers for each shift between 0 and 25
                  chisqr( table.rotate(n), english ) 
               })   
  val min    = chitab.minimum()           // find the mininal element
  val shift  = chitab.indexOf( fun(f){ f == min } ).negate  // and use its position as our shift
  s.encode( shift )
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

Being a function-oriented languague, Koka has powerful support for function
calls where it supports both optional and named parameters. For example, the
function `substr` takes a string, a ``start`` position, and the length ``len`` of the
desired substring:

```
function main() { println(world()) }
////
function world() 
{
  substr("hi world", 3, 5)  // returns "world"
}
```

Using named parameters, we can also write the function call as:

```
function main() { println(world2()) }
////
function world2() 
{
  return "hi world".substr( len=5, start=3 )
}
```

Optional parameters let you specify default values for parameters that do not
need to be provided at a call-site.  As an example, let's define a function
`sublist` that takes a list, a ``start`` position, and the length ``len`` of the desired
sublist.  We can make the ``len`` parameter optional and by default return all
elements following the ``start`` position by picking the length of the input list by
default:

```
function main() { println( ['a','b','c'].sublist(1).string ) }
////
function sublist( xs : list<a>, start : int,
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
create instances, as in `Person(25,``Gaga``)`. Moreover, these constructors
can use named arguments so we can also call the constructor as "Person( name =
"`Lady Gaga``, age = 25, realname = ``Stefani Joanne Angelina Germanotta`` )`
which is quite close regular record syntax but without any special rules; it
is just functions all the way down!

Also, Koka automatically generates accessor functions for each field in a
struct (or other data type), and we can access the `age` of a `:person` as
`gaga.age` (which is of course just syntactic sugar for `age(gaga)`).

## Copying

By default, all structs (and other data types) are _immutable_. Instead of
directly mutating a field in a struct, we usually return a new struct where
the fields are updated. For example, here is a `birthday` function that
increments the `age` field:

```
function main() { println( gaga.birthday.age ) }

struct person( age : int, name : string, realname : string = name )

val gaga = Person( 25, "Lady Gaga" )
////
function birthday( p : person ) : person  
{
  return p( age = p.age + 1 )
}
```

Here, `birthday` returns a fresh `:person` which is equal to `p` but with the
`age` incremented. The syntax ``p(...)`` is sugar for calling the copy constructor of
a `:person`. This constructor is also automatically generated for each data
type, and is basically defined as:

```
function main() { println( gaga.copy().age ) }

struct person( age : int, name : string, realname : string = name )

val gaga = Person( 25, "Lady Gaga" )
////
function copy( p, age = p.age, name = p.name,
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

Note that the layout rule seperates each _constructor_ with a semi-colon, and
we can also write this on one line as `type colors { Red; Green; Blue }`.
Special cases of these enumerated types are the `:void` type which has no
alternatives (and therefore there exists no value with this type), the unit
type `:()` which has just one constructor, also written as `()` (and
therefore, there exists only one value with the type `:()`, namely `()`), and
finally the boolean type `:bool` with two constructors `True` and `False`.

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

todo

## Inductive, co-inductive, and recursive types

For the purposes of equational reasoning and termination checking, a `type`
declaration is limited to finite inductive types. There are two more
declarations, namely `cotype` and `rectype` that allow for co-inductive types,
and arbitrary recursive types respectively.

