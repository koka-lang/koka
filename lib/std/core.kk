/*---------------------------------------------------------------------------
  Copyright 2012-2021, Microsoft Research, Daan Leijen.

  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the LICENSE file at the root of this distribution.
---------------------------------------------------------------------------*/

/* Core functions.

   This module is implicitly imported and all functions and types
   are always available.
   Some types and operations are required to be defined for the compiler
   to work correctly (i.e. types like `:exn` or `:list`)
*/
module std/core

pub import std/core/types
pub import std/core/hnd
pub import std/core/console
pub import std/core/exn
pub import std/core/bool
pub import std/core/order
pub import std/core/char
pub import std/core/int
pub import std/core/vector
pub import std/core/string
pub import std/core/list


extern import
  c  file "core/core-inline"
  cs file "core/core-inline.cs"
  js file "core/core-inline.js"

// ----------------------------------------------------------------------------
// Builtin effects
// ----------------------------------------------------------------------------

// An alias for the empty effect.
// pub alias total = <>

// An alias for pure effects: a pure function always returns the same result
// when called with the same arguments but may not terminate or raise an exception.
pub alias pure = <exn,div>

// The `:global-scope` is a special type constant to denote the global scope
pub type global-scope :: S

// The `:net` effect signifies a function may access the network
pub type net :: X

// The `:fsys` effect signifies a function may access the file system
pub type fsys :: X

// The `:ui` effect signifies a function may access the graphics system
pub type ui :: X

// The `:blocking` effect signifies that a function may block
pub type blocking :: X

// The `:io-total` effect is used for functions that perform arbitrary I/O operations, but are terminating without raising exceptions.
pub alias io-total = <ndet,console,net,fsys,ui,st<global>>

// The `:io-noexn` effect is used for functions that perform arbitrary I/O operations, but raise no exceptions
pub alias io-noexn = <div,io-total>

// The `:io` effect is used for functions that perform arbitrary I/O operations.
pub alias io = <exn,io-noexn>


// ----------------------------------------------------------------------------
// Masking
// ----------------------------------------------------------------------------

// Add the state effect to a function effect.
pub inline extern inject-st : forall<a,h,e> (() -> e a) -> total (() -> <st<h>|e> a)
  inline "#1"

// ----------------------------------------------------------------------------
// Standard Functions
// ----------------------------------------------------------------------------

// The `const` funs returns its first argument and ignores the second.
pub fun const(x,y)
  x

// Apply a function `f` to a specified argument `x`.
pub fun apply(f,x)
  f(x)

// Compose two funs `f` and `g`.
pub fun o(f,g)
  fn(x) f(g(x))

// The `ignore` function ignores its argument.
pub fun ignore( x : a ) : ()
  ()

// Return a 'constant' function that ignores its argument and always returns the same result
pub fun default/const( default : a ) : total (( x : b ) -> a)
  fn(_) default

// ----------------------------------------------------------------------------
// Standard Data types
// ----------------------------------------------------------------------------

pub fip fun bool/int( b : bool ) : int
  if b then 1 else 0

pub fip fun mbint( m : maybe<int> ) : int
  match m
    Nothing -> 0
    Just(i) -> i

// Convert an int to a boolean, using `False` for 0 and `True` otherwise.
pub fun int/bool( i : int ) : bool
  i != 0

// Convert a `:maybe` type to a boolean using `False` for `Nothing` and `True` for `Just`.
pub fun maybe/bool( m : maybe<a> ) : bool
  match m
    Nothing -> False
    _       -> True

// Convert a string to a boolean, using `False` for the empty string and `True` otherwise.
pub fun string/bool( s : string ) : bool
  s != ""

// Match a `:maybe` value and either return a default value on `Nothing` or apply a function to the value on `Just`
pub fun maybe/maybe( m : maybe<a>, onNothing: b, onJust: a -> e b ) : e b
  match m
    Nothing -> onNothing
    Just(x) -> onJust(x)

// Convert a `:maybe<a>` value to `:a`, using the `nothing` parameter for `Nothing`.
// This is an alias for `default`.
pub fun default/maybe( m : maybe<a>, nothing : a ) : a
  default(m,nothing)

// Convert a `:maybe<a>` value to `:a`, using the `nothing` parameter for `Nothing`.
pub fun default( m : maybe<a>, nothing : a ) : a
  match m
    Nothing -> nothing
    Just(x) -> x

// Get the value of the `Just` constructor or raise an exception
pub fun unjust( m : maybe<a> ) : exn a
  match m
    Just(x) -> x
    Nothing -> throw("unexpected Nothing in std/core/unjust")

pub fun maybe/map( m : maybe<a>, f : a -> e b ) : e maybe<b>
  match m
    Nothing -> Nothing
    Just(x) -> Just(f(x))

pub fun maybe/(||)( m1 : maybe<a>, m2: maybe<a> ) : maybe<a>
  match m1
    Nothing -> m2
    _       -> m1

// Convert a `:either` to a `:maybe` type discarding the value of the `Left` constructor
// and using `Just` for the `Right` constructor.
pub fun either/maybe( e : either<a,b> ) : maybe<b>
  match e
    Left -> Nothing
    Right(x) -> Just(x)

// Map over the `Right` component of an `:either` type.
pub fun either/map( e : either<a,b>, f : b -> e c  ) : e either<a,c>
  match e
    Right(x) -> Right(f(x))
    Left(x)  -> Left(x)

// A `:stream` is a co-inductive type representing an infinite list of elements.
pub co type stream<a>
  con Next(head:a, tail: stream<a> )

pub fun tuple2/map( t : (a,a), f : a -> e b ) : e (b, b)
  (t.fst.f, t.snd.f)

pub fun tuple3/map( t : (a,a,a), f : a -> e b ) : e (b, b, b)
  (t.fst.f, t.snd.f, t.thd.f)

pub fun tuple4/map( t : (a,a,a,a), f : a -> e b ) : e (b,b,b,b)
  (t.fst.f, t.snd.f, t.thd.f, t.field4.f)



// ----------------------------------------------------------------------------
// Generic equality
// ----------------------------------------------------------------------------

// Element-wise tuple equality
pub fun tuple2/(==)( (x1,y1) : (a,b), (x2,y2) : (a,b), ?fst/(==) : (a,a) -> bool, ?snd/(==) : (b,b) -> bool ) : bool
  (x1==x2) && (y1==y2)

// Element-wise triple equality
pub fun tuple3/(==)( (x1,y1,z1) : (a,b,c), (x2,y2,z2) : (a,b,c), ?fst/(==) : (a,a) -> bool, ?snd/(==) : (b,b) -> bool, ?thd/(==) : (c,c) -> bool ) : bool
  (x1==x2) && (y1==y2) && (z1==z2)

// Equality on `:maybe`
pub fun maybe/(==)( mb1 : maybe<a>, mb2 : maybe<a>, ?(==) : (a,a) -> bool ) : bool
  match mb1
    Just(x)  -> match mb2
                  Just(y) -> x==y
                  Nothing -> False
    Nothing  -> match mb2
                  Nothing -> True
                  _       -> False

// Element-wise list equality
pub fun list/(==)( xs : list<a>, ys : list<a>, ?(==) : (a,a) -> bool ) : bool
  match xs
    Cons(x,xx) -> match ys
                    Nil        -> False
                    Cons(y,yy) -> (x==y) && (xx==yy)
    Nil -> match ys
             Nil  -> True
             Cons -> False

// Generic inequality
pub fun eq/(!=)(x : a, y : a, ?(==) : (a,a) -> bool ) : bool
  not(x==y)

// Generic equality if `cmp` exists
pub fun cmp/(==)(x : a, y : a, ?cmp : (a,a) -> order ) : bool
  match cmp(x,y)
    Eq -> True
    _  -> False

// ----------------------------------------------------------------------------
// Generic compare
// ----------------------------------------------------------------------------

pub fip fun order/(&&)( x : order, y : order ) : order
  match x
    Eq -> y
    lg -> lg

// Order on tuples
pub fun tuple2/cmp( (x1,y1) : (a,b), (x2,y2) : (a,b), ?fst/cmp : (a,a) -> order, ?snd/cmp : (b,b) -> order ) : order
  fst/cmp(x1,x2) && snd/cmp(y1,y2)

// Order on triples
pub fbip fun tuple3/cmp( (x1,y1,z1) : (a,b,c), (x2,y2,z2) : (a,b,c), ^?fst/cmp : (a,a) -> order, ^?snd/cmp : (b,b) -> order, ^?thd/cmp : (c,c) -> order ) : order
  fst/cmp(x1,x2) && snd/cmp(y1,y2) && thd/cmp(z1,z2)

// Order on `:maybe` values
pub fun maybe/cmp( mb1 : maybe<a>, mb2 : maybe<a>, ?cmp : (a,a) -> order ) : order
  match mb1
    Just(x)  -> match mb2
                  Just(y) -> cmp(x,y)
                  Nothing -> Gt
    Nothing  -> match mb2
                  Nothing -> Eq
                  _       -> Lt

// Order on lists
pub fun list/cmp( xs : list<a>, ys : list<a>, ?cmp : (a,a) -> order ) : order
  match xs
    Cons(x,xx) -> match ys
                    Nil        -> Gt
                    Cons(y,yy) -> match cmp(x,y)
                                    Eq -> cmp(xx,yy)
                                    lg -> lg
    Nil -> match ys
             Nil  -> Eq
             Cons -> Lt

// Generic greater than
pub fun cmp/(>)(x : a, y : a, ?cmp : (a,a) -> order ) : bool
  cmp(x,y) == Gt

// Generic lower than
pub fun cmp/(<)(x : a, y : a, ?cmp : (a,a) -> order ) : bool
  cmp(x,y) == Lt

// Generic greater than or equal
pub fun cmp/(>=)(x : a, y : a, ?cmp : (a,a) -> order ) : bool
  y < x

// Generic lower than or equal
pub fun cmp/(<=)(x : a, y : a, ?cmp : (a,a) -> order ) : bool
  y > x





// ----------------------------------------------------------------------------
// Parse numbers
// ----------------------------------------------------------------------------

// Parse an integer using `parseInt`. If an illegal digit character is encountered the
// `default` value is returned. An empty string will also result in `default`.
pub fun parse-int-default( s : string, default : int = 0, hex : bool = False ) : int
  if s.is-empty then default else s.parse-int(hex).maybe(default)

// Parse an integer after trimming whitespace.
// If an illegal digit character is encountered `Nothing` is returned.
// An empty string will result in `Just(0)`.
// A string can start with a `-` sign for negative numbers,
// and with `0x` or `0X` for hexadecimal numbers (in which case the `hex` parameter is ignored).
pub fun parse-int( s : string, hex : bool = False) : maybe<int>
  s.trim.xparse-int(hex)

extern xparse-int( s : string, hex : bool ) : maybe<int>
  c  "kk_integer_xparse"
  cs "Primitive.IntParse"
  js "$std_core_int._int_parse"

// ----------------------------------------------------------------------------
// Floating point
// todo: move to std/num/float64
// ----------------------------------------------------------------------------

pub inline fip extern float64/(==) : (float64,float64) -> bool  { inline "(#1 == #2)"; js inline "(#1 === #2)" }
pub inline fip extern float64/(!=) : (float64,float64) -> bool  { inline "(#1 != #2)"; js inline "(#1 !== #2)" }
pub inline fip extern float64/(<=) : (float64,float64) -> bool  { inline "(#1 <= #2)" }
pub inline fip extern float64/(>=) : (float64,float64) -> bool  { inline "(#1 >= #2)" }
pub inline fip extern float64/(<)  : (float64,float64) -> bool  { inline "(#1 < #2)" }
pub inline fip extern float64/(>)  : (float64,float64) -> bool  { inline "(#1 > #2)" }
pub inline fip extern float64/(+) : (float64,float64) -> float64 { inline "(#1 + #2)" }
pub inline fip extern float64/(-) : (float64,float64) -> float64 { inline "(#1 - #2)" }
pub inline fip extern float64/(*) : (float64,float64) -> float64 { inline "(#1 * #2)" }
pub inline fip extern float64/(/) : (float64,float64) -> float64 { inline "(#1 / #2)" }
pub inline fip extern float64/(%) : (float64,float64) -> float64 { c inline "fmod(#1,#2)"; inline "(#1 % #2)" }


// Is the value negative?
pub fip fun float64/is-neg( d : float64 ) : bool
  d < 0.0

// Is the value positive?
pub fip fun float64/is-pos( d : float64 ) : bool
  d > 0.0

// Is the value zero?
pub fip fun float64/is-zero( d : float64 ) : bool
  d == 0.0

pub fip fun float64/sign( d : float64 ) : order
  if d < 0.0 then Lt elif d > 0.0 then Gt else Eq

// Negate a `:float64`.
pub inline fip extern float64/(~)( f : float64 ) : float64
  inline "(-#1)"  // inline so `~0.0` becomes negative zero

// convert a `:float64` to an `:int` using `round` to round to its nearest integer.
// (rounding to an even number on a tie)
// Returns `0` if the argument is not `finite?`.
pub inline extern float64/int( f : float64 ) : int
  c  "kk_integer_from_double"
  cs "Primitive.IntDouble"
  js "$std_core_int._int_double"

// Returns the value `f`  raised to the power `p` .
pub inline fip extern float64/(^)( f : float64, p : float64) : float64
  c inline "pow(#1,#2)"
  cs "Math.Pow"
  js "Math.pow"

// Return the absolute value of a `:float64` `f`
pub inline fip extern float64/abs( f : float64 ) : float64
  c inline "kk_double_abs(#1)"
  cs "Math.Abs"
  js "Math.abs"

// Returns the smallest of two floats
pub fip fun float64/min( x : float64, y : float64 ) : float64
  if x <= y then x else y

// Returns the largest of two floats
pub fip fun float64/max( x : float64, y : float64 ) : float64
  if x >= y then x else y

// Returns the smallest element of a list of floats (or `0` for the empty list)
pub fun float64/minimum( xs : list<float64> ) : float64
  match xs
    Nil -> 0.0
    Cons(x,xx) -> xx.foldl( x, min )

// Returns the largest element of a list of floats (or `0` for the empty list)
pub fun float64/maximum( xs : list<float64> ) : float64
  match xs
    Nil -> 0.0
    Cons(x,xx) -> xx.foldl( x, max )


// ----------------------------------------------------------------------------
// Delayed values
// ----------------------------------------------------------------------------

// Delayed (or _lazy_) values are computed (with effect `:e`) only the first time
// `force` is called and cached afterwards.
abstract value type delayed<e,a>
  con XDelay( dref : ref<global,either<() -> e a,a>> )

// Create a new `:delayed` value.
pub fun delay( action : () -> e a ) : delayed<e,a>
  unsafe-total
    val r : ref<global,either<_,_>> = ref(Left(action))
    XDelay(r)

// Force a delayed value; the value is computed only on the first
// call to `force` and cached afterwards.
pub fun force( delayed : delayed<e,a> ) : e a
  unsafe-total
    val r = delayed.dref
    match !r
      Right(x) -> x
      Left(action) ->
        val x = (inject-st{mask<div>(action)})()
        r := Right(x)
        x

// ----------------------------------------------------------------------------
// Show
// ----------------------------------------------------------------------------

// Generic show: shows the internal representation of an object as a string
// Note: this breaks parametricity so it should not be public
extern gshow : forall<a> a -> string
  c "kk_show_any"
  cs inline "#1.ToString()"
  js inline "#1.toString()"

// Generic print routine: prints the internal representation as a string to the console, including a final newline character.
// Note: this breaks parametricity so it should not be pub
fun gprintln( x : a ) : console ()
  println( x.gshow )

// Generic print routine: prints the internal representation as a string to the console,
// including a final newline character.
// Note: this breaks parametricity so it should not be pub
fun gprint( x : a ) : console ()
  print( x.gshow )

// Convert an `:int` to a string
pub extern int/show( i : int ) : string
  c "kk_integer_to_string"
  cs inline "#1.ToString()"
  js inline "#1.toString()"

extern int-show-hex(i:int,use-capitals:bool) : string
  c "kk_integer_to_hex_string"
  cs "Primitive.IntShowHex"
  js "$std_core_int._int_showhex"

// Show an `:int` as a hexadecimal value.\
// The `width`  parameter specifies how wide the hex value is where `"0"`  is used to align.\
// The `use-capitals` parameter (= `True`) determines if captical letters should be used to display the hexadecimal digits.\
// The `pre` (=`"0x"`) is an optional prefix for the number (goes between the sign and the number).
pub fun int/show-hex( i : int, width : int = 1, use-capitals : bool = True, pre : string = "0x" )
  (if i<0 then "-" else "") ++ pre ++ int-show-hex(i.abs,use-capitals).pad-left(width,'0')


// Show a `:float64` as a string.
// If `d >= 1.0e-5` and `d < 1.0e+21`, `show-fixed` is used and otherwise `show-exp`.
// Default `precision` is `-17`.
pub fun float64/show( d : float64, precision : int = -17 ) : string
  val dabs = d.abs
  if dabs >= 1.0e-5 && dabs < 1.0e+21
    then show-fixed(d,precision)
    else show-exp(d,precision)

// Show a `:float64` fixed-point notation.
// The optional `precision` (= `-2`) specifies the maximum precision.
// If `>=0` it specifies the number of digits behind the dot (up to `20` max).
// If negative, then at most the absolute value of `precision` digits behind the dot are used.
// This may still show a number in exponential notation if the it is too small or large,
// in particular, for  a `d` where `d > 1.0e21` or `d < 1.0e-15`, or if
// `precision.abs > 17`, the `show-exp` routine is used.
pub fun float64/show-fixed( d : float64, precision : int = -2 ) : string
  val dabs = d.abs
  if dabs < 1.0e-15 || dabs > 1.0e+21
    then show-exp(d,precision)
    else show-fixedx(d, precision.int32)

extern show-fixedx( d : float64, prec : int32 ) : string
  c  "kk_double_show_fixed"
  cs "Primitive.DoubleShowFixed"
  js "_double_show_fixed"

// Show a `:float64` in exponential (scientific) notation.
// The optional `precision` (= `-17`) specifies the precision.
// If `>=0` it specifies the number of digits behind the dot (up to `17` max).
// If negative, then at most the absolute value of `precision` digits behind the dot are used.
pub fun float64/show-exp( d : float64, precision : int = -17 )
  show-expx(d,precision.int32)

extern show-expx( d : float64, prec : int32 ) : string
  c  "kk_double_show_exp"
  cs "Primitive.DoubleShowExp"
  js "_double_show_exp"

// Show a character as a string
pub fun show-char( c : char ) : string

  if c < ' ' || c > '~' then
    if c == '\n' then "\\n"
    elif c == '\r' then "\\r"
    elif c == '\t' then "\\t"
    elif c.int <= 0xFF then "\\x" ++ c.int.show-hex(2,pre="")
    elif c.int <= 0xFFFF then "\\u" ++ c.int.show-hex(4,pre="")
    else "\\U" ++ c.int.show-hex(6,pre="")
  else
    if c == '\'' then "\\'"
    elif c == '\"' then "\\\""
    elif c == '\\' then "\\\\"
    else c.string

// Show a `:char` as a character literal
pub fun char/show( c : char ) : string
  "'" ++ c.show-char ++ "'"

// Show a string as a string literal
pub noinline fun string/show( s : string ) : string
  "\"" ++ s.list.map(show-char).join ++ "\""

// Convert a `:bool` to a string
pub fun bool/show( b : bool ) : string
  if b then "True" else "False"

// Convert a unit value `()` to a string
pub fun unit/show( u : () ) : string
  "()"

// Show an `:sslice` as a string literal
pub fun sslice/show( s : sslice ) : string
  s.string.show

// Show a list
pub fun list/show( xs : list<a>, ?show : a -> e string ) : e string
  "[" ++ xs.map(show).join(",") ++ "]"

// Show a tuple
pub fun tuple2/show( x : (a,b), ?fst/show : a -> e string, ?snd/show : b -> e string) : e string
  "(" ++ x.fst.show ++ "," ++ x.snd.show ++ ")"

// Show a triple
pub fun tuple3/show( x : (a,b,c), ?fst/show : a -> e string, ?snd/show : b -> e string, ?thd/show : c -> e string) : e string
  "(" ++ x.fst.show ++ "," ++ x.snd.show ++ "," ++ x.thd.show ++ ")"

// Show a `:maybe` type
pub fun maybe/show( mb : maybe<a>, ?show : a -> e string ) : e string
  match mb
    Just(x) -> "Just(" ++ x.show ++ ")"
    Nothing -> "Nothing"


// _deprecated_, use `list/show` instead.
pub fun show-list( xs : list<a>, show-elem : (a) -> e string ) : e string
  list/show(xs,?show=show-elem)

// _deprecated_, use `tuple2/show` instead
pub fun show-tuple( x : (a,b), showfst : a -> e string, showsnd : b -> e string ) : e string
  tuple2/show(x,?fst/show=showfst,?snd/show=showsnd)



// Print a string to the console.
pub fun string/print(s : string)
  prints(s)

// Print a value that has a `show` function
pub fun show/print( x : a, ?show : a -> string ) : console ()
  prints(x.show)

// Print a string to the console, including a final newline character.
pub fun string/println(s : string) : console ()
  printsln(s)

// Print a value that has a `show` function, including a final newline character.
pub fun show/println( x : a, ?show : a -> string ) : console ()
  printsln(x.show)

/*
// Print an integer to the console.
pub fun int/print(i : int)
  prints(show(i))

// Print a float64 to the console.
pub fun float64/print(d : float64)
  prints(show(d))

// Print a boolean to the console
pub fun bool/print( b : bool )
  prints(show(b))

// Print a character to the console.
pub fun char/print(c : char)
  prints(c.string)

// Print a unit value to the console
pub fun unit/print( u : () )
  prints(show(()))
*/

/*
// Print an integer to the console, including a final newline character.
pub fun int/println(i : int)
  printsln(show(i))

// Print a float64 to the console, including a final newline character.
pub fun float64/println(d : float64)
  printsln(show(d))

// Print a boolean to the console, including a final newline character
pub fun bool/println( b : bool )
  printsln(show(b))

// Print a character to the console, including a final newline character.
pub fun char/println(c : char)
  printsln(c.string)

// Print a unit value to the console, including a final newline character
pub fun unit/println( u : () )
  printsln(show(()))
*/

// ----------------------------------------------------------------------------
// Exceptions
// ----------------------------------------------------------------------------

// The `:named` effect is the default umbrella effect for named effects
pub type nmd :: X

pub type scope :: S -> X


// ----------------------------------------------------------------------------
// Null is used for external interfaces
// ----------------------------------------------------------------------------

// Abstract type used for passing `null` values to external functions
pub type null<a>

// Unsafe: transform any type to a `null` type; used internally by the compiler.
pub extern "@null-any"(x : a) : null<a>
  c  inline "(kk_box_is_null(#1) ? kk_datatype_null() : kk_datatype_unbox(#1))"
  cs inline "#1"
  js inline "(#1==null ? null : #1)"  // undefined -> null

// Transform a `:maybe` type to a `:null` type (using `null` for `Nothing`).
pub extern maybe/null(x : maybe<a>) : null<a>
  c  inline "(kk_std_core_types__is_Nothing(#1,kk_context()) ? kk_datatype_null() : kk_datatype_unbox((#1)._cons.Just.value) /* kk_datatype_unbox(kk_datatype_unjust(#1,kk_context())) */ )"
  cs inline "(#1.tag_ == __std_core._maybe_Tag.Nothing ? default(##1) : #1.@value)"
  js inline "(#1==null ? null : #1.value)"

// Transform a `:null` type to a `:maybe` type. Note that it is not
// always the case that `id(x) == maybe(null(x))` (e.g. when `x = Just(Nothing)`).
pub extern null/maybe( n : null<a> ) : maybe<a>
  c  inline "(kk_datatype_is_null(#1) ? kk_std_core_types__new_Nothing(kk_context()) : kk_std_core_types__new_Just(kk_datatype_box(#1),kk_context()))"
  cs inline "(EqualityComparer<##1>.Default.Equals(#1,default(##1)) ? __std_core._maybe<##1>.Nothing_ : new __std_core._maybe<##1>(#1))"
  js inline "(#1==null ? $std_core_types.Nothing : $std_core_types.Just(#1))"

// Cast a integer that is zero to a null
pub fun int/null( i : int ) : null<int>
    i.maybe.null

// Cast an empty string a null
pub fun string/null( s : string ) : null<string>
    s.maybe.null

// Cast a boolean `False` to null
pub fun bool/null( b : bool ) : null<()>
    b.maybe.null

// val null-const : forall<a> null<a> = null(Nothing)

// ----------------------------------------------------------------------------
// Main
// ----------------------------------------------------------------------------

// Used by the compiler to wrap main console applications
pub extern main-console : forall<a,e> ( main : () -> e a ) -> e a
  c "kk_main_console"
  cs inline "Primitive.MainConsole<##1>(#1)"
  js inline "(#1)()"

// ----------------------------------------------------------------------------
// References
// ----------------------------------------------------------------------------

pub inline extern inject-local<a,h,e>( action : () -> e a ) : total (() -> <local<h>|e> a)
  inline "#1"

// Assign to an entry in a local `:vector` variable.
pub inline extern array/index( ^self : local-var<s,vector<a>>, ^index : int, assigned : a ) : <local<s>,exn|e> ()
  c  "kk_ref_vector_assign_borrow"
  cs inline "(#1)[(int)#2] = #3"
  js inline "(#1)[#2] = #3"

// Given a total function to calculate a value `:a`, return
// a total function that only calculates the value once and then
// returns the cached result.
pub fun once( calc : () -> a ) : (() -> a)
  unsafe-total
    val r = ref(Nothing)
    return fn()
      unsafe-total
        match !r
          Just(x) -> x
          Nothing ->
            val x = calc()
            r := Just(x)
            x


// ----------------------------------------------------------------------------
// Unsafe
// ----------------------------------------------------------------------------

// _Unsafe_. This function removes the exception effect (`:exn`) from the effect of an action
pub fun unsafe-no-exn( action : () -> <exn|e> a ) : e a
  unsafe-total( action )



// ----------------------------------------------------------------------------
// Non determinism
// ----------------------------------------------------------------------------

noinline val unique-count : ref<global,int> = unsafe-total{ ref(0) }

// Returns a unique integer (modulo 32-bits).
pub fun unique() : ndet int
  unsafe-total
    val u = !unique-count
    unique-count := u+1
    u

// ----------------------------------------------------------------------------
// Control
// ----------------------------------------------------------------------------

// The `while` fun executes `action`  as long as `pred`  is `true`.
pub fun while( predicate : () -> <div|e> bool, action : () -> <div|e> () ) : <div|e> ()
  if predicate() then
    action()
    while(predicate, action)

// The `repeat` fun executes `action`  `n`  times.
pub fun action/repeat( ^n : int, action : () -> e () ) : e ()
  for(1,n) fn(i)
    action()

// Executes `action`  for each integer between [`start`,`end`]  (including both `start`  and `end` ).
// If `start > end`  the function returns without any call to `action` .
pub fun for( ^start: int, end : int, action : (int) -> e () ) : e ()
  fun rep( ^i : int )
    if i <= end then
      action(i)
      rep(unsafe-decreasing(i.inc))
  rep(start)


// Executes `action`  for each integer between `start`  upto `end`  (including both `start`  and `end` ).
// If `start > end`  the function returns without any call to `action` .
// If `action` returns `Just`, the iteration is stopped and the result returned
pub fun for-while( start: int, end : int, action : (int) -> e maybe<a> ) : e maybe<a>
  fun rep( i : int )
    if i <= end then
      match action(i)
        Nothing -> rep(unsafe-decreasing(i.inc))
        Just(x) -> Just(x)
    else Nothing
  rep(start)


// Return the host environment: `dotnet`, `browser`, `webworker`, `node`, or `libc`.
pub extern host() : ndet string
  c "kk_get_host"
  cs inline "\"dotnet\""
  js inline "$std_core_console._host"

/*
// .open is used in the compiler to open up closed effects.
// Usually simplified away but still present if --no-simplify flag is given
pub inline extern ".open"<e1 :: E, e2 :: E, a, b>( x : a ) : total b
  inline "#1"
*/

// Magic casting to the `:any` type.
pub inline extern "@toany"( x : a ) : any
  inline "#1"
  cs inline "(object)(#1)"

// Internal: used for value effects
// TODO: revisit value effects codegen
pub alias value<a> = a

// Internal: used for value effects
// TODO: revisit value effects codegen
pub extern phantom<a>() : a
  c inline "kk_box_null()"
  inline "undefined"


// ------------------------------------------------------------------------------
// File locations
// ------------------------------------------------------------------------------

// Compilation constant that is replaced with the current file's module name
pub val file/kk-module : string = ""

// Compilation constant that is replaced with the current line number
pub val file/kk-line : string = ""

// Compilation constant that is replaced with the current file name
pub val file/kk-file : string = ""

pub fun file/kk-file-line( ?kk-file, ?kk-line )
  ?kk-file ++ "(line " ++ ?kk-line ++ ")"

// ----------------------------------------------------------------------------
// Trace, assert, todo
// ----------------------------------------------------------------------------

extern xtrace : ( message : string ) -> ()
  c  "kk_trace"
  cs "Primitive.Trace"
  js "_trace"

extern xtrace-any : forall<a> ( message: string, x : a ) -> ()
  c  "kk_trace_any"
  cs "Primitive.TraceAny"
  js "_trace_any"

val trace-enabled : ref<global,bool> = unsafe-total{ ref(True) }

// Trace a message used for debug purposes.
// The behaviour is system dependent. On a browser and node it uses
// `console.log`  by default.
// Disabled if `notrace` is called.
pub fun trace( message : string ) : ()
  unsafe-total
    if !trace-enabled then xtrace(message)

pub fun trace-info( message : string, ?kk-file-line : string ) : ()
  trace(?kk-file-line ++ ": " ++ message)

pub fun trace-show( x : a, ?show : a -> string, ?kk-file-line : string ) : ()
  trace-info(x.show)

pub fun trace-any( message : string, x : a ) : ()
  unsafe-total
    if !trace-enabled then xtrace-any(message,x)

// Disable tracing completely.
pub noinline fun notrace() : st<global> ()
  trace-enabled := False

extern unsafe-assert-fail( msg : string ) : ()
  c "kk_assert_fail"
  js inline "function() { throw new Error(\"assertion failed: \" + #1) }()"

pub fun assert( message : string, condition : bool, ?kk-file-line : string ) : ()   // Compiler removes assert calls in optimized builds
  if !condition then unsafe-assert-fail(kk-file-line ++ ": " ++ message)
