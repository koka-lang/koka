/*---------------------------------------------------------------------------
  Copyright 2012-2024, Microsoft Research, Daan Leijen.

  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the LICENSE file at the root of this distribution.
---------------------------------------------------------------------------*/

/* Primitive types and functions.

   This module is implicitly imported and all functions and types
   are always available.
   These types are required to be defined for the compiler
   to work correctly (i.e. types like `:int` or `:div`)

   The _kinds_ of types are all builtin:

   * `V`   value types (i.e. the star kind)
   * `E`   effect rows (`<div,exn|e>`)
   * `X`   a single effect (`div`, `exn`, `parse`, etc.), also called atomic effect
   * `H`   heap types (always a phantom type without actual values)
   * `HX`  a short-hand for user defined effect handlers of kind `(E,V) -> V`

*/
module std/core/types

pub infixr 80  (^)
pub infixl 70  (*), (%), (/), cdiv, cmod
pub infixr 60  (++)
pub infixl 60  (+), (-)
pub infixr 55  (++.)
pub infix  40  (!=), (==), (<=), (>=), (<), (>)
pub infixr 30  (&&)
pub infixr 20  (||)
// prefix     (!), (-)


// build: 131

// ----------------------------------------------------------------------------
// Core types
// ----------------------------------------------------------------------------

// An arbitrary precision signed integer.
// See `module std/core/int` for integer operations.
pub type int

// A 64-bit IEEE 754 double precision floating point value.
// See `module std/num/float64` for operations on `:float64`s.
pub value type float64

// A unicode character.
// Characters are unicode _codepoint_\/s.
// This is different from a unicode _grapheme_ which represents a single displayed
// symbol and can consists of multiple codepoints due to combining characters and marks.
// (see also `module std/core/char` and the `module std/text/unicode` module).
pub value type char

// A string is a sequence of unicode character points (`char`).
// The encoding of a string is internal and there
// is no constant-time indexing of characters in a string.
// Use the `:sslice` type for efficient matching and retrieving
// sub-strings from string. See `module std/core/string` and
// `module std/core/sslice` for string operations.
pub type string

// A 8-bit signed integer (represented in two's complement).
// Provides no operations and used only for storage and interaction with external code.
pub value type int8

// A 16-bit signed integer (represented in two's complement).
// Provides no operations and used only for storage and interaction with external code.
pub value type int16

// A 32-bit signed integer (represented in two's complement).
// See the `module std/num/int32` module for operations on 32-bit integers.
pub value type int32

// A 64-bit signed integer (represented in two's complement).
// See the `module std/num/int64` module for operations on 64-bit integers.
pub value type int64

// A signed two's complement integer equal to a signed `size_t` in C, i.e.
// it can hold the byte size of any object, in particular the maximum possible
// byte size of a vector or string.
// Provides no operations but is used mostly for interaction with external code.
// The Javascript backend uses `int32` for this.
pub value type ssize_t

// A signed two's complement integer equal to an `intptr_t` in C, i.e.
// it has the same bit-size as a native pointer (`void*`).
// Provides no operations but is used mostly for interaction with external code.
// The Javascript backend uses `int64` for this.
pub value type intptr_t

// A 32-bit IEEE 754 single precision floating point value.
// Provides currently no operations and currently only used for storage and for interaction with external code.
pub value type float32

// The type of immutable polymorphic arrays is called `:vector`.
// See `module std/core/vector` for vector operations.
pub type vector<a>

// An any type. Used for external calls.
pub type any

// Internal type used for in-place update of unique pattern matches.
pub type @reuse

pub extern @no-reuse() : @reuse
  c inline  "NULL"
  js inline "null"


// The _total_ effect represents the absence of any effect.
// Usually written as `<>`
pub type total :: E

// The effect constructor extends an effect row with another effect.
// Usually written as `<_|_>`
pub type effect-extend :: (X,E) -> E

// The divergence effect: a divergent function may not terminate.
pub type div :: X

// The non-determinism effect.
pub type ndet :: X

// The alloc effect signifies a function may allocate in a heap `:h`
pub type alloc :: H -> X

// The read effect: signifies that a function may read from from heap `:h`.
pub type read :: H -> X

// The write effect: signifies that a function may write to heap `:h`.
pub type write :: H -> X

// Stateful functions can manipulate heap `:h` using allocations, reads and writes.
pub alias st<h> = <read<h>, write<h>, alloc<h>>

// The `:global` heap is a special heap constant to denote the global shared heap
pub type global :: H

// Handled effects are lifted to an atomic effect using the `:handled` type constructor
// All user defined effects are `:handled` since they require a handler to be dismissed.
// In contrast, some built-in effects, like `:div` or `:st` do not require a handler
// as these are handled by the system.
// In particular, the runtime evidence vector only contains `:handled` (and `:handled1`) effects.
pub type handled    :: HX -> X;

// Linear effects are lifted to an atomic effect using the `:handled1` type constructor
pub type handled1   :: HX -> X;

// Named effects are lifted to an atomic effect with the `:nhandled` type constructor.
// In contrast to `:handled` effects, these effects are not passed in the evidence vector.
pub type nhandled    :: HX -> X;

// Linear named effects are lifted to an atomic effect using the `:nhandled1` type constructor
pub type nhandled1   :: HX -> X;


// ----------------------------------------------------------------------------
// Standard Data types
// ----------------------------------------------------------------------------

// The `:void` type is empty and has no constructors.
// See also the `:()` unit type and the `:bool` type.
pub type void

// The type of booleans has two inhabitants: `True` and `False`.
pub type bool
  con False
  con True

// The unit type `:unit` is inhabited by just a single value, namely `Unit`.
// Usually the type and constructor are written as `()`.
// See also the `:void` type and the `:bool` type.
pub struct unit

// A pair of values `:a` and `:b`. Usually written as `(x,y)` (for types and terms)
pub value struct tuple2<a,b>(fst:a,snd:b)

// A triple of values. Usually written as `(x,y,z)` (for types and terms)
pub value struct tuple3<a,b,c>(fst:a,snd:b,thd:c)

// A quadruple of values. Usually written as `(a,b,c,d)` (for types and terms)
pub struct tuple4<a,b,c,d>(fst:a,snd:b,thd:c,field4:d)

// A quintuple of values. Usually written as `(a,b,c,d,e)` (for types and terms)
pub struct tuple5<a,b,c,d,e>(fst:a,snd:b,thd:c,field4:d,field5:e)

// The `:maybe` type is used to represent either a value (`Just(x)`) or `Nothing`.
// This type is often used to represent values that can be _null_.
// See also `module std/core/maybe`.
pub value type maybe<a>
  // No result
  con Nothing
  // A single result
  con Just( value : a )

// The choice type represents one of two possible types `:a` or `:b`.
// See also `module std/core/either`.
pub value type either<a,b>
  // Left result, usually used for errors.
  con Left( left : a )
  // Right result, usually used for success.
  con Right( right : b )

// An enumeration to represent order: `Lt`, `Eq`, or `Gt`.
// See also `module std/core/order`.
pub type order
  // lower-than
  Lt
  // equal
  Eq
  // greater-than
  Gt

// Represent two elements in ascending order. This is used to
// allow comparison on elements to be used linearly for `fip` functions.
// See also `module std/core/order`
pub value type order2<a>
  // The elements compared lower-than, with `lt < gt`.
  Lt2( lt: a, gt : a )
  // The elements compared equal (with value `eq`).
  Eq2( eq: a )
  // The elements compared greater-than, with `lt < gt`.
  Gt2( lt: a, gt : a )


// The type of lists, which can be either empty (`Nil`) or an element followed
// by a list (`Cons`).
// See `module std/core/list` for operations on lists.
pub type list<a>
  // The empty list.
  con Nil                  // note: must come first; see Core/Core.hs
  // A ``head``  element followed by the ``tail``  of the list.
  con Cons(head:a, tail : list<a> )


// Explicitly box values using the `Box` constructor.
pub value type box<a>
  con Box( unbox : a )


/*
// Explicitly heap allocate using the `Hbox` constructor.
pub ref type hbox<a>
  con Hbox( unhbox : a )

pub fun hbox( x : a ) : hbox<a>
  Hbox(x)
*/

// Prevent inlining an expression by passing it to `keep` (which is a non-inlineable identity function)
pub noinline fip fun keep( x : a ) : a
  x

// ----------------------------------------------------------------------------
// Standard functions
// ----------------------------------------------------------------------------

// The identity function returns its argument unchanged
pub fip fun id(x : a) : a
  x

// Logical conjunction. This is compiled specially avoid evaluating the second argument if `x==False`.
pub fip fun (&&)( x : bool, y : bool) : bool   // inlined in the compiler for short-circuit evaluation
  if x then y else False

// Logical disjunction. This is compiled specially avoid evaluating the second argument if `x==True`.
pub fip fun (||)( x : bool, y : bool) : bool  // inlined in the compiler for short-circuit evaluation
  if x then True else y

// Logical negation
pub fip fun bool/(!)( b : bool ) : bool
  if b then False else True

// Logical negation
pub fip fun not( b : bool ) : bool
  if b then False else True


extern import
  js file "inline/types.js"

// Convert an integer to an `:int32`. The number is _clamped_ to the maximal or minimum `:int32`
// value if it is outside the range of an `:int32`.
// Needed for control flow contexts in `module std/core/hnd`
pub inline fip extern @make-int32( i : int) : int32
  c  "kk_integer_clamp32"
  cs "Primitive.IntToInt32"
  js "$std_core_types._int_clamp32"


// Convert an integer to an `:ssize_t`. The number is _clamped_ to the maximal or minimum `:ssize_t`
// value if it is outside the range of an `:ssize_t`.
// Needed for evidence indices in `module std/core/hnd`
pub fip extern @make-ssize_t( i : int) : ssize_t
  c  "kk_integer_clamp_ssize_t"
  cs "Primitive.IntToInt32"
  js "$std_core_types._int_clamp32"


// Append two strings
pub extern (++)(x : string, y : string) : string
  c "kk_string_cat"
  inline "(#1 + #2)"


// _Internal_: generated by type inference and later refined into one of the `open` variants in `std/core/hnd`.
pub noinline extern @open<e1::E,e2::E,a,b>( x : a ) : e2 b
  inline "#1"


// ----------------------------------------------------------------------------
// References
// ----------------------------------------------------------------------------

// The predicate `:hdiv<h,a,e>` signifies that if the type `:a` contains a reference to `:h`,
// then the effect `:e` must contain the divergent effect (`:div`). This constraint is generated
// when reading from the heap (see `(!)`) and is necessary to catch cases where code can diverge
// by storing self referential functions in the heap.
pub type hdiv :: (H,V,E) -> P

// The predicate `:ediv<x,a,e>` signifies that if the type `:a` contains a reference to effect constant `:x",
// then the effect `:e` must contain the divergent effect (`:div`). This constraint is generated
// for operations on first-class effects where code can diverge through operations.
pub type ediv :: (X,V,E) -> P


// A reference `:ref<h,a>` points to a value of type `:a` in heap `:h`.
pub type ref :: (H,V) -> V

// Allocate a fresh reference with an initial value.
pub inline extern ref( value : a) : alloc<h> ref<h,a>
  c  "kk_ref_alloc"
  cs inline "new Ref<##1,##2>(#1)"
  js inline "{ value: #1 }"

// Assign a new value to a reference.
pub inline extern set( ^ref : ref<h,a>, assigned : a) : <write<h>> ()
  c  "kk_ref_set_borrow"
  cs inline "#1.Set(#2)"
  js inline "((#1).value = #2)"

// Read the value of a reference.
pub inline extern ref/(!) : forall<h,a,e> ( ref : ref<h,a>) -> <read<h>|e> a with(hdiv<h,a,e>)
  c  "kk_ref_get"
  cs inline "#1.Value"
  js inline "#1.value"

// Modify the value of a reference.
// This is especially useful when the reference contains a vector, because
// getting the vector into a local variable and then setting it back into the reference later
// would mean that we hold on to a copy (and we can't update the vector in place then!).
// In contrast, this function passes the ownership to the given function.
pub inline extern modify : forall<h,a,b,e> ( ref : ref<h,a>, f : forall<s> local-var<s,a> -> <local<s>|e> b) -> <read<h>,write<h>|e> b with(hdiv<h,a,e>)
  c  "kk_ref_modify"
  js inline "((#2)(#1))"

// If a heap effect is unobservable, the heap effect can be erased by using the `run` fun.
// See also: _State in Haskell, by Simon Peyton Jones and John Launchbury_.
pub extern run : forall<e,a> ( action : forall<h> () -> <alloc<h>,read<h>,write<h> | e> a ) -> e a
  c  inline "(kk_function_call(kk_box_t,(kk_function_t,kk_context_t*),#1,(#1,kk_context()),kk_context()))"
  cs inline "Primitive.Run<##2>(#1)"
  js inline "((#1)())"


// Mask the state effect. This is more convenient than masking each `:read`, `:write`, and `:alloc` effect separately.
pub inline extern mask-st : forall<a,h,e> (() -> e a) -> total (() -> <st<h>|e> a)
  inline "#1"

/*
// Inject the local effect into a function effect.
pub inline extern inject-local<a,h,e>( action : () -> e a ) : total (() -> <local<h>|e> a)
  inline "#1"
*/

// ----------------------------------------------------------------------------
// Local Variables
// ----------------------------------------------------------------------------

// A local heap effect.
pub type local :: (H) -> X

// A local variable `:local-var<s,a>` points to a value of type `:a` in local scope `:s`.
pub type local-var :: (H,V) -> V

// Allocate a fresh local with an initial value.
pub inline extern local-new<a,s,e>(value:a) : <local<s>|e> local-var<s,a>
  c  "kk_ref_alloc"
  cs inline "new Ref<##1,##2>(#1)"
  js inline "{ value: #1 }"

// Assign a new value to a local variable
pub inline extern local-set<a,s,e>( ^v: local-var<s,a>, assigned: a) : <local<s>|e> ()
  c  "kk_ref_set_borrow"
  cs inline "#1.Set(#2)";
  js inline "((#1).value = #2)"

// Read the value of a local variable.
pub inline extern local-get : forall<a,s,e> (v: local-var<s,a>) -> <local<s>|e> a with(hdiv<s,a,e>)
  c  "kk_ref_get"
  cs inline "#1.Value";
  js inline "((#1).value)";

// _Internal_: if local mutation is unobservable, the `:local` effect can be erased by using the `local-scope` function.
// See also: _State in Haskell, by Simon Peyton Jones and John Launchbury_.
pub inline fun local-scope<a,e>( action : forall<h> () -> <local<h> | e> a) : e a
  unsafe-no-local-cast(action)()

inline extern unsafe-no-local-cast : forall<a,e> ( action : forall<h> () -> <local<h>|e> a ) -> (() -> e a)
  inline "#1"

/*
inline extern local-scope : forall<e,a> ( action : forall<h> () -> <local<h> | e> a) -> e a
  c  "(kk_function_call(kk_box_t,(kk_function_t,kk_context_t*),#1,(#1,kk_context())))"
  cs inline "Primitive.Run<##2>(#1)";
  js inline "((#1)())";
*/

// _Internal_: used to pass locals by reference.
pub inline extern @byref(loc : a) : a
  inline "#1"


// ----------------------------------------------------------------------------
// Used during codegen
// ----------------------------------------------------------------------------

// Magic casting to the `:any` type.
pub inline extern @toany( x : a ) : any
  inline "#1"
  cs inline "(object)(#1)"

// Internal: used for value effects
pub type @valueop<e::E,a::V>
  con @Valueop


// ----------------------------------------------------------------------------
// Internal types
// ----------------------------------------------------------------------------

// _Internal_: Optional is used by the compiler to pass optional arguments.
// It is usually displayed as `: ? tp` for some type `:tp`.
pub value type @optional<a>
  // The `Optional` constructor is used when an optional argument is given.
  con @Optional( value : a )
  // `None` is used when an optional argument is not provided.
  con @None   // note: must come second; see Core/Core.hs


// ----------------------------------------------------------------------------
// First-class constructor contexts.
// These primitives are used by the compiler for
// _tail recursion module cons_ (TRMC) optimization.
// ----------------------------------------------------------------------------

extern import
  c header-end-file "inline/types-cctx.h"
  js file "inline/types-cctx.js"

extern import
  c file "inline/types-cctx.c"

// _Internal_. Internal type for constructor contexts.
// Holds the address to a field of type `:a` in a constructor.
pub type @field-addr<a>

// _Internal_. Internal function name for field addresses.
pub fip extern @field-addr-of( x : a, conname : string, fieldname : string ) : @field-addr<a>
  c inline "kk_field_addr_null()"
  inline ""  // compiled specially

// First-class constructor context (for _tail recursion modulo cons_ (TRMC) optimization).
// See also ``samples/syntax/contexts`` for further examples.
abstract value type cctx<a,b>
  con @Cctx( res : a, holeptr : @field-addr<b> )

// A first-class constructor context where the hole is of the same type as the root of the structure.
pub alias ctx<a> = cctx<a,a>

// _Internal_. Create a hole for a context
pub inline fip extern @cctx-hole-create() : a
  c  inline "kk_cctx_hole()"
  js inline "undefined"

// _Internal_. Create an initial non-empty context.
pub inline fip extern @cctx-create( x : a, xhole : @field-addr<b> ) : cctx<a,b>
  c   "kk_cctx_create"
  js  "$std_core_types._cctx_create"

// _Internal_. Extend a constructor context with a guaranteed non-empty context
pub inline fip extern @cctx-extend( c : cctx<a,b>, x : b, xhole : @field-addr<c> ) : cctx<a,c>
  c inline "kk_cctx_extend(#1,#2,#3,kk_context())"
  js "$std_core_types._cctx_extend"

// _Internal_. Compose a constructor context with a guaranteed non-empty context
pub inline fip extern @cctx-compose-extend( c1 : cctx<a,b>, c2 : cctx<b,c> ) : cctx<a,c>
  c inline "kk_cctx_extend(#1,#2.res,#2.holeptr,kk_context())"
  js "$std_core_types._cctx_compose"

// Apply a constructor context to its final value (of type `:b`), which is
// put into the hole of the context, and returns the root of the resulting structure `:a`.
// This is a constant time operation for unique contexts.
//
// `(ctx Cons(1,_)) ++ (ctx Cons(_,Nil)) ++. 3 == [1,3]`
pub inline fip extern cctx/(++.)( c : cctx<a,b>, x : b ) : a
  c inline "kk_cctx_apply(#1,#2,kk_context())"
  js "$std_core_types._cctx_apply"

// Compose two constructor contexts.
// This is a constant time operation for unique contexts.
//
// `(ctx Cons(1,_)) ++ (ctx Cons(2,_)) ++. Nil == [1,2]`
pub inline fip extern cctx/(++)( c1 : cctx<a,b>, c2 : cctx<b,c> ) : cctx<a,c>
  c inline "kk_cctx_compose(#1,#2,kk_context())"
  js "$std_core_types._cctx_compose"

// Create an empty context. Usually written directly as an empty context `ctx _`.
pub inline fip extern cctx/empty() : cctx<a,a>
  c  "kk_cctx_empty"
  js "$std_core_types._cctx_empty"


