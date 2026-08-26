module std/core/cextern

import std/num/int32
import std/core/types
import std/core/int

extern import
  c header-file "inline/cextern.h"

// TIM: Some thoughts / notes on wrapping C libraries and C / extern memory management

// C structs / memory has the following allocation / ownership behavior for function calls

// Parameters to C functions
// - Passed as value: 
//   + Caller is not responsible for the struct's memory anymore, but could be responsible for embedded pointers
//   + Allocation of the struct itself is stack based, and unimportant for memory management
// - Passed as pointer
//   + Could be stack or heap allocated pointer, and could be caller owned or transfer of ownership to callee (for heap allocated references)
//   + If ownership is transferred the caller should not have any other references to the memory, and should not use it anymore.
// Return values from C functions
// - As value
//   + Caller is responsible for continuing to hold onto the important pieces by value
//   + or if the struct escapes the stack / is returned by value the caller needs to allocate + own (at least the outer struct)
// - As pointer
//   + Could be stack or heap allocated pointer, and could be callee owned or transferred to caller (for heap allocated references)
//   + If ownership is transferred to caller then the caller needs to become responsible for deallocation 
//   + Unlikely to be a stack allocated pointer, unless it is of one that we have access to otherwise, (e.g. a pointer into an array we allocated on the stack)

// In summary we have values, stack references and heap references. Following covers the difference between these.

// Stack allocated values
//   The normal value types of Koka could be used with no changes, other than the object header, which would require a boxing step, which happens anyways. 
//   We'd just need to specialize the boxing step for this kind of memory.
//   Maybe we could introduce a `raw struct` variant - to ensure native (C) ordering / representation, and restrict fields to native compatible types
//   Raw structs embedded within other structs are boxed
//   
// Stack allocated references
// Could we reuse Koka's `local<h>` effect, maybe `local-raw<h>` to prevent scope escaping? 
//   For example we could have a function that like `local-new` returns a local that cannot escape the stack - but for a `raw value` type.
// 
// Stack allocation: 
//   On the parameter side, we would need to be able to pass the struct by value or by reference
//   We could also have a `.ref` to get the reference to the stack location, and a `.value` to copy a stack allocated ref into the value type. 
//   Return types from C functions could introduce the `local-raw<h>` effect tied to the return value (for reference types), and transparently represent `raw struct`s.
//   Stack allocation is similar to the concept of Rust lifetimes. However, I expect that Koka's lifetime will always be stack related (region-like?) and not infinitely relatable to other objects (other than through invariant type polymorphism?)

// Heap allocated raw types (all heap types are references)
//   In the heap we have two main concepts owned and borrowed
//   - The owner is in charge of determining the lifetime and deallocating the pointer
//   - A borrower should not deallocate the pointer, and needs to know that the memory is still valid

// Owned references:
//   This one is somewhat easy for Koka. 
//   We can use a reference counting wrapper to determine how many references to the pointer are from Koka
//   The C object might have specific deallocation / cleanup functions to call, and might have been allocated with a different allocator.
//   Unfortunately the wrapper won't transparently pass through the C code.
//   Function calls will require domain specific understanding of the APIs and whether C maintains it's own references or not.
//   If the reference is duplicated and both sides can access it (shared ownership), the `dup` / `drop` calls needs to happen in a domain specific manner.
//     Using borrow annotations on the parameters should help to handle this, but the C code needs the raw pointer, so we also want to unwrap it when we generate the binding code.
//     Also we need to defer the `drop` call to after the C function is run so that the memory is still accessible to the function.
//   If the reference should be accessible by only one side at a time, and ownership should be passed, we need to guard calls to C by making sure the reference count is one when handing it off.
//     This is where we would need some sort of static reference counting / escape analysis at compile time to make a guarantee that this won't cause an error.
//   When C passes an owned reference to us that is much easier, we do as normal and wrap it up. Again, some automatic wrapping would be nice here.

// Borrowed references: These are really just a special case of owned references, with specific patterns
//   C code might want to borrow a reference, this is fine with a Koka owned reference, as long as we know when the C code is done with it and we can `drop` the reference count.
//   C code might give us a reference (e.g. in a callback) that we should not own permanently, which it is in charge of cleaning up (likely after the function returns).
//     In this case, treating it as a stack allocated region limited reference is the appropriate thing to do, with maybe some restrictions / warnings on copying it into a value?
//     Some fields might be okay or intended for you to copy / own, and some may not.

// I imagine that most C interfaces follow these common patterns:
//  - Owned handles with create() and destruct() functions, with the user of the library in charge of cleaning up & lifetime as long as the user wants.
//  - Passing / returning by value
//  - Taking in a stack allocated reference (i.e. calls where the C code doesn't store the reference long term)
//  - Embedded owned handles. (Composition of heap allocated references)

// The tricky part is likely not any of the basics, but rather dealing with nested / nullable fields / mutation.
// Rust is complicated because of lifetime annotation equivalence, upper bounds, multiple lifetimes, etc. 


// A managed koka pointer to C memory
// Owned values can be freely passed around, captured in lambdas, escape their scope, etc.
pub alias owned-c<t> = extern-owned<t>

// A borrowed koka pointer to C memory during a scope s
// Borrowed values are only guaranteed valid during their scope, should not escape the scope
// For example an `owned-c<array<t>>` should only allow borrowed references to the middle of an array during a scope where the owned pointer will not be dropped
pub alias borrowed-c<s::S,t> = extern-borrowed<t>

// A raw pointer to C memory
pub type c-pointer<t>

// An opaque type to designate c-array types in Koka
pub type c-array<t>

// A type alias for a null pointer of type `t`
pub alias c-null<t> = c-pointer<t>

// The null pointer in C
pub inline extern cnull(): c-null<t>
  c inline "(intptr_t)0"

// A null callback pointer
pub val null-cb = 0.intptr_t

// Needs to be extern otherwise, the compiler will optimize out the function and not keep the reference alive
// Ensure that a reference is > 1 until after this point
pub extern owned/keepalive(^s: owned-c<a>): <> ()
  ""

// Release a reference (decref an owned reference)
pub fun owned/release(s: owned-c<a>): <> ()
  ()

// Retain a reference (incref an owned reference)
pub extern owned/retain(s: owned-c<a>): <> ()
  ""

pub inline extern int/ptr<t>(i: intptr_t): c-pointer<t>
  c inline "(intptr_t)#1"

pub inline extern carray/intptr<t>(c: c-array<t>): intptr_t
  c inline "#1"

pub inline extern carray/ptr<t>(c: c-array<t>): c-pointer<t>
  c inline "#1"

pub inline extern intptr/carray<t>(c: intptr_t): c-array<t>
  c inline "#1"

pub inline extern ptr/carray<t>(c: c-pointer<t>): c-array<t>
  c inline "#1"

// Allow casting between different types of pointers
pub inline extern unsafe/cptr-cast<s>(c: c-pointer<t>): c-pointer<s>
  c inline "#1"

// Allocate `n*size-of` bytes of memory using kk_malloc and return a pointer to the allocated memory
inline extern int/malloc<t>(n: int32, size-of: int32): c-pointer<t>
  c inline "(kk_addr_t)kk_malloc(#1*#2, kk_context())"

// Allocate `n*size-of` bytes of memory using C's malloc and return a pointer to the allocated memory
inline extern int/malloc-c<t>(n: int32, size-of: int32): c-pointer<t>
  c inline "(kk_addr_t)malloc(#1*#2)"

// Allocate a single element of type `t` using `kk_malloc` and return a managed pointer
// Type `t` should:
// - Be an opaque type in Koka corresponding to a C type (e.g. `pub type cstruct` with no members)
// - Have a `size-of` function that returns the size of the structure in bytes
pub inline fun single/malloc<t>(?size-of: (c-null<t>) -> int32): owned-c<t>
  int/malloc(1.int32, size-of(cnull())).c-own-extern

// Allocate `n` elements of type `t` using `kk_malloc` and return a managed pointer to the array
// Type `t` should:
// - Be an opaque type in Koka corresponding to a C type (e.g. `pub type cstruct` with no members)
// - Have a `size-of` function that returns the size of the structure in bytes
pub inline fun array/malloc<t>(n: int32, ?size-of: (c-null<t>) -> int32): owned-c<c-array<t>>
  int/malloc(n, size-of(cnull())).c-own-extern

// Allocate a single element of type `t` using C's `malloc` and return a managed pointer
// Type `t` should:
// - Be an opaque type in Koka corresponding to a C type (e.g. `pub type cstruct` with no members)
// - Have a `size-of` function that returns the size of the structure in bytes
pub inline fun single/malloc-c<t>(?size-of: (c-null<t>) -> int32): owned-c<t>
  int/malloc-c(1.int32, size-of(cnull())).c-own-free-calloc-extern

// Allocate `n` elements of type `t` using C's `malloc` and return a managed pointer to the array
// Type `t` should:
// - Be an opaque type in Koka corresponding to a C type (e.g. `pub type cstruct` with no members)
// - Have a `size-of` function that returns the size of the structure in bytes
pub inline fun array/malloc-c<t>(n: int32, ?size-of: (c-null<t>) -> int32): owned-c<c-array<t>>
  int/malloc-c(n, size-of(cnull())).c-own-free-calloc-extern

// !!!WARNING!!! UNSAFE API
// Allocate `n` elements of type `t` using `kk_malloc` and return a managed pointer to the array
// Type `t` should:
// - Be an opaque type in Koka corresponding to a C type (e.g. `pub type cstruct` with no members)
// - Have a `size-of` function that returns the size of the structure in bytes
//
// NOTES:
// Prefer using `array/malloc` or `single/malloc` instead of this function which return a managed pointer.
// Raw `c-pointer<t>` should be used in low-level generated koka ffi functions since the pointer is unknown to be managed or not.
// Conversion routines for `owned-c<t>` and `borrowed-c<s,t>` then should be used to get the raw pointers to be used in the ffi functions
// Higher level apis to c libraries should then provide an interface using `owned-c<t>` and `borrowed-c<s,t>` instead of `c-pointer<t>`
pub inline fun ptr/unsafe-malloc<t>(n: int32, ?size-of: (c-null<t>) -> int32): c-pointer<t>
  int/malloc(n, size-of(cnull()))

// !!!WARNING!!! UNSAFE API
// Allocate `n` elements of type `t` using C's `malloc` and return a managed pointer to the array
// Type `t` should:
// - Be an opaque type in Koka corresponding to a C type (e.g. `pub type cstruct` with no members)
// - Have a `size-of` function that returns the size of the structure in bytes
//
// NOTES:
// Prefer using `array/malloc-c` or `single/malloc-c` instead of this function which return a managed pointer.
// Raw `c-pointer<t>` should be used in low-level generated koka ffi functions since the pointer is unknown to be managed or not.
// Conversion routines for `owned-c<t>` and `borrowed-c<s,t>` then should be used to get the raw pointers to be used in the ffi functions
// Higher level apis to c libraries should then provide an interface using `owned-c<t>` and `borrowed-c<s,t>` instead of `c-pointer<t>`
pub inline fun ptr/unsafe-malloc-c<t>(n: int32, ?size-of: (c-null<t>) -> int32): c-pointer<t>
  int/malloc-c(n, size-of(cnull()))

// Transform a C ptr into a managed koka value, which will be freed by `kk_free` when koka's reference count reaches 0
inline extern c-own-extern(c: c-pointer<a>): owned-c<a>
  c inline "kk_cptr_raw_box(&kk_free_fun, (void *)#1, kk_context())"

// Transform a C ptr into a managed koka value, which will be freed by C's `free` when koka's reference count reaches 0
inline extern c-own-free-calloc-extern(c: c-pointer<a>): owned-c<a>
  c inline "kk_cptr_raw_box(&kk_free_calloc, (void *)#1, kk_context())"

// Transform a C ptr `c` into a koka value that holds the c reference without freeing it
// The pointer should be valid for the duration of the callback `f`.
inline extern c-borrow(c: c-pointer<t>, f: forall<s> borrowed-c<s,t> -> e a): e a
  c "kk_borrow_ptr"

// !!!WARNING!!!: Extremely unsafe API (needed for `c-borrow`), get approval to use anywhere else.
inline extern unsafe-cast<s>(b: c-pointer<t>): borrowed-c<s,t>
  c inline "#1"

// Transform an unmanaged C ptr into a managed koka reference to C memory
// Ensure the pointer is not going to be freed by C code, otherwise use `c-borrow` instead
// Also ensure the memory was allocated using `kk_malloc`
pub inline fun c-own<t>(t: c-pointer<t>): owned-c<t>
  t.c-own-extern

// Transform an unmanaged C ptr into a managed koka reference to C memory
// Ensure the pointer is not going to be freed by C code, otherwise use `c-borrow` instead
// Also ensure the memory was allocated using C's `malloc`
pub fun c-own-free-calloc<t>(t: c-pointer<t>): owned-c<t>
  t.c-own-free-calloc-extern

// Transform a koka `owned-c` managed pointer into a C ptr 
// Keeps the koka reference alive during the scope of the callback `f`
pub inline extern owned/with-ptr(^t: owned-c<t>, f: c-pointer<t> -> e a): e a
  c "kk_owned_with_ptr"

// Transform a koka `borrowed-c` managed pointer into a C ptr
// Keeps the koka reference alive during the scope of the callback `f`
pub inline extern borrowed/with-ptr(^t: borrowed-c<s,t>, f: c-pointer<t> -> e a): e a
  c "kk_borrowed_with_ptr"

// !!!WARNING!!! Extremely UNSAFE API
// Get the raw C pointer from a `borrowed-c` managed pointer to use immediately in an ffi function
// This doesn't return a typed pointer, and accepts any boxed type as input, so it is very dangerous
//   Use `borrowed/with-ptr` most of the time and
//       `borrow/use-ffi-ptr` if directly passing to an safe ffi call
pub inline extern unsafe-borrowed-ffi-ptr-extern<t>(c: borrowed-c<s,t>): c-pointer<t>
  c inline "(kk_addr_t)kk_cptr_unbox_borrowed(#1, kk_context())"

// !!!WARNING!!! UNSAFE API
// Get the raw C pointer from an `borrowed-c` managed pointer to use immediately in an ffi function
// Not safe to pass around Koka code
// However, since an immediate use is still within the scope of the `borrowed-c` it is safe
//   This is due borrowed pointers being guaranteed to be valid during their whole scope (the lambda enclosing the call to this method) 
//   A similar api for `owned-c` is not possible since converting an owned pointer to a raw pointer could allow the owned pointer to be freed if this was its last use
//   For owned pointers use `owned/with-ptr` instead
pub inline fun borrow/use-ffi-ptr<t>(c: borrowed-c<s,t>): c-pointer<t>
  c.unsafe-borrowed-ffi-ptr-extern

// Transform a koka `owned-c` managed pointer to an array into a C ptr pointing to the element at index `idx` of type `t` and size `size-of(cnull())`
// Keeps the koka reference alive during the scope of the callback `f`
// This is guaranteed due to be this being an external function (`f` is not inlineable), and `t` being borrowed
pub inline extern offset/with-ptr(^t: owned-c<c-array<t>>, idx: ssize_t, f: c-pointer<t> -> e a, size-of: int32): e a
  c "kk_owned_with_ptr_idx"

// Transform a koka `owned-c` managed pointer to an array into a C ptr pointing to the element at index `idx` of type `t` and size `size-of(cnull())`
// Keeps the koka reference alive during the scope of the callback `f`
pub inline fun c-array/with-ptr(t: owned-c<c-array<t>>, idx: ssize_t, f: forall<s> borrowed-c<s,t> -> e a, ?size-of: (c-null<t>) -> int32): e a
  offset/with-ptr(t, idx, fn(p) c-borrow(p, f), size-of(cnull()))

// Transform an assumed pointer to a C string into a Koka string
// Copies the memory
pub inline extern ptr/to-string(ptr: c-pointer<int8>): string
  c inline "kk_string_alloc_raw((const char *)#1, false, kk_context())"

// Transform an assumed pointer to a C string of length len into a Koka string
// Copies the memory
// Assume the array is non-null terminated and adds the terminating character
pub inline extern strlen-ptr/to-string(ptr: c-pointer<int8>, len: int64): string
  c inline "kk_string_alloc_raw_buff(#2, (char *)#1, false, kk_context())"

// Borrows the c pointer to a koka managed string for the duration of the callback `f`
inline extern ptr/with-c-string(^s: string, f: c-pointer<int8> -> e a): e a
  c "kk_with_c_string"

// Borrows the c pointer to a koka managed string for the duration of the callback `f`
pub inline fun cptr/with-c-string(^s: string, f: forall<s> borrowed-c<s,int8> -> e a): e a
  with-c-string(s, fn(p) c-borrow(p, f))
