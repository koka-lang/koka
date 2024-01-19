/*---------------------------------------------------------------------------
  Copyright 2012-2024, Microsoft Research, Daan Leijen.

  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the LICENSE file at the root of this distribution.
---------------------------------------------------------------------------*/

// Standard output to the console.
module std/core/console

import std/core/types
import std/core/unsafe
import std/core/hnd
import std/core/string
import std/core/show

extern import
  js file "inline/console.js"


// The console effect signifies that a function may write to the console.
pub type console :: X


// ----------------------------------------------------------------------------
// Print to the console
// ----------------------------------------------------------------------------

noinline val redirect : ref<global,maybe<(string) -> console ()>> =
  unsafe-total { ref(Nothing) }

// Redirect `print` and `println` calls to a specified function.
noinline fun print-redirect( print : (msg : string) -> console () ) : <ndet,console,st<global>> ()
  redirect := Just(print)

// Print a string to the console, including a final newline character.
extern xprintsln(s : string) : console ()
  c  "kk_println"
  cs "Console.WriteLine"
  js "_println"

// Print a string to the console
extern xprints( s : string) : console ()
  c  "kk_print"
  cs "Console.Write"
  js "_print"

// _Unsafe_. This function removes the state effect from the effect of an action
inline extern unsafe-nostate( action : () -> <st<h>,console> a ) : (() -> console a)
  inline "#1"

noinline fun prints( s : string ) : console ()
  (unsafe-nostate
    match !redirect
      Nothing -> xprints(s)
      Just(f) -> f(s)
  )()

noinline fun printsln( s : string ) : console ()
  (unsafe-nostate
    match !redirect
      Nothing -> xprintsln(s)
      Just(f) -> { f(s ++ "\n") }
  )()



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
