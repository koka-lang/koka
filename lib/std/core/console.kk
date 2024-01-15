module std/core/console

import std/core/types
import std/core/hnd

extern import
  js file "console-inline.js"


// The console effect signifies that a function may write to the console.
pub type console :: X


// ----------------------------------------------------------------------------
// Print to the console
// ----------------------------------------------------------------------------

noinline val redirect : ref<global,maybe<(string) -> console ()>> =
  unsafe-total { ref(Nothing) }

// redirect `print` and `println` calls to a specified function.
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

// _Unsafe_. This function removes the non-termination effect (`:div`) from the effect of an action
pub inline extern unsafe-nostate( action : () -> <st<h>,console> a ) : (() -> console a)
  inline "#1"

pub noinline fun prints( s : string ) : console ()
  (unsafe-nostate
    match !redirect
      Nothing -> xprints(s)
      Just(f) -> f(s)
  )()

pub noinline fun printsln( s : string ) : console ()
  (unsafe-nostate
    match !redirect
      Nothing -> xprintsln(s)
      Just(f) -> { f(s); f("\n") }
  )()
