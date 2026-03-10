// phantom.kk — Phantom Implicit Parameters
//
// "Syntactic Implicit Parameters with Static Overloading"
// Section 4.3: Phantom Implicits
//
// A "phantom implicit" is an implicit parameter whose name has a special
// compiler-provided meaning. When the name is not in scope at the call site,
// the compiler supplies a value automatically rather than rejecting the program.
//
// Koka defines (in std/core/debug):
//   ?kk-line : string   — current source line number (as a string)
//   ?kk-file : string   — current source file name
//
// Note: The paper describes ?kk-line as `:int` for illustration; the actual
// Koka implementation uses `:string` for both `kk-line` and `kk-file`.
//
// Unlike a preprocessor macro, these can be abstracted over: if ?kk-line is
// already in scope (as a real parameter), the compiler passes it along instead
// of providing the call-site value.
//
// Run with: stack run koka -- -e test/artifact/pldi26/examples/phantom.kk

module phantom

import std/core/debug   // provides kk-line, kk-file, and kk-file-line phantom implicit names


// ─────────────────────────────────────────────────────────────────────────────
// Basic phantom implicit: ?kk-line (as a string in the actual implementation)
//
// The paper shows ?kk-line:int for simplicity; Koka uses ?kk-line:string.
// At the call site `assert-line(False, "failed")`, the compiler elaborates to
//   assert-line(False, "failed", ?kk-line="<current-line-number>")
// because `kk-line` is not defined at the call site.
// ─────────────────────────────────────────────────────────────────────────────

fun assert-line( condition : bool, msg : string, ?kk-line : string ) : exn ()
  if condition then () else throw("at line " ++ ?kk-line ++ ": " ++ msg)


// ─────────────────────────────────────────────────────────────────────────────
// Abstracting over phantom implicits: ?kk-line is passed through
//
// assert-fline accepts both ?kk-line and ?kk-file as implicits.
// Inside the body, the call to assert-line is elaborated to:
//   assert-line(condition, ?kk-file ++ ": " ++ msg, ?kk-line=?kk-line)
// because `kk-line` IS in scope (as a parameter), so it's threaded through.
//
// At the call site of assert-fline, both are not in scope, so the compiler
// provides the call-site values.
// ─────────────────────────────────────────────────────────────────────────────

fun assert-fline( condition : bool, msg : string, ?kk-line : string, ?kk-file : string ) : exn ()
  assert-line(condition, ?kk-file ++ ": " ++ msg)


// ─────────────────────────────────────────────────────────────────────────────
// Using the combined kk-file-line phantom (as the stdlib does)
//
// `?kk-file-line : string` combines file and line into a single string.
// This is how `assert`, `impossible`, `trace-info`, etc. work in std/core/debug.
// ─────────────────────────────────────────────────────────────────────────────

fun my-assert( condition : bool, msg : string, ?kk-file-line : string ) : exn ()
  // ?kk-file-line is threaded through from our caller: the compiler supplies
  // "file(line)" at the outermost call site where kk-file-line is not in scope.
  if condition then () else throw(?kk-file-line ++ ": assertion failed: " ++ msg)

fun main() : <exn, console> ()
  // At these call sites, kk-line, kk-file, and kk-file-line are not in scope,
  // so the compiler supplies the current line/file values automatically.
  assert-line(True, "should not fail")
  assert-fline(True, "should not fail")
  my-assert(True, "should not fail")
  println("All phantom implicit assertions passed.")
  // Uncomment to see the location reported:
  // assert-line(False, "deliberate failure")
