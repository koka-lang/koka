/* Basic examples for `handler` expressions
   See also `handler/basic` for examples of effect handlers in practice.
   Prerequisites:
   - `syntax/basic`
   - `syntax/with`
*/
module syntax/handler

// An effect declaration of type `:example` with
// a single (control) operation `raise`.
effect example
  ctl raise( msg :string ) : a


/* The basic `handler` expression takes a block
   of operation clauses and return a handler
   function: that function takes a unit-function `action`
   that is run under the handler.
*/
pub fun example1() : console int
  val h = handler
            ctl raise(msg) { println(msg); 42 }
  h(fn() raise("error"))


// The `with` statement (see `syntax/with`) can help to
// specify the previous handler more concisely as:
pub fun example2() : console int
  with handler
    ctl raise(msg) { println(msg); 42 }
  raise("error")


// If there is only one operation, we can shorten this further as:
pub fun example3() : console int
  with ctl raise(msg){ println(msg); 42 }
  raise("error")


// There are three kinds of operations:
// - `ctl`: ("control")
//   Most generally is `ctl` which binds implicitly the
//   `resume` function that can be used to resume at the call-site
//   of the operation (see `handlers/basic`).
//   You can mark a control operation as _final_: `final ctl`.
//   This is used for operations that never resume, like exceptions.
//   and is more efficient as it runs any finalizers eagerly and does
//   need to capture a resumption.
// - `fun`:
//   `fun op(<args>{ <body> })  ~>  ctl op(<args>){ resume (<body>) }`
//   The `fun` operation clause always resumes once at with the result
//   of the body (we call this _tail resumptive_). This is very efficiently
//   implemented and most operations should use `fun` if possible.
// - `val`:
//   `val op = <expr>  ~> val x = <expr>; ... ; ctl op(){ resume(x) }`
//   An operation that always resumes with a constant. As efficient
//   as `fun` operations.

pub fun example4() : console int
  with final ctl raise(msg) { println(msg); 42 }
  raise("error")


// A dynamically bound value `ask`
effect val ask : int

// Dynamic binding with static typing!
fun print-ask() : <ask,console> ()
  println(ask)

// Using different handlers for each invocation
pub fun main()
  with val ask = 42 in print-ask()
  with val ask = 43 in print-ask()


