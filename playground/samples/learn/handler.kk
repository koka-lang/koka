/* Basic examples for `handler` expressions
   See also `samples/handler/basic` for examples of effect handlers in practice.
   Prerequisites:
   - `learn/basic`
   - `learn/with`
*/
module learn/handler

// An effect declaration of type `:example` with
// a single (control) operation `raise`.
effect example
  // raise with a message
  ctl raise( msg :string ) : a

// Functions can use abstract operations like `raise`,
// but it is reflected in the effect type (`:example`).
// Such function requires the effect to be handled at some point.
fun foo(pred) : example int
  if pred then raise("error") else 1


// The basic `handler` expression takes a block
// of operation clauses and returns a handler function
// that function takes a unit-function `action`
// that is run under the handler.
fun example1() : console int
  val h = handler
            ctl raise(msg) { println(msg); 42 }
  h(fn() foo(True))


// The `with` statement (see `learn/with`) can help to
// specify the previous handler more concisely as:
fun example2() : console int
  with handler
    ctl raise(msg) { println(msg); 42 }
  foo(True)


// If there is only one operation, we can shorten this further as:
fun example3() : console int
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

// Mark `:raise` as `final` since it never resumes to the call-site
fun example4() : console int
  with final ctl raise(msg) { println(msg); 42 }
  raise("error")


// A dynamically bound value `ask`
effect val ask : int

// Dynamic binding with static typing!
fun print-ask() : <ask,console> ()
  println(ask)


// Using different handlers for each invocation
fun example5()
  with val ask = 42 in print-ask()
  with val ask = 43 in print-ask()

pub fun main()
  example5()

