/* Basic examples for `handler` expressions
   See also `handler/basic` for examples of effect handlers in practice.
   Prerequisites:
   - `syntax/basic`
   - `syntax/with`
*/
module syntax/handler

effect test 
  ctl raise( msg :string ) : a


/* The basic `handler` expression takes a block
   of operation clauses and return a handler
   function: that function takes a unit-function `action`
   that is run under the handler.
*/
public fun test1() : console int 
  val h = handler 
            ctl raise(msg){ println(msg); 42 }
  h(fn(){ raise("error") })


// The `with` statement (see `syntax/with`) can help to
// specify the previous handler more easily as:
public fun test2() : console int
  with handler 
    ctl raise(msg){ println(msg); 42 }
  raise("error")


fun test2-expanded() : console int {
  (handler{ ctl raise(msg){ println(msg); 42 } })(fn(){ raise("error") })
}


// Moreover, if there is only one operation, we can
// shorten this further as:
public fun test3() : console int
  with ctl raise(msg){ println(msg); 42 }
  raise("error")


// There are three kinds of operations:
// - `ctl`:
//    Most generally is `ctl` which binds implicitly the
//   `resume` function that can be used to resume at the call-site
//   of the operation (see `handlers/basic`). If `resume` is not
//   used in the body, it is an operation that never resumes which
//   is used for exception-like effects.
// - `fun`:
//   `fun op(<args>{ <body> })  ~>  ctl op(<args>){ resume (<body>) }`
//   The `fun` operation clause always resumes once at with the result
//   of the body (we call this _tail resumptive_). This is very efficiently
//   implemented and most operations should use `fun` if possible.
// - `val`:
//   `val op = <expr>  ~> val x = <expr>; ... ; ctl op(){ resume(x) }`
//   An operation that always resumes with a constant. As efficient
//   as `fun` operations.

public fun test4() : console int 
  with ctl raise(msg){ println(msg); 42 }
  raise("error")


effect val ask : int

public fun main()
  with val ask = 42
  println(ask)

