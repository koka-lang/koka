/* Basic examples for `handler` expressions
   See also `handler/basic` for examples of effect handlers in practice.
   Prerequisites:
   - `syntax/basic`
   - `syntax/with`
*/
module syntax/handler

effect test {
  control raise( msg :string ) : a
}

/* The basic `handler` expression takes a block
   of operation clauses and return a handler
   function: that function takes a unit-function `action`
   that is run under the handler.
*/
pub fun test1() : console int {
  val h = handler {
            control raise(msg){ println(msg); 42 }
          }
  h( fn(){
    raise("error")
  })
}


// The `with` statement (see `syntax/with`) can help to
// specify the previous handler more easily as:
pub fun test2() : console int {
  with handler {
    control raise(msg){ println(msg); 42 }
  }
  raise("error")
}

fun test2-expanded() : console int {
  (handler{ control raise(msg){ println(msg); 42 } })(fn(){ raise("error") })
}


// Moreover, if there is only one operation, we can
// shorten this further as:
pub fun test3() : console int {
  with control raise(msg){ println(msg); 42 }
  raise("error")
}

// There are four kinds of operations:
// - `control`:
//    Most generally is `control` which binds implicitly the
//   `resume` function that can be used to resume at the call-site
//   of the operation (see `handlers/basic`)
// - `except`:
//   `except op(<args>){ <body> }  ==  control op(<args>){ <body> }`
//   is equivalent to control but does not bind `resume` and thus
//   can never resume. It is more efficient than general `control`.
// - `fun`:
//   `fun op(<args>{ <body> })  ==  control op(<args>){ resume (<body>) }`
//   The `fun` operation clause always resumes once at with the result
//   of the body (we call this _tail resumptive_). This is very efficiently
//   implemented and most operations should use `fun` if possible.
// - `val`:
//   `val op = <expr>  == val x = <expr>; ... ; control op(){ resume(x) }`
//   An operation that always resumes with a constant. As efficient
//   as `fun` operations.

pub fun test4() : console int {
  with except raise(msg){ println(msg); 42 }
  raise("error")
}

effect val ask : int

pub fun main() {
  with val ask = 42
  println(ask)
}
