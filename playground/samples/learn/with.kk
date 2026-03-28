/* Shows `with` syntax examples.
   Prerequisites:
   - learn/basic
*/
module learn/with

/* The `with` statement essentially wraps all statements
   following it into a function and passes that as
   the last argument to the `with` expression.
   ````
   with f(e1,...,eN)
   <body>
   ````
   translates to:
   ````
   f(e1,...,eN, fn(){ <body> })
   ````
*/

pub fun twice(f)
  f()
  f()


pub fun example1()
  with twice
  with twice
  println("hi")

// The above is equivalent to:
pub fun example1-expanded()
  twice
    twice
      println("hi")


// Without using layout, the fully expanded version is:
pub fun example1-fully-expanded() {
  twice( fn(){
    twice( fn(){
      println("hi")
    })
  })
}


/* Also, a `with` statement can have a (monomophic lambda) binding:
    ````
    with x <- f(e1,...,eN)
    <body>
    ````
    translates to:
    ````
    f(e1,...,eN, fn(x){ <body> })
    ````
*/

pub fun example2()
  with x <- list(1,10).map
  println(x)


// which is equivalent to:
pub fun example2-expanded()
  list(1,10).map fn(x)
    println(x)


/* Of course, we can use `with` with a `handler` expression */

// a dynamically bound value `ask`
effect val ask : int

fun ask-twice()
  ask + ask

pub fun example3()
  with handler
         val ask = 21
  println(ask-twice())

// Which is the same as:
pub fun example3-expanded()
  (handler{ val ask = 21 }) (fn(){
    println(ask-twice())
  })



/* To make a `with` statement more convenient with handlers
   we can leave out the `handler` keyword.
   ````
   with { <ops> }  ~>  with handler{ <ops> }
   ````
   and for single operations, we get:
   ````
   with val op = <expr>       ~> with handler{ val op = <expr> }
   with fun op(x){ <stats> }  ~> with handler{ fun op(x){ <stats> } }
   with ctl op(x){ <stats> }  ~> with handler{ ctl op(x){ <stats> } }
   ````
*/

pub fun example4()
  with val ask = 21
  println(ask-twice())


pub fun example4-expanded()
  (handler{ val ask = 21 }) (fn(){
    println(ask-twice())
  })

pub fun main()
  example1()
  example2()
  example3()
  example4()

