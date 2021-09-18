/* Shows `with` syntax examples.
   Prerequisites:
   - syntax/basic
   - syntax/handler
*/
module syntax/with

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

fun twice(f) 
  f()
  f()


public fun test1() 
  with twice
  with twice
  println("hi")


fun test1-expanded()
  twice
    twice
      println("hi")


// Without using layout, the fully expanded version is:
fun test1-fully-expanded() 
  twice( fn(){
    twice( fn(){
      println("hi")
    })
  })
}


/* Also, a `with` statement can have a (monomophic lambda) binding:
    ````
    with x = f(e1,...,eN)
    <body>
    ````
    translates to:
    ````
    f(e1,...,eN, fn(x){ <body> })
    ````
*/

public fun test2()
  with x = list(1,10).map
  println(x)


fun test2-expanded()
  list(1,10).map( fn(x) {
    println(x)
  })


/* Of course, we can use `with` with a `handler` expression */

// a dynamically bound value `ask`
effect val ask : int

fun ask-twice() 
  ask + ask


public fun test3() 
  with handler{ val ask = 21 }
  println(ask-twice())


fun test3-expanded() 
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
   with val op = <expr>           ~> with handler{ val op = <expr> }
   with fun op(x){ <stats> }      ~> with handler{ fun op(x){ <stats> } }
   with control op(x){ <stats> }  ~> with handler{ control op(x){ <stats> } }
   ````
*/

public fun test4() 
  with val ask = 21
  println(ask-twice())


fun test4-expanded()
  (handler{ val ask = 21 }) (fn(){
    println(ask-twice())
  })

public fun main() 
  test1()
  test2()
  test3()
  test4()

