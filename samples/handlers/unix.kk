// Based on Daniel Hillerström's PhD Thesis,
// "Foundations for Programming and Implementing Effect Handlers" (chap. 2)
// <https://www.dhil.net/research/papers/thesis.pdf>
// which shows how to build an OS API as a composition of (orthogonal) effect handlers.
//
// The terminology and design is based on:
// "The UNIX Time- Sharing System" by Dennis M. Ritchie and Ken Thompson
// https://dsf.berkeley.edu/cs262/unix.pdf


// -----------------------------------------
// Basic I/O
// Shows output state (writer monad)

effect bio  // basic I/O
  fun write( fd : filedesc, s : string ) : ()

alias filedesc = int

val stdout = 0

fun echo( s : string ) : bio ()
  write(stdout,s)

fun bio( action : () -> <bio|e> a ) : e (a,string)
  var buf := ""
  with return(x) (x,buf)
  with fun write(fd,s) buf := buf ++ s
  action()

fun example1()
  with bio
  echo("hi ") 
  echo("unix world")


// -----------------------------------------
// Exit
// Show non-linear control by exiting a computation. (exception monad)

effect exit 
  ctl exit( exitcode : int ) : a

fun status( action : () -> <exit|e> a ) : e int
  with ctl exit(code) code
  action()
  0

fun example2() : (int,string)
  with bio
  with status
  echo("hi ")
  exit(1)
  echo("unix world")


// -----------------------------------------
// User environment
// Show dynamic binding  (reader monad)

type user
  Root
  Alice
  Bob

fun show( user : user ) : string
  match user
    Root  -> "root"    
    Alice -> "alice"
    Bob   -> "bob"

effect whoami
  fun whoami() : string



fun env( user : user, action : () -> <whoami|e> a ) : e a
  with fun whoami() show(user)    
  action()

fun example3()
  with bio
  with status
  with env(Alice)
  echo("hi ")
  echo(whoami())
  

fun show( (_,s) : ((),string) ) : string
  show(s)

fun show( (i,s) : (int,string) ) : string
  "exit with status " ++ i.show ++ "\n" ++ show(s)  


// -----------------------------------------
// Session management
// su: substitute user

effect su 
  ctl su( u : user ) : ()

fun session-manager1( initial-user : user, action : () -> <su,whoami|e> a ) : e a
  with env(initial-user)
  with ctl su( u : user ) 
         mask<whoami>
           with env(u) 
           resume(())
  action()

fun session-manager2( initial-user : user, action : () -> <su,whoami|e> a ) : e a
  with fun whoami() show(initial-user)
  with ctl su( u : user ) 
         with override fun whoami() show(u)
         resume(())
  action()

fun session-manager3( initial-user : user, action : () -> <su,whoami|e> a ) : e a
  var user := initial-user
  with fun whoami() show(user)
  with fun su(u) user := u
  action()


fun example4()
  with bio
  with status
  with session-manager3(Root)
  echo("hi ")
  echo(whoami())
  su(Alice)
  echo(", and hi ")
  echo(whoami())
  

// -----------------------------------------
// Multitasking
// 

effect fork 
  ctl fork() : bool     // true if this is the parent


fun forking( action : () -> <fork|e> a ) : e list<a> 
  with handler 
    return(x) [x]
    ctl fork() resume(True) ++ resume(False)
  action()


type pstate<e,a>
  Done(result : a)
  Paused(resumption : () -> e pstate<e,a> )

effect interrupt
  ctl interrupt() : ()

fun reify-process( action : () -> <interrupt|e> a ) : e pstate<e,a> 
  with raw ctl interrupt() Paused( fn() rcontext.resume(()) )
  Done(action())

fun scheduler( pstates : list<pstate<<fork,div|e>,a>> ) : <div|e> list<a>
  fun schedule( todos : list<pstate<<fork,div|e>,a>>, dones : list<a> ) : <div|e> list<a>
    match todos
      Nil -> dones
      Cons(Done(x),pp) -> schedule(pp, Cons(x,dones))
      Cons(Paused(p),pp) -> 
        val ps = forking( p )
        schedule( pp ++ ps, dones )
  schedule(pstates,[])

fun timeshare( action : () -> <fork,interrupt,div|e> a ) : <div|e> list<a>
  val p = Paused( fn() reify-process(action) )
  scheduler([p])


fun ritchie() : <bio|e> ()
  echo("UNIX is basically ")
  echo("a simple operating system, ")
  echo("but ")
  echo("you have to be a genius to understand the simplicity.\n")

fun hamlet() : <bio|e> () 
  echo("To be, or not to be, ")
  echo("that is the question:\n")
  echo("Whether 'tis nobler in the mind to suffer\n")

fun example5()
  with return(x:(list<int>,string)) x.snd.println
  with bio
  with timeshare
  with status
  with session-manager3(Root)
  if fork() then 
    su(Alice)
    ritchie()
  else
    su(Bob)
    hamlet()


fun interrupt-write( action : () -> <bio,interrupt|e> a ) : <bio,interrupt|e> a
  with override fun write(fd,s) { interrupt(); write(fd,s) }
  action()

fun example6()
  with return(x:(list<int>,string)) x.snd.println
  with bio
  with timeshare
  with interrupt-write
  with status
  with session-manager3(Root)
  if fork() then 
    su(Alice)
    ritchie()
  else
    su(Bob)
    hamlet()

