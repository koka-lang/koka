// Based on Daniel Hillerström PhD Thesis,
// "Foundations for Programming and Implementing Effect Handlers" (chap. 2)
// <https://www.dhil.net/research/papers/thesis.pdf>
//
// which is based on
//
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

effect whoami
  fun whoami() : string

fun env( user : user, action : () -> <whoami|e> a ) : e a
  with fun whoami() match user
                      Root  -> "root"    
                      Alice -> "alice"
                      Bob   -> "bob"
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
