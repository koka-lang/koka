// Implicit evidence raised inside a recursive group must be discharged by the
// group. Reading a global reference that holds a function raises an `hdiv`
// constraint; the members of a recursive group are inferred without being
// generalized one by one, and the group itself was quantified without solving
// its pending constraints, so the evidence was never bound and compilation
// stopped with "internal error: no empty iconstraints".
import std/core/unsafe
import std/core/undiv

val cb : ref<global, maybe<(int) -> ioc ()>> = unsafe-total{ ref(Nothing) }

// a self-recursive function reading the reference (the original counterexample)
fun drain( n : int ) : <ioc,div> ()
  match !cb
    Just(f) ->
      f(n)
      if n > 0 then drain(n - 1)
    Nothing -> ()

// mutual recursion: each member binds the evidence it uses
fun ping( n : int ) : <ioc,div> ()
  match !cb
    Just(f) -> { f(n); if n > 0 then pong(n - 1) }
    Nothing -> ()

fun pong( n : int ) : <ioc,div> ()
  match !cb
    Just(f) -> { f(0 - n); if n > 0 then ping(n - 1) }
    Nothing -> ()

// a recursive local function reads the reference, and so does the enclosing
// function: the local group must not capture the enclosing scope's evidence
fun outer( k : int ) : <ioc,div> ()
  fun loop( n : int ) : <ioc,div> ()
    match !cb
      Just(f) -> { f(n); if n > 0 then loop(n - 1) }
      Nothing -> ()
  loop(k)
  match !cb
    Just(f) -> f(100)
    Nothing -> ()

// an unannotated recursive local function (its effect is inferred)
fun outer2( k : int ) : <ioc,div> ()
  fun loop(n)
    match !cb
      Just(f) -> { f(n); if n > 0 then loop(n - 1) }
      Nothing -> ()
  loop(k)

pub fun main()
  cb := Just(fn(i) println(i.show))
  pretend-no-div
    drain(2)
    ping(3)
    outer(1)
    outer2(1)
