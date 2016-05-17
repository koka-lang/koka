//-------------------------------------------------------------
//
//-------------------------------------------------------------

effect await<a> {
  await() : a
}

effect send<a> {
  send( x : a ) : ()
}

function down(prod : () -> <send<c>,await<c>,cps|e> a, action : () -> <await<c>,send<c>,cps|e> a) : <send<c>,await<c>,cps|e> a {
  handle shallow(action) {
    return x -> x
    await()  -> {cps(); up(resume){ prod() } }
  }
}

function up(cons : (c) -> <await<c>,send<c>,cps|e> a, action : () -> <send<c>,await<c>,cps|e> a) : <send<c>,await<c>|e> a {
  handle shallow({uncps(action)}) {
    return x -> x
    send(x)  -> down{ resume(()) }{ cons(x) }
  }
}

function producer() {
  send(1)
  send(2)
}

function consumer() {
  val x = await()
  if (x > 0) then await() + 1 else await() - 1
  ()
}

val unawait = handler {
  return x -> x
  await()  -> error("unresolved await")
}


val unsend = handler {
  return x -> x
  send(x)  -> error("unresolved send")
}

function main() {
  uncps{ unsend{ unawait{ down( producer, consumer ) } } }
}