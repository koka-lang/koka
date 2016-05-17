//-------------------------------------------------------------
//
//-------------------------------------------------------------

effect await<a> {
  await() : a
}

effect send<a> {
  send( x : a ) : ()
}

function down(prod : () -> <send<c>,await<c>|e> a, action : () -> <await<c>,send<c>|e> a) : <send<c>,await<c>|e> a {
  handle shallow(action) {
    return x -> x
    await()  -> up(resume){ prod() } 
  }
}

function up(cons : (c) -> <await<c>,send<c>|e> a, action : () -> <send<c>,await<c>|e> a) : <send<c>,await<c>|e> a {
  handle shallow(action) {
    return x -> x
    send(x)  -> down{ resume(()) }{ cons(x) } 
  }
}

function producer() {
  send(1)
  send(2)
  0
}

function consumer() {
  val x = await()
  if (x > 0) then await() + 1 else await() - 1
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
  unsend{ unawait{ down( producer, consumer ) } } 
}