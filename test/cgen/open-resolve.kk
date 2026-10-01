// The call to `set-root` opens `main`'s effect row into one with a different tail.
import std/os/path

effect val root : path

fun main()
  with set-root(cwd())
  println(root.nodir)

noinline fun set-root( r : path, f : () -> <root,ndet|e> a ) : <ndet|e> a
  with val root = r / "b"
  f()
