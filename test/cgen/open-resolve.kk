import std/os/path
import std/os/env

effect val package-root: path

fun main()
  println("Expecting :" ++ (cwd() / ".kokac").show)
  with set-package-root(cwd())
  println(package-root)

noinline fun set-package-root(root: path, f: () -> <package-root,ndet|e> a): <ndet|e> a
  with val package-root = root / ".kokac"
  f()
