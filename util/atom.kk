import std/os/path
import std/os/file

fun main() {
  if (!is-file("support/atom/package.json".path)) {
    println("error: you can only run 'util/atom' from the koka root directory.")
    return ()
  }
  
  val home = homedir()
  val atom = home + ".atom/packages".path  
  println("installing atom package to: " + atom.string)
  
  if (!is-directory(atom)) {
    println("error: cannot find the atom package directory.")
    return ()
  }
  
  copy-directory("support/atom".path, atom + "language-koka".path)
  println("done installing.")
}
