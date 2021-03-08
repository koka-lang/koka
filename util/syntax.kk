/*-----------------------------------------------------------------------------
Copyright 2020 Microsoft Corporation, Daan Leijen.
This is free software; you can redistribute it and/or modify it under the
terms of the Apache License, Version 2.0. A copy of the License can be
found in the file "license.txt" at the root of this distribution.

Install Atom and VSCode packages locally
-----------------------------------------------------------------------------*/

import std/os/path
import std/os/dir
import std/os/process

fun main() {
  if (!is-file("util/syntax.kk".path)) {
    println("error: you can only run 'util/syntax' from the koka root directory.")
    return ()
  }

  val home = homedir()
  val atom = home ++ ".atom/packages".path
  
  if (!is-directory(atom)) {
    println("atom is not found, skipping.")
  }
  else {
    println("installing atom package to: " ++ atom.string)
    copy-directory("support/atom".path, atom ++ "language-koka".path)
    println("installed atom support -- restart atom to take effect")
  }

  val vscode = home ++ ".vscode/extensions".path
  if (!is-directory(vscode)) {
    println("vscode is not found, skipping.")
  }
  else {
    println("installing vscode package to: " ++ vscode.string)
    copy-directory("support/vscode".path, vscode)
    println("installed vscode support -- restart vscode to take effect")
  }

  println("done.")
}
