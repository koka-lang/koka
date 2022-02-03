/*-----------------------------------------------------------------------------
Copyright 2020-2021, Microsoft Research, Daan Leijen.
This is free software; you can redistribute it and/or modify it under the
terms of the Apache License, Version 2.0. A copy of the License can be
found in the LICENSE file at the root of this distribution.

Compile and test the lexical and grammar specification in `doc/spec/grammar`.
Assumes `bison` and `flex` are installed.
- Requires at least Flex 2.5.37; you can get a version for windows from: 
  <https://sourceforge.net/projects/winflexbison>
- Requires at least Bison 3+; you can get a version for windows from: 
  <https://sourceforge.net/projects/winflexbison> (use the "latest" zip package)
-----------------------------------------------------------------------------*/
import std/os/path
import std/os/file
import std/os/dir
import std/os/env
import std/os/process

fun main()
  build()


fun build()
  // Install paths
  val outdir = ".koka/grammar".path
  val gdir   = "doc/spec/grammar".path

  println("copying...")
  copy-directory(gdir,outdir)
  println("compile grammar...")
  //run-echo("cd " ++ outdir.string ++ " && bison -vd -W -Wno-empty-rule -Wcounterexamples --color=always parser.y")
  run-echo("cd " ++ outdir.string ++ " && bison -vd -W -Wno-empty-rule parser.y")
  println("compile lexer...")
  run-echo("cd " ++ outdir.string ++ " && flex -8 lexer.l")
  println("compile generated code...")
  // run-echo("cd " ++ outdir.string ++ " && stack exec ghc --system-ghc --skip-ghc-check -- -no-hs-main -o koka-parser lex.yy.c parser.tab.c")
  val cc = get-env("cc").default("gcc")
  run-echo("cd " ++ outdir.string ++ " && " ++ cc ++ " -o koka-parser lex.yy.c parser.tab.c")
  val parser = (outdir / "koka-parser").string
  println("created: " ++ parser)
  println("run as : " ++ parser ++ " lib/std/core.kk" )


fun run-echo(cmd : string) : io ()
  // println("> " ++ cmd)
  val exit-code = run-system(cmd)
  println( if exit-code==0 then "ok" else "error: " ++ exit-code.show )

