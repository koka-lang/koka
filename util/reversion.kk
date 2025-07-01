/*-----------------------------------------------------------------------------
Copyright 2020-2025, Microsoft Research, Daan Leijen.
This is free software; you can redistribute it and/or modify it under the
terms of the Apache License, Version 2.0. A copy of the License can be
found in the LICENSE file at the root of this distribution.

Run this to update the version number of the compiler.
-----------------------------------------------------------------------------*/
import std/os/path
import std/os/flags
import std/os/file
import std/os/env
import std/text/regex
import std/time

val header = "usage:\n stack exec koka -- util/reversion -v <version>\n\noptions:"

struct iflags
  version  : string = ""


val flag-descs : list<flag<iflags>> =
  fun set-version( f : iflags, s : string ) : iflags
    f(version = s)

  [ Flag( "v", ["version"], Req(set-version,"<version>"), "the new koka version (without preceding 'v')" ),
  ]


fun process-flags() : <ndet,console> maybe<iflags>
  val (flags,args,errs) = parse( Iflags(), flag-descs, get-args() )
  if errs.is-nil && args.is-nil then
    Just(flags)
  else
    println( errs.join("\n") ++ "\n" ++ flag-descs.usage(header) )
    Nothing

pub fun main()
  match process-flags()
    Nothing -> ()
    Just(flags) -> re-version(flags.version || get-compiler-version())

fun re-version( version : string ) : _ ()
  val rversion = r#"(\d+\.\d+(?:\.\d+(?:\-[\w\-]+)?)?)"#
  val files = [ ("package.yaml",     ["^version:\\s*","-DKOKA_VERSION\\s*=\\s*\""])
              , ("util/install.bat", ["^set KOKA_VERSION=v"] )
              , ("util/install.sh",  ["^VERSION=\"v"] )
              , ("util/minbuild.sh", ["^KOKA_VERSION="] )
              , ("support/vscode/koka.language-koka/package.json", ["\"compilerVersion\"\\s*:\\s*\""] )
              , ("util/release.md",  ["/download/v"])
              ]

  val date = ts-ti.local-time-now().date.show
  val rversionDate = rversion ++ "\\s*,\\s*[\\w\\d\\-]+\\s*$"
  val filesDate    = [ ("whatsnew.md", ["^##+\\s*v"]), // first match only
                       ("readme.md", ["^_Latest release_: v"] )]

  println("updating with new version: " ++ version ++ ", " ++ date )
  files.replace fn(txt,pre)
    val rx = regex("(" ++ pre ++ ")" ++ rversion, multiline=True)
    match txt.find(rx)
      Nothing ->
        println("  WARNING: could not find regex: " ++ rx.source)
        txt
      Just(m) ->
        println("  update: " ++ m)
        txt.replace-all(rx, "$1" ++ version)

  filesDate.replace fn(txt,pre)
    val rx = regex("(" ++ pre ++ ")" ++ rversionDate, multiline=True)
    match txt.find(rx)
      Nothing ->
        println("  WARNING: could not find regex: " ++ rx.source)
        txt
      Just(m) ->
        print("  update: " ++ m)
        txt.replace(rx, "$1" ++ version ++ ", " ++ date ++ "\n")

  println("done.")

fun replace( fnames : list<(string,list<string>)>, repl : (string,string) -> io string ) : io ()
  fnames.foreach fn((fname,regexs))
    println("reading: " ++ fname)
    val content   = read-text-file(fname.path)
    val replaced  = regexs.foldl(content,repl)
    write-text-file(fname.path,replaced,False)
    println("updated.")
