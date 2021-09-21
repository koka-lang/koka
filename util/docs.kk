/*-----------------------------------------------------------------------------
Copyright 2020-2021, Microsoft Research, Daan Leijen.
This is free software; you can redistribute it and/or modify it under the
terms of the Apache License, Version 2.0. A copy of the License can be
found in the LICENSE file at the root of this distribution.

Run this to generate documentation from the `doc/spec` directory
using the Madoko program (install: `> npm install -g madoko`)
-----------------------------------------------------------------------------*/
import std/os/path
import std/os/flags
import std/os/env
import std/os/file
import std/os/dir
import std/os/process

val header = "usage:\n stack exec koka -- util/docs [-- [options]]\n\noptions:"

struct iflags 
  publish  : bool  = False
  version  : string = ""
  speconly : bool  = False


val flag-descs : list<flag<iflags>> = 
  fun set-publish( f : iflags, b : bool ) : iflags  
    f(publish = b) 
  fun set-speconly( f : iflags, b : bool ) : iflags  
    f(speconly = b) 
  fun set-version( f : iflags, s : string ) : iflags  
    f(version = s) 

  [ Flag( "p", ["publish"], Bool(set-publish), "publish to ../koka-pages directory" ),
    Flag( "v", ["version"], Req(set-version,"<version>"), "latest koka version" ),
    Flag( "s", ["speconly"], Bool(set-speconly), "specification only")
  ]


public fun process-flags() : <ndet,console> maybe<iflags> 
  val (flags,args,errs) = parse( Iflags(), flag-descs, get-args() )
  if errs.is-nil && args.is-nil then
    Just(flags)
  else 
    println( errs.join("\n") ++ "\n" ++ flag-descs.usage(header) )
    Nothing

fun main()
  match process-flags()
    Nothing -> ()
    Just(flags) -> build(flags.publish, flags.version || get-compiler-version(), flags.speconly)

fun build( publish : bool, version : string, speconly : bool ) : io () 
  val docsite  = "https://koka-lang.github.io/koka/doc/"
  val doclocal = "../koka-pages/doc".path

  val outspec  = "out/spec".path;
  //val outspec   = outspecx ++ "js-debug"
  val outstyles = outspec / "styles"
  val outscripts = outspec / "scripts"
  val outimages = outspec / "images"
  val specdir   = "doc/spec".path
  val docflags  = "--htmlcss=styles/madoko.css --htmlcss=styles/koka.css " ++ (if (publish) then "--htmlbases=" ++ docsite ++ " " else "")
  val cmd = "koka -c -l --builddir=" ++ outspec.string ++ " -i" ++ specdir.string ++ " --html " ++ docflags
  if !speconly then
    println("compile standard libraries...")
    run-echo(cmd ++ " toc.kk")
  
  println("compile documentation inlined code...")
  run-echo(cmd ++ " book.kk.md getstarted.kk.md why.kk.md tour.kk.md spec.kk.md index.kk.md")
  println("patch book.md")
  file-replace-all(outspec / "book.md", ".kk.md", ".md");

  println("copy web files...")
  ensure-dir(outstyles)
  ensure-dir(outimages)
  ensure-dir(outscripts)
  copy-directory(specdir / "images", outimages)
  copy-directory(specdir / "styles", outstyles)
  copy-directory(specdir / "scripts", outscripts)
  copy-file-to-dir("doc/koka.css".path, outstyles)
  copy-file-to-dir("doc/logo/koka-logo.png".path, outimages)
  copy-file-to-dir(specdir / "koka.bib", outspec)
  copy-file-to-dir("lib/std/time/timescales.png".path, outspec)
  copy-file-to-dir("doc/spec/book-style.md".path, outspec)
  copy-file-to-dir("doc/spec/news.mdk".path, outspec)
  
  val xmps = list-directory(outspec).
             filter(fn(p){ p.string.ends-with(".xmp.html").bool }).
             map(fn(p){ p.nodir.string })
  println("run madoko...")
  run-echo( "cd " ++ outspec.string ++ " && madoko --odir=. -v -mline-no:false -mlogo:false -mkokaversion:" ++ version
             ++ " " ++ xmps.join(" ") ++ " book.md index.md" )
             
  copy-file-to-dir(outspec / "madoko.css", outstyles)
  if publish then
    println("copy files to output folder: " ++ doclocal.string)
    ensure-dir(doclocal)
    val htmls = list-directory(outspec).
                filter(fn(p){ p.extname == "html" && !p.string.ends-with(".xmp.html").bool })
    htmls.map fn(p){ copy-file-to-dir(p,doclocal)
    copy-directory(outstyles,doclocal / "styles")
    copy-directory(outimages,doclocal / "images")
  
  println("done.")


fun file-replace-all( fname : path, pat : string, rep : string ) : io () 
  val content = read-text-file(fname)
  write-text-file( fname, content.replace-all(pat, rep) )


fun run-echo(cmd : string) : io () 
  // println("> " ++ cmd)
  val exit-code = run-system(cmd)
  if exit-code==0 then
    println("ok")
  else 
    println("> " ++ cmd)
    throw("error: " ++ exit-code.show )