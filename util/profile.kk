/*---------------------------------------------------------------------------
  Copyright 2026, Microsoft Research, Angelica Moreira.

  Utility to profile Koka programs using the --fprofile flag.
  Compiles with profiling instrumentation (-pg), runs the binary to
  generate gmon.out, then invokes gprof for a flat+call-graph report.

  Usage:
    koka util/profile.kk -- <source.kk> [--runs=N] [--cc=<compiler>] [--opt=<level>]
---------------------------------------------------------------------------*/
module profile

import std/os/path
import std/os/dir
import std/os/process
import std/os/env
import std/os/flags

// Configuration
struct pflags
  source   : string = ""
  runs     : int = 1
  cc       : string = "gcc"
  opt      : string = "-O2"
  args     : string = ""
  verbose  : bool = False
  showhelp : bool = False

val flag-descs : list<flag<pflags>> =
  fun set-source( f : pflags, s : string ) : pflags  { f(source = s) }
  fun set-runs( f : pflags, s : string ) : pflags    { f(runs = s.parse-int.default(1)) }
  fun set-cc( f : pflags, s : string ) : pflags      { f(cc = s) }
  fun set-opt( f : pflags, s : string ) : pflags     { f(opt = s) }
  fun set-args( f : pflags, s : string ) : pflags    { f(args = s) }
  fun set-verbose( f : pflags, b : bool ) : pflags   { f(verbose = b) }
  fun set-help( f : pflags, b : bool ) : pflags      { f(showhelp = b) }
  [ Flag("s", ["source"],  Req(set-source,"FILE"),   "Koka source file to profile"),
    Flag("r", ["runs"],    Req(set-runs,"N"),        "Number of profiling runs (default: 1)"),
    Flag("c", ["cc"],      Req(set-cc,"CC"),         "C compiler to use (default: gcc)"),
    Flag("o", ["opt"],     Req(set-opt,"LEVEL"),     "Optimization level (default: -O2)"),
    Flag("a", ["args"],    Req(set-args,"ARGS"),     "Extra arguments for the compiled program"),
    Flag("v", ["verbose"], Bool(set-verbose),        "Show verbose output"),
    Flag("h", ["help"],    Bool(set-help),           "Show this help")
  ]

fun process-flags() : <console,ndet> maybe<pflags>
  val header = "Usage: koka util/profile.kk -- [options] <source.kk>\n\nOptions:"
  val (pflags, rest, errs) = parse(Pflags(), flag-descs, get-args())
  if errs.is-nil then
    val pf = match rest
      Cons(src, _) | pflags.source.is-empty -> pflags(source = src)
      _ -> pflags
    if pf.showhelp || pf.source.is-empty then
      flag-descs.usage(header).println
      Nothing
    else Just(pf)
  else
    errs.map(println)
    flag-descs.usage(header).println
    Nothing

pub fun main()
  match process-flags()
    Nothing -> ()
    Just(pf) ->
      profile(pf)

fun profile( pf : pflags ) : io ()
  val src = pf.source
  println("=== Koka Profiler ===")
  println("source : " ++ src)
  println("cc     : " ++ pf.cc)
  println("opt    : " ++ pf.opt)
  println("runs   : " ++ pf.runs.show)
  println("")

  // Step 1: Compile with profiling
  println("[1/3] Compiling with --fprofile ...")
  val compile-cmd = "koka --fprofile --cc=" ++ pf.cc ++ " " ++ pf.opt ++ " -e " ++ src
  if pf.verbose then println("  > " ++ compile-cmd)
  // Use -e to compile and find the binary, but first just compile
  val compile-only = "koka --fprofile --cc=" ++ pf.cc ++ " " ++ pf.opt ++ " -c " ++ src
  val cret = run-system(compile-only)
  if cret != 0 then
    println("ERROR: compilation failed (exit code " ++ cret.show ++ ")")
    return ()

  println("  compilation succeeded")
  println("")

  // Step 2: Run with profiling (generates gmon.out)
  println("[2/3] Running " ++ pf.runs.show ++ " time(s) to collect profile data ...")
  val run-cmd = "koka --fprofile --cc=" ++ pf.cc ++ " " ++ pf.opt ++ " -e " ++ src ++ " -- " ++ pf.args
  for( pf.runs ) fn(i)
    if pf.verbose then println("  run " ++ i.show ++ ": " ++ run-cmd)
    val rret = run-system(run-cmd)
    if rret != 0 then
      println("  WARNING: run " ++ i.show ++ " exited with code " ++ rret.show)
  println("  profiling runs complete")
  println("")

  // Step 3: Generate gprof report
  println("[3/3] Generating gprof report ...")
  val gprof-cmd = "gprof $(ls -t .koka/v*/gcc-profile-*/*__main 2>/dev/null | head -1) gmon.out"
  if pf.verbose then println("  > " ++ gprof-cmd)
  val gret = run-system(gprof-cmd)
  if gret != 0 then
    println("NOTE: gprof failed (exit " ++ gret.show ++ "). You may need to install gprof or check gmon.out exists.")
    println("  Try manually: gprof <binary> gmon.out")
