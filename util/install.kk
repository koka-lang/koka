import std/os/path
import std/os/file
import std/os/env
import std/os/flags

val header = "usage:\n stack exec koka -- util/install [-- [options]]\n\noptions:"

struct iflags {
  prefixdir : string = ""
}

val flag-descs : list<flag<iflags>> = {
  fun set-prefix( f : iflags, s : string ) : iflags {
    f(prefixdir = s)
  }
  [ Flag( "", ["prefix"], Req(set-prefix,"DIR"), "set prefix directory ('/usr/local')" ) ]
}

public fun process-flags() : <ndet,console> maybe<iflags> {
  val (flags,args,errs) = parse( Iflags(), flag-descs, get-args() )
  if (errs.is-nil && args.is-nil) {
    Just(flags)
  }
  else {
    println( errs.join("\n") + "\n" + flag-descs.usage(header) )
    Nothing
  }
}

fun main() {
  if (!is-file("koka.cabal".path)) {
    println("you can only run 'util/install' from the koka root directory")
    println(flag-descs.usage(header))
    return ()
  }
  match (process-flags()) {
    Nothing -> ()
    Just(flags) -> {
      val predir = if (flags.prefixdir.is-empty)
                    then get-default-prefix()
                    else flags.prefixdir.path
      // get version and compiler from the install script itself
      match(app-path().dirparts.reverse) {
        (Cons(_,Cons(variant,Cons(version)))) {
           val cc = variant.trim-right("-debug")
           install( predir, cc, version)
        }
      }
    }
  }
}

fun install( prefixdir : path, cc : string, version : string ) {
  // Install paths
  val koka    = koka-exe-path()
  val bindir  = prefixdir + "bin".path
  val libdir  = prefixdir + "lib/koka".path + version.path
  val sharedir= prefixdir + "share/koka".path + version.path

  println("installing koka:")
  println("  bin    : " + bindir.string)
  println("  lib    : " + libdir.string)
  println("  share  : " + sharedir.string)
  println("  cc     : " + cc)
  println("  version: " + version)
  ensure-dir(bindir)
  ensure-dir(libdir)
  ensure-dir(sharedir)

  // Build kklib versions
  [Debug,DRelease,Release].foreach fn(v){ build-kklib(cc,v) }

  // Copy Koka
  println("copy koka executable...")
  copy-file-to-dir(koka, bindir)

  // Copy std libs
  val stdlibdir = sharedir + "lib"
  println("copy standard libraries...")
  copy-directory("lib/std".path, stdlibdir + "std")
  copy-file-to-dir("lib/toc.kk".path, stdlibdir)
  copy-file-to-dir("lib/package.std.json".path, stdlibdir)

  // Copy samples
  println("copy samples...")
  copy-directory("samples".path, stdlibdir + "samples")

  // Copy kklib
  val kklibdir = sharedir + "kklib"
  println("copy kklib runtime...")
  copy-directory("kklib/src".path, kklibdir + "src")
  copy-directory("kklib/include".path, kklibdir + "include")
  copy-directory("kklib/mimalloc".path, kklibdir + "mimalloc")
  copy-file-to-dir("kklib/CMakeLists.txt".path, kklibdir)
  copy-file-to-dir("kklib/kkmain.cmake".path, kklibdir)

  // Copy contrib
  val contribdir = sharedir + "contrib"
  println("copy contrib...")
  ensure-dir(contribdir)
  copy-file-to-dir("contrib/biginteger.js".path, contribdir)

  // Copy kklib binaries
  [Debug,DRelease,Release].foreach fn(variant){
    println("copying standard library: " + variant.show)
    val vardir = cc + "-" + variant.show
    val objdir = libdir + vardir
    val outdir = "out".path + version + vardir
    val kklib  = if (partsep()=="\\" && cc!="mingw") then "kklib.lib" else "libkklib.a"  // todo: detect OS
    ensure-dir(objdir)
    copy-file-to-dir( outdir + kklib, objdir )
    val all = list-directory(outdir)
    val files = all.filter(fn(f){ !is-directory(f) })
    val objs = files.filter(fn(f){ f.basename.starts-with("std_").bool && f.extname != "core" })
    objs.foreach fn(obj){
      // println("copy " + obj.string)
      copy-file-to-dir( obj, objdir )
    }
  }

  println("done installing.")
  println("  bin    : " + bindir.string)
  println("  lib    : " + libdir.string)
  println("  share  : " + sharedir.string)
  println("  cc     : " + cc)
  println("  version: " + version)
}

fun koka-exe-path() : <file,exn> path {
  val out = run-command("stack path --local-install-root").exn
  val exe = out.trim.path + "bin/koka".path
  if (exe.is-file) return exe
  val win = exe.change-ext(".exe")
  if (win.is-file) return win
  throw("unable to find koka executable: use '> stack build' first.\n  (" + exe.string + ")")
}

/*
fun koka-version( koka : path ) : <file,exn> string {
  val out = run-command(koka.string + " --version").exn
  out.list.drop-while(fn(c){ !is-white(c) }).take-while(fn(c){ c != ',' && c != '\n' }).string.trim
}
*/

fun get-default-prefix() : <file,exn> path {
  val out = run-command("stack path --local-bin").exn
  out.trim.path.nobase
}

type variant {
  Debug
  DRelease
  Release
}

fun show( v : variant ) : string {
  match(v) {
    Debug    -> "debug"
    DRelease -> "drelease"
    Release  -> "release"
  }
}

fun build-kklib(cc : string, v : variant) : io () {
  println("build kklib: " + v.show + "...")
  val extra = match(v) {
    Debug    -> ""
    DRelease -> "-O2"
    Release  -> "-O2 --no-debug"
  }
  val code = run-system("stack exec koka -- --cc=" + cc + " " + extra + " util/link-std")
  if (code != 0) throw("error: " + code.show)
}
