import std/os/env

pub effect val compFlags: compilerFlags;

struct compilerFlags
  programName: string
  version: string
  compiler: string
  buildVariant: string;

struct fileConfig
  exeExt: string
  dllExt: string
  objExt: string
  libExt: string
  libPrefix: string
  pathSep: string
  pathDelim: string;

fun os-config(): <exn,ndet> fileConfig
  match get-os-name()
    "linux" -> FileConfig(".so", ".o", ".a", "lib", "", "/", ":")
    "windows" -> FileConfig(".exe", ".dll", ".obj", ".lib", "", "\\", ";")
    "macos" -> FileConfig(".dylib", ".o", ".a", "lib", "", "/", ":")

val sourceExt = ".kk"
