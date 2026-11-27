//---------------------------------------------------------------------------
// Copyright 2026, Microsoft Research, Daan Leijen. Tim Whiting.
//
// This is free software; you can redistribute it and/or modify it under the
// terms of the Apache License, Version 2.0. A copy of the License can be
// found in the LICENSE file at the root of this distribution.
//---------------------------------------------------------------------------
// Port of the "Detect C compiler" section of src/Compile/Options.hs
// (~Options.hs:1063-1349): the `CC` record (a description of a C-toolchain's
// command-line shape) and the pure builder functions for the toolchains
// upstream recognizes (`ccGcc`/`ccMsvc`) plus small OS/target-name helpers
// they depend on. Kept in its own module (rather than compiler/compile/
// options.kk) because these are pure, flags-independent "how does THIS
// toolchain spell -I/-o/-c" descriptions -- `cc-from-path` (upstream
// `ccFromPath`, which additionally reacts to `Flags` like `--fasan`/
// `--target`) lives in options.kk instead, alongside the rest of the
// flags-aware environment probe.
module compiler/compile/cc
import std/os/env
import compiler/common/syntax

// upstream `CC` (Options.hs:1069), including the function-valued fields
// (`ccFlagStack`, `ccAddDef`, ...) that assemble a compile/link command line
// per toolchain.
pub struct cc
  ccName: string = ""
  ccPath: string = ""
  ccFlags: list<string> = []
  ccFlagsBuild: list<(build-type,list<string>)> = []
  ccFlagsWarn: list<string> = []
  ccFlagsCompile: list<string> = []
  ccFlagsLink: list<string> = []
  ccFlagStack: (stksize: int) -> ndet list<string> = fn(_) []
  ccFlagHeap: (heapsize: int) -> ndet list<string> = fn(_) []
  ccAddLibraryDir: (dir: string) -> list<string> = fn(dir) ["-L", dir]
  ccIncludeDir: (dir: string) -> list<string> = fn(dir) ["-I", dir]
  ccTargetObj: (fname: string) -> ndet list<string> = fn(fname) ["-o", fname]
  ccTargetExe: (out: string) -> ndet list<string> = fn(out) ["-o", out]
  ccAddSysLib: (syslib: string) -> ndet list<string> = fn(syslib) ["-l" ++ syslib]
  ccAddLib: (lib: string) -> list<string> = fn(lib) [lib]
  ccAddDef: (def: (string,string)) -> list<string> = fn(d) { val (n, v) = d; ["-D" ++ n ++ (if v == "" then "" else "=" ++ v)] }
  ccLibFile: (lib: string) -> ndet string = fn(lib) "lib" ++ lib ++ ".a"
  ccObjFile: (obj: string) -> ndet string = fn(obj) obj ++ ".o"
  ccFlagsOpt: (opt: int, cpuArch: string) -> list<string> = fn(_, _) []

// Pick the right `(BuildType,Args)` entry
// (falling back to no extra flags) and append the arch-optimization flags.
pub fun cc/flags-build(c: cc, bt: build-type, optimize: int, cpuArch: string): list<string>
  val base = match c.ccFlagsBuild.find(fn(be) { val (b, _) = be; b == bt })
    Just((_, args)) -> args
    Nothing -> []
  base ++ (c.ccFlagsOpt)(optimize, cpuArch)

// ----------------------------------------------------------------------
// small OS/host helpers (upstream Options.hs uses `System.Info`/Platform.Config)
// ----------------------------------------------------------------------

pub fun on-macos(): ndet bool
  get-os-name() == "macos"

pub fun on-windows(): ndet bool
  get-os-name() == "windows"

pub fun exe-extension(): ndet string
  if on-windows() then ".exe" else ""

pub fun obj-extension(): ndet string
  if on-windows() then ".obj" else ".o"

pub fun lib-extension(): ndet string
  if on-windows() then ".lib" else ".a"

pub fun lib-prefix(): ndet string
  if on-windows() then "" else "lib"

// upstream `targetExeExtension`/`targetObjExtension`/`targetLibFile`
pub fun target-exe-extension(t: target): ndet string
  match t
    C(Wasm) -> ".wasm"
    C(WasmJs) -> ".js"
    C(WasmWeb) -> ".html"
    C -> exe-extension()
    JS(JsWeb) -> ".html"
    JS -> ".mjs"
    _ -> exe-extension()

pub fun target-obj-extension(t: target): ndet string
  match t
    C(Wasm) -> ".o"
    C(WasmJs) -> ".o"
    C(WasmWeb) -> ".o"
    C -> obj-extension()
    JS -> ".mjs"
    _ -> obj-extension()

pub fun target-lib-file(t: target, fname: string): ndet string
  match t
    C(Wasm) -> "lib" ++ fname ++ ".a"
    C(WasmJs) -> "lib" ++ fname ++ ".a"
    C(WasmWeb) -> "lib" ++ fname ++ ".a"
    C -> lib-prefix() ++ fname ++ lib-extension()
    JS -> fname ++ ".mjs"
    _ -> lib-prefix() ++ fname ++ lib-extension()

pub val gnu-warn: list<string> =
  ["-Wall","-Wextra","-Wpointer-arith","-Wshadow","-Wstrict-aliasing",
   "-Wno-unknown-pragmas","-Wno-missing-field-initializers",
   "-Wno-unused-parameter","-Wno-unused-variable","-Wno-unused-value",
   "-Wno-unused-but-set-variable"]

// Remove the file extension, as upstream `notext`: a `.` in a directory name
// is not an extension. A string helper, since these builders work on plain
// command-line strings.
pub fun strip-ext(s: string): string
  match s.find-last(".")
    Just(sl) | !sl.after.string.list.any(fn(c) c == '/' || c == '\\') -> sl.before.string
    _ -> s

// Also used (with small overrides) as the
// base for `mingw`/`emcc`/`clang`/`generic` in `cc-from-path`.
pub fun cc-gcc(name: string, path: string, hasOptG: bool): ndet cc
  fun opt-arch(opt: int, cpuArch: string): list<string>
    // unfortunately not as widely supported as one may hope, so only at -O2+
    if opt < 2 then []
    elif cpuArch == "x64" then ["-march=haswell","-mtune=native"]        // Haswell (2013) = x86-64-v3
    elif cpuArch == "arm64" then ["-march=armv8.3-a+crypto+aes","-mtune=native"]  // popcnt, simd, lse, pmull (+aes), ldapr
    else []
  Cc(
    ccName = name, ccPath = path,
    ccFlags = [],
    ccFlagsBuild = [(DebugFull, ["-g","-O0","-fno-omit-frame-pointer"]),
                    (Debug, ["-g", if hasOptG then "-Og" else "-O1"]),
                    (RelWithDebInfo, ["-O2","-g","-DNDEBUG"]),
                    (Release, ["-O2","-DNDEBUG"])],
    ccFlagsWarn = gnu-warn,
    ccFlagsCompile = ["-c"],
    ccFlagsLink = [],
    ccFlagStack = fn(stksize) if on-macos() && stksize > 0 then ["-Wl,-stack_size,0x" ++ stksize.show-hex(pre="")] else [],
    ccFlagHeap = fn(_) [],
    ccAddLibraryDir = fn(libdir) ["-L", libdir],
    ccIncludeDir = fn(idir) ["-I", idir],
    ccTargetObj = fn(fname) ["-o", fname.strip-ext ++ obj-extension()],
    ccTargetExe = fn(out) ["-o", out],
    ccAddSysLib = fn(syslib) ["-l" ++ syslib],
    ccAddLib = fn(lib) [lib],
    ccAddDef = fn(d) { val (n, v) = d; ["-D" ++ n ++ (if v == "" then "" else "=" ++ v)] },
    ccLibFile = fn(lib) lib-prefix() ++ lib ++ lib-extension(),
    ccObjFile = fn(obj) obj ++ obj-extension(),
    ccFlagsOpt = opt-arch)

pub fun cc-msvc(name: string, path: string): ndet cc
  fun opt-arch(opt: int, cpuArch: string): list<string>
    if opt < 2 then []
    elif cpuArch == "x64" then ["-arch:AVX2"]
    elif cpuArch == "arm64" then ["-arch:armv8.1"]
    else []
  Cc(
    ccName = name, ccPath = path,
    ccFlags = ["-DWIN32","-nologo"],
    ccFlagsBuild = [(DebugFull, ["-MDd","-Zi","-FS","-Od","-RTC1"]),
                    (Debug, ["-MDd","-Zi","-FS","-O1"]),
                    (Release, ["-MD","-O2","-Ob2","-DNDEBUG"]),
                    (RelWithDebInfo, ["-MD","-Zi","-FS","-O2","-Ob2","-DNDEBUG"])],
    ccFlagsWarn = ["-W3"],
    ccFlagsCompile = ["-EHs","-TP","-c"],      // always compile as C++ on msvc (atomics etc.)
    ccFlagsLink = ["-link"],
    ccFlagStack = fn(stksize) if stksize > 0 then ["/STACK:" ++ stksize.show] else [],
    ccFlagHeap = fn(_) [],
    ccAddLibraryDir = fn(libdir) ["/LIBPATH:" ++ libdir],
    ccIncludeDir = fn(idir) ["-I", idir],
    ccTargetObj = fn(fname) ["-Fo" ++ fname.strip-ext ++ obj-extension()],
    ccTargetExe = fn(out) ["-Fe" ++ out ++ ".exe"],
    ccAddSysLib = fn(syslib) [syslib ++ lib-extension()],
    ccAddLib = fn(lib) [lib],
    ccAddDef = fn(d) { val (n, v) = d; ["-D" ++ n ++ (if v == "" then "" else "=" ++ v)] },
    ccLibFile = fn(lib) lib-prefix() ++ lib ++ lib-extension(),
    ccObjFile = fn(obj) obj ++ obj-extension(),
    ccFlagsOpt = opt-arch)
