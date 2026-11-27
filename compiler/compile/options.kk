// Port of src/Compile/Options.hs: the `Flags` record, its defaults
// full command-line parsing (upstream
// `options`/`parseOptions`), and the environment probe that turns parsed
// flags into a fully-resolved configuration (upstream `processInitialOptions`
// -- CC-toolchain detection, editor/package discovery, env-var defaults --
// and `processDerivedOptions` -- optimize-level-driven knobs, the vcpkg
// triplet, and the C preprocessor defines).
// Built on `std/os/flags` (a close analog of upstream's `Platform.GetOptions`,
// itself a re-export of Haskell's `System.Console.GetOpt` -- see the doc
// comment on `flag`/`parse` in std/os/flags.kk for the correspondence). The
// CC record and its pure per-toolchain builders (`ccGcc`/`ccMsvc`) live in
// the separate compiler/compile/cc.kk module (see its doc comment for why).
//
// Divergences from upstream (documented, not silent):
//  - Help/Version/Interactive/LanguageServer are modeled as plain boolean
//    fields on `flags` (set by ordinary `Bool` flags) rather than upstream's
//    separate `Option` sum type (Interactive|LanguageServer|Version|Help|
//    Flag|Error) -- `std/os/flags`'s `parse` always folds into a single
//    accumulator type, so `mode` is derived from those fields after parsing
//    instead of scanned out of a heterogeneous option list.
//  - Hidden ("internal"/"deprecated") flags parse identically to upstream but
//    are not omitted from `usage`'s help text (upstream's `hide` drops them
//    from `--help` entirely); this port instead lists them, grouped and
//    documented LAST, under their own "internal options" header -- see
//    `koka-flags-internal`/`koka-flags/usage`.
//  - `discover-packages` (upstream `discoverPackages`, Compile/Package.hs) is
//    stubbed to `Packages()`: real workspace-package discovery is a separate
//    subsystem from flag/environment resolution and isn't ported.
//  - `get-koka-dirs` sniffs the same two layouts
//    upstream does -- this repository (`<root>/kklib` exists) or an install
//    prefix (`<prefix>/share/koka/v<version>`, what `util/bundle.kk`
//    installs) -- but derives the root from this port's OWN build layout
//    (`.koka/v<ver>/<target>-<type>-<hash>/`) plus the `bin` convention,
//    rather than upstream's cabal/stack dist-directory patterns.
//    `--libdir`/`--sharedir` override it.
//  - `get-env-options`'s C# `csc`-toolchain auto-detection (upstream
//    `getEnvCsc`, Windows `.NET Framework` registry-style directory probing)
//    is not ported: the C# backend itself isn't ported.
module compiler/compile/options
import std/os/flags
import std/os/env
import std/os/path
import std/os/dir
import std/core/undiv
import std/core/exn
import std/core/unsafe
import compiler/common/syntax
import compiler/common/color-scheme
import compiler/platform/config
import compiler/compile/cc

// upstream `Package`/`Packages` (Compile/Package.hs) -- population
// (`discoverPackages`) is not ported; only the record shapes are.
pub struct package
  pkgDir: string
  pkgQualName: string
  pkgLocal: string
  pkgSub: list<package> = []

pub struct packages
  packages: list<package> = []
  roots: list<string> = []

pub type mode
  ModeHelp
  ModeVersion
  ModeCompiler(files: list<string>)
  ModeInteractive(files: list<string>)
  ModeLanguageServer(files: list<string>)

// upstream `Flags` (Options.hs:143), defaults matching `flagsNull` (Options.hs:294)
// not a `value struct`: `baseFlags: maybe<flags>` makes it self-referential
pub struct flags
  // warnings / show flags
  warnShadow: bool = True
  showKinds: bool = False
  showKindSigs: bool = False
  showSynonyms: bool = False
  showCore: bool = False
  showInitialCore: bool = False
  showFinalCore: bool = False
  showCoreTypes: bool = False
  // Write the specializer's provenance comment into interfaces. OFF by default:
  // it is documentation that nothing reads back -- the lexer scans every byte of
  // it and discards it -- and it was ~30% of interface bytes. Interface text is
  // what an LSP pays to start up, so the default matters there most. Turn it on
  // when you need to see WHY something did or did not specialize.
  coreProvenance: bool = False
  showTypeIds: bool = False
  showAsmCS: bool = False
  showAsmJS: bool = False
  showAsmC: bool = False
  showTypeSigsField: bool = False    // upstream `_showTypeSigs` (private field; see `flags/show-type-sigs`)
  showHiddenTypeSigs: bool = False
  showElapsed: bool = False

  // run mode
  evaluate: bool = False
  execOpts: list<string> = []
  library: bool = False

  // target platform (upstream bundles these into one `TargetPlatform`
  // record; the port keeps target/platform as separately-typed fields that
  // predate this pass and adds the remaining TargetPlatform fields alongside
  // -- see `flags/tpl`, which reassembles the full `target-platform`)
  // upstream `flagsNull`'s `targetPlatformC64 = TargetPlatform (C LibC) "" ""
  // platform64 []`: the default backend is C with the libc host. `C(LibC)` and
  // not `C(CDefault)` -- `match-target` only treats CDefault as a wildcard on
  // its LEFT side, so a `[host=libc]` conditional import (which matches
  // `C(LibC)` against this field) can never match a `C(CDefault)` target.
  target: target = C(LibC)
  targetPlatform: platform = platform64
  targetArch: string = ""
  targetOS: string = ""
  buildDefs: list<string> = []

  stackSize: int = 0
  heapSize: int = 0
  simplify: int = 5
  simplifyMaxDup: int = 10           // >= 10 to inline partial applications across binds
  colorScheme: color-scheme = ColorScheme()

  buildDir: string = ""              // kkbuild
  buildTag: string = ""
  outBuildDir: string = ""
  outBaseName: string = ""
  outFinalPath: string = ""
  includePath: list<string> = []     // .kk/.kki search path

  csc: string = "csc"
  node: string = "node"
  wasmrun: string = "wasmtime"
  cmake: string = "cmake"
  cmakeArgs: string = ""

  ccompPath: string = ""
  ccompCompileArgs: list<string> = []
  ccompIncludeDirs: list<string> = []
  ccompDefs: list<(string,string)> = []
  ccompLinkArgs: list<string> = []
  ccompLinkSysLibs: list<string> = []
  ccompLinkLibs: list<string> = []
  ccomp: cc = Cc()                   // upstream default is `ccGcc "gcc" "gcc" True`, but that
                                      // probes the OS (`ndet`) so can't be a pure struct-field
                                      // default here; `process-initial-options` fills in the
                                      // real toolchain (see `cc-from-path`)
  ccompLibDirs: list<string> = []
  autoInstallLibs: bool = True
  vcpkgRoot: string = ""
  vcpkgTriplet: string = ""
  conan: string = "conan"

  editor: string = ""
  redirectOutput: string = ""
  outHtml: int = 0
  htmlBases: list<(string,string)> = []
  // upstream `flagsNull` (Options.hs:352) defaults this to
  // `"styles/" ++ programName ++ ".css"`, not empty -- without it the generated
  // documentation has no stylesheet link at all.
  htmlCss: string = "styles/koka.css"
  htmlJs: string = ""

  verbose: int = 1
  showSpan: bool = False
  console: string = "ansi"           // ansi | html | raw
  rebuild: bool = False
  genCore: bool = False
  coreCheck: bool = False
  enableMon: bool = True
  semiInsert: bool = True
  genRangeMap: bool = False
  languageServerPort: int = 6061
  languageServerStdio: bool = False

  localBinDir: string = ""
  localDir: string = ""
  localLibDir: string = ""
  localShareDir: string = ""
  packages: packages = Packages()
  forceModule: string = ""

  // Only reaches the C compiler at `optimize >= 1`, where it is what picks
  // RelWithDebInfo over Release (see `build-type`); the Debug and DebugFull
  // flag lists carry `-g` themselves. Default OFF: debug info costs 29% of a
  // cold self-build (228s -> 161s) and 5.5x the peak clang memory (9.9GB ->
  // 1.8GB on a 21MB translation unit) while changing no generated code, and
  // line-level profiling comes from `--profile` instead. `-g` opts back in.
  debug: bool = False                // emit debug info
  optimize: int = 0                  // upstream default 0 (debug); specialize needs >= 1
  optInlineMax: int = 12
  optctail: bool = True
  optctailCtxPath: bool = True
  optUnroll: int = -1
  optEagerPatBind: bool = False
  parcReuse: bool = True
  parcSpecialize: bool = True
  parcReuseSpecialize: bool = True   // upstream `parcReuseSpec`
  parcBorrowInference: bool = False
  asan: bool = False
  profile: bool = False
  useStdAlloc: bool = False
  optSpecialize: bool = True         // only effective when optimize >= 1 (upstream)
  mimallocStats: bool = False
  allowInfiniteChains: bool = False
  maxConcurrency: int = 16
  maxErrors: int = 25
  useBuildDirHash: bool = True
  outputEntryName: string = ""
  mainEntryName: string = ""
  baseFlags: maybe<flags> = Nothing

  // Help/Version/Interactive/LanguageServer detection (see module doc
  // comment: upstream's separate `Option` sum type, folded here into plain
  // fields so a single `flag<flags>` table can set everything)
  wantHelp: bool = False
  wantVersion: bool = False
  wantInteractive: bool = False
  wantLanguageServer: bool = False

  // port-specific bridge field: upstream finds kklib's headers under
  // `localShareDir`; here they are this repository's own `kklib/include`.
  kklibIncludeDir: string = ""

  // port-specific: the language server's analysis compile. Upstream's
  // `rebuildFile` only type checks the focus module (`buildcTypeCheck`) and runs
  // the full pipeline just to execute; this port has no per-module build phases,
  // so the flag stops the ENTRY after type checking (see `compile-module`) and
  // skips compiling C objects. Dependencies still compile in full: imports are
  // loaded from their interfaces.
  typeCheckOnly: bool = False

pub fun flags/tpl(fl: flags): target-platform
  target-platform-default(tpl-target = fl.target, tpl-platform = fl.targetPlatform,
    tpl-arch = fl.targetArch, tpl-os = fl.targetOS, tpl-build-defs = fl.buildDefs)

// upstream gates specialization on the optimization level
pub fun flags/use-specialize(fl: flags): bool
  fl.optSpecialize && fl.optimize >= 1

// Derived from optimize/debug, not stored
pub fun flags/build-type(fl: flags): build-type
  if fl.optimize < 0 then DebugFull
  elif fl.optimize == 0 then Debug
  elif fl.debug then RelWithDebInfo
  else Release

// Upstream `flagsHash` (Options.hs:289) over its `instance Hashable Flags`
// (Options.hs:245): the flags that change what is EMITTED, and therefore which
// build directory a module belongs in.
//
// What is ABSENT matters as much as what is present. Every `show*` output
// flag, `evaluate`, `library`, `verbose` and `console` are deliberately not
// hashed -- two builds differing only in those emit identical artifacts and
// MUST land in one directory, which is what lets a test suite reuse a single
// std build across tests that differ only in what they print. `optimize` IS
// hashed, so -O1 and -O2 never share an interface cache.
//
// Upstream also omits `stackSize`/`heapSize` (commented out in its list).
pub fun flags-hash-fields(fl: flags): list<string>
  [fl.targetPlatform.show,
   fl.simplify.show,
   fl.simplifyMaxDup.show,
   fl.includePath.join(""),
   fl.csc,
   fl.ccompPath,
   fl.ccompCompileArgs.join(""),
   fl.ccompIncludeDirs.join(""),
   fl.ccompDefs.map(fn((k, v)) k ++ "=" ++ v).join(""),
   fl.ccompLinkArgs.join(""),
   fl.ccompLinkSysLibs.join(""),
   fl.ccompLinkLibs.join(""),
   fl.ccomp.ccName,
   fl.ccompLibDirs.join(""),
   fl.localBinDir,
   fl.localLibDir,
   fl.localShareDir,
   fl.debug.show,
   fl.optimize.show,
   fl.optInlineMax.show,
   fl.optctail.show,
   fl.optctailCtxPath.show,
   fl.optUnroll.show,
   fl.optEagerPatBind.show,
   fl.parcReuse.show,
   fl.parcSpecialize.show,
   fl.parcReuseSpecialize.show,
   fl.parcBorrowInference.show,
   fl.asan.show,
   fl.profile.show,
   fl.useStdAlloc.show,
   fl.optSpecialize.show,
   fl.allowInfiniteChains.show]

// A polynomial hash kept inside 48 bits so the arbitrary-precision `int` never
// grows. Only stability and discrimination matter here -- the value does not
// need to agree with upstream's Haskell `hash`, since nothing reads the other
// compiler's directories.
fun hash-string(s: string): int
  s.list.foldl(2166136261) fn(h: int, c: char)
    ((h * 33) + c.int) % 281474976710656

pub fun flags-hash(fl: flags): string
  // "\x01" as the separator so two adjacent fields cannot be confused with one
  // longer field (a classic hash-collision-by-concatenation)
  val h = hash-string(fl.flags-hash-fields.join("\x01"))
  show-hex(h % 16777216, 6, False, "")

// The `<cc>-<build-type>[-<hash>]` directory name for the CURRENT compile
// Interface CONTENT depends on the optimization
// level, so an -O0 interface reused by an -O2 build is wrong code, silently --
// the build-type prefix alone does NOT separate those (with `debug` on, both
// -O1 and -O2 are RelWithDebInfo), so the flags hash is what actually keeps
// them apart.
pub fun build-variant(fl: flags): string
  fl.build-lib-variant ++ (if fl.useBuildDirHash then "-" ++ fl.flags-hash else "")

// upstream `buildLibVariant`: the variant without the flags hash, which is also
// the directory of a precompiled library under `--libdir` (e.g. `js-debug`)
pub fun build-lib-variant(fl: flags): string
  val bt = match fl.build-type
    DebugFull -> "debugfull"
    Debug -> "debug"
    RelWithDebInfo -> "drelease"
    Release -> "release"
  val pre = match fl.target
    C(ctarget) ->
      val arch = match ctarget
        Wasm -> "-wasm" ++ (8 * fl.targetPlatform.ptrSize).show
        WasmJs -> "-wasmjs"
        WasmWeb -> "-wasmweb"
        _ -> if fl.targetPlatform.has-compressed-fields then "-" ++ fl.targetArch ++ "c" else ""
      fl.ccomp.ccName ++ arch
    _ -> "js"
  pre ++ "-" ++ bt

pub fun flags/show-type-sigs(fl: flags): bool
  fl.showHiddenTypeSigs || fl.showTypeSigsField

// `<shareDir>/kklib/include`, resolved from the share dir `get-koka-dirs`
// sniffs -- so it follows the compiler whether it runs from this repository
// or from an install prefix.
fun kklib-include-of(share-dir: string): string
  share-dir ++ "/kklib/include"

// the starting point for `parse-options`: C/libc/64-bit. `--target=js` (etc.)
// switches from here.
// `kklibIncludeDir` is filled in by `process-initial-options` from the sniffed
// share dir; it has no static default.
pub val flags-default: flags = Flags()

// ----------------------------------------------------------------------
// small helpers (upstream Options.hs / Common.File)
// ----------------------------------------------------------------------

// Split a shell-argument-ish string on whitespace,
// honoring '...'/"..." quoting and backslash-escapes
pub fun unquote(s: string): list<string>
  // `inq`: `Nothing` while scanning unquoted text, `Just(q)` while inside a
  // `q`-quoted span (single state machine to avoid a mutually-recursive pair
  // of local functions)
  fun scan(acc: list<char>, inq: maybe<char>, cs: list<char>): div list<string>
    match cs
      Cons(c, rest) | inq.is-nothing && (c == '"' || c == '\'') -> scan([], Just(c), rest)
      Cons(c, rest) | inq == Just(c) -> Cons(acc.reverse.string, scan([], Nothing, rest))
      Cons('\\', Cons(c2, rest)) -> scan(Cons(c2, acc), inq, rest)
      Cons(c, rest) | inq.is-nothing && c.is-white -> Cons(acc.reverse.string, scan([], Nothing, rest.drop-while(fn(c2) c2.is-white).pretend-decreasing))
      Cons(c, rest) -> scan(Cons(c, acc), inq, rest)
      Nil -> [acc.reverse.string]
  pretend-no-div({scan([], Nothing, s.list)}).filter(fn(x) x != "")

// Split a list of paths delimited by `;`, and by `:` except on Windows, as
// upstream `undelimPaths`. Leading spaces of an entry are dropped, and a drive
// letter (`c:`) at the start of an entry is not a delimiter.
pub fun undelim-paths(s: string): list<string>
  val colon-delimits = unsafe-total{ get-os-name() != "windows" }
  fun is-delim(c: char): bool
    c == ';' || (c == ':' && colon-delimits)
  // `p` is the current entry, reversed
  fun split(cs: list<char>, p: list<char>): list<string>
    match cs
      Cons(c, rest) | p.is-nil && c.is-white -> split(rest, p)
      Cons(d, Cons(':', rest)) | p.is-nil -> split(rest, [':', d])
      Cons(c, rest) | c.is-delim -> Cons(p.reverse.string, split(rest, []))
      Cons(c, rest) -> split(rest, Cons(c, p))
      Nil -> [p.reverse.string]
  split(s.list, []).filter(fn(x) x != "")

// An integer optionally suffixed with k/kb/m/mb/g/gb
pub fun parse-size(s0: string): maybe<int>
  val s = s0.to-lower
  fun with-suffix(suf: string, mult: int): maybe<int>
    if s.ends-with(suf).bool then s.truncate(s.count - suf.count).trim.parse-int.map(fn(n) n * mult)
    else Nothing
  val suffixed = [("kib",1024),("kb",1024),("k",1024),
                   ("mib",1024*1024),("mb",1024*1024),("m",1024*1024),
                   ("gib",1024*1024*1024),("gb",1024*1024*1024),("g",1024*1024*1024)]
  match suffixed.map(fn((suf, mult)) with-suffix(suf, mult)).find(fn(m) m.is-just)
    Just(m) -> m
    Nothing -> s.trim.parse-int

pub fun read-html-bases(s: string): list<(string,string)>
  fun split-comma(xs: list<char>): list<list<char>>
    val (pre, ys) = xs.span(fn(c) c != ',')
    match ys
      Cons(_, post) -> Cons(pre, post.pretend-decreasing.split-comma)
      Nil -> [pre]
  fun to-base(cs: list<char>): (string,string)
    val (pre, ys) = cs.span(fn(c) c != '=')
    match ys
      Cons(_, post) -> (pre.string, post.string)
      Nil -> ("", cs.string)
  s.list.split-comma.map(to-base)

val target-archs: list<string> = ["x64","arm64","x86","riscv64","riscv32"]

// ----------------------------------------------------------------------
// the flag table (upstream `options`, Options.hs:420-523)
// ----------------------------------------------------------------------

// a boolean flag with an auto `--no-<name>` negation (`std/os/flags`'s
// `Bool` parser already implements upstream's `flag`/`--no-` pairing and
// the `--foo=true|false` forms, so this needs only one entry -- upstream
// spells out both)
fun bflag(short: string, long: list<string>, f: (flags, bool) -> flags, desc: string): flag<flags>
  Flag(short, long, Bool(f), desc)

// upstream `numOption`/`fnum`: an optional-integer flag with a default when
// bare, PLUS a `--no-<name>` that sets `f(-1)` (disable) -- ported as two
// table entries.
fun num-option(def: int, short: string, long: list<string>, f: (flags, int) -> flags, desc: string): list<flag<flags>>
  val setter = fn(fl: flags, mbs: maybe<string>)
    match mbs
      Nothing -> f(fl, def)
      Just(s) -> match s.parse-int
        Just(i) -> f(fl, i)
        Nothing -> f(fl, def)
  [ Flag(short, long, Opt(setter, "n"), desc),
    Flag("", long.map(fn(l) "no-" ++ l), Bool(fn(fl, _b) f(fl, -1)), "") ]

// A hidden feature flag whose
// long names are `f`-PREFIXED -- `--ftrmc` enables and `--fno-trmc` disables
// (the reference test suite's config.json files use these spellings, e.g.
// cgen/specialize passes `--fno-trmc`). Note `fno-` does not go through the
// std/os/flags automatic `no-` negation (that would be `--no-ftrmc`), so the
// negative form is its own table entry.
fun fflag(long: list<string>, f: (flags, bool) -> flags, desc: string): list<flag<flags>>
  [ Flag("", long.map(fn(l) "f" ++ l), Bool(f), desc),
    Flag("", long.map(fn(l) "fno-" ++ l), Bool(fn(fl, _b) f(fl, False)), "") ]

// The numeric analog of `fflag` (`--fsimplify[=n]` /
// `--fno-simplify` which passes -1)
fun fnum(def: int, long: list<string>, f: (flags, int) -> flags, desc: string): list<flag<flags>>
  val setter = fn(fl: flags, mbs: maybe<string>)
    match mbs
      Nothing -> f(fl, def)
      Just(s) -> match s.parse-int
        Just(i) -> f(fl, i)
        Nothing -> f(fl, def)
  [ Flag("", long.map(fn(l) "f" ++ l), Opt(setter, "n"), desc),
    Flag("", long.map(fn(l) "fno-" ++ l), Bool(fn(fl, _b) f(fl, -1)), "") ]

// upstream `config`/`configstr`: a required string argument, validated
// against a fixed set of choices
fun config-str(short: string, long: list<string>, opts: list<string>, argDesc: string, f: (flags, string) -> flags, desc: string): flag<flags>
  Flag(short, long, Req(f, if argDesc != "" then argDesc else "(" ++ opts.join("|") ++ ")"), desc)

// shared shape for `--include`/`--ccincdir`/`--cclibdir`/`--cclibpath`
// (semi-colon-or-comma-delimited path list, OptArg empty resets it) and
// `--ccopts`/`--cclinkopts` (shell-quoted arg list, same reset-on-empty rule)
fun opt-append(get: (flags) -> list<string>, set: (flags, list<string>) -> flags, extract: (string) -> list<string>): ((flags, maybe<string>) -> flags)
  fn(fl, mbs)
    match mbs
      Just(s) | s != "" -> set(fl, get(fl) ++ s.extract)
      _ -> set(fl, [])

fun target-flag(fl: flags, s: string): flags
  match s.target-platform-from-string
    Just(tgt) ->
      val fl1 = fl(target = tgt.tpl-target, targetPlatform = tgt.tpl-platform)
      fl1
    Nothing -> fl

fun target-arch-flag(fl: flags, s: string): flags
  if target-archs.any(fn(a) a == s) then fl(targetArch = s) else fl

// upstream `options` (Options.hs:420-523) returns TWO lists (visible/hidden,
// `fst`/`snd`) and `commandLineHelp` shows only `fst`, silently dropping the
// hidden ones from --help. This port keeps that same visible/internal split
// (`koka-flags-visible`/`koka-flags-internal`, still concatenated into
// `koka-flags` for PARSING -- every flag works either way) but documents the
// internal ones too, appended LAST under their own header in `usage()`
// rather than omitting them (see module doc comment).
pub val koka-flags-visible: list<flag<flags>> = [
  Flag("?h", ["help"], Bool(fn(fl, b) fl(wantHelp = b)), "show this information"),
  Flag("", ["version"], Bool(fn(fl, b) fl(wantVersion = b)), "show the compiler version"),
  Flag("p", ["prompt"], Bool(fn(fl, b) fl(wantInteractive = b)), "interactive mode"),
  Flag("", ["language-server"], Bool(fn(fl, b) fl(wantLanguageServer = b)), "language server mode"),
  bflag("e", ["execute"], fn(fl, b) fl(evaluate = b), "compile and execute"),
  bflag("c", ["compile"], fn(fl, b) fl(evaluate = !b), "only compile, do not execute (default)"),
] ++ num-option(16, "j", ["jobs"], fn(fl, i) fl(maxConcurrency = max(i, 1)), "maximum concurrency (16)") ++ [
  // OptArg, resets the path when given without a value
  Flag("i", ["include"], Opt(opt-append(fn(fl) fl.includePath, fn(fl, v) fl(includePath = v), undelim-paths), "dirs"),
    "add <dirs> to module search path (empty resets)"),
  Flag("o", ["output"], Req(fn(fl, s) fl(outFinalPath = s), "file"), "write executable to <file> (without extension)"),
] ++ num-option(0, "O", ["optimize"], fn(fl, i) fl(optimize = i), "optimize (0=default,1=space,2=full,3=aggressive)") ++ [
  bflag("g", ["debug"], fn(fl, b) fl(debug = b), "emit debug information (off by default)"),
] ++ num-option(1, "v", ["verbose"], fn(fl, i) fl(verbose = i), "verbosity 'n' (0=quiet, 1=default, 2=trace)") ++ [
  bflag("r", ["rebuild"], fn(fl, b) fl(rebuild = b), "rebuild all"),
  bflag("l", ["library"], fn(fl, b) fl(library = b, evaluate = if b then False else fl.evaluate), "generate a library"),
  config-str("", ["target"], target-platform-ids.map(fst), "target", target-flag,
    "target: " ++ target-platform-ids.map(fst).join(",")),
  config-str("", ["target-arch"], target-archs, "arch", target-arch-flag,
    "target architecture: " ++ target-archs.join(",")),

  Flag("", ["buildtag"], Req(fn(fl, s) fl(buildTag = s), "tag"), "set build variant tag (e.g. 'bundle' or 'dev')"),
  Flag("", ["builddir"], Req(fn(fl, s) fl(buildDir = s), "dir"), "build under <dir> ('.koka' by default)"),
  Flag("", ["buildname"], Req(fn(fl, s) fl(outBaseName = s), "name"), "base name of the final output"),
  bflag("", ["buildhash"], fn(fl, b) fl(useBuildDirHash = b), "use hash in build directory name"),
  Flag("", ["buildcfg"], Req(fn(fl, s) fl(buildDefs = fl.buildDefs ++ s.split(",").map(trim)), "def"), "add a build configuration definition (for use in 'buildcfg' conditionals)"),
  Flag("", ["outputdir"], Req(fn(fl, s) fl(outBuildDir = s), "dir"), "write intermediate files in <dir>"),

  Flag("", ["libdir"], Req(fn(fl, s) fl(localLibDir = s), "dir"), "object library <dir> (= <prefix>/lib/koka/<ver>)"),
  Flag("", ["sharedir"], Req(fn(fl, s) fl(localShareDir = s), "dir"), "source library <dir> (= <prefix>/share/koka/<ver>)"),
  Flag("", ["cc"], Req(fn(fl, s) fl(ccompPath = s), "cmd"), "use <cmd> as the C backend compiler"),
  Flag("", ["ccincdir"], Opt(opt-append(fn(fl) fl.ccompIncludeDirs, fn(fl, v) fl(ccompIncludeDirs = v), undelim-paths), "dirs"),
    "search semi-colon separated <dirs> for headers"),
  Flag("", ["cclibdir"], Opt(opt-append(fn(fl) fl.ccompLibDirs, fn(fl, v) fl(ccompLibDirs = v), undelim-paths), "dirs"),
    "search semi-colon separated <dirs> for libraries"),
  Flag("", ["cclib"], Req(fn(fl, s) fl(ccompLinkSysLibs = fl.ccompLinkSysLibs ++ s.undelim-paths), "libs"), "link with semi-colon separated system <libs>"),
  Flag("", ["ccopts"], Opt(opt-append(fn(fl) fl.ccompCompileArgs, fn(fl, v) fl(ccompCompileArgs = v), unquote), "opts"),
    "pass <opts> to C backend compiler"),
  Flag("", ["cclinkopts"], Opt(opt-append(fn(fl) fl.ccompLinkArgs, fn(fl, v) fl(ccompLinkArgs = v), unquote), "opts"),
    "pass <opts> to C backend linker"),
  Flag("", ["cclibpath"], Opt(opt-append(fn(fl) fl.ccompLinkLibs, fn(fl, v) fl(ccompLinkLibs = v), undelim-paths), "lpath"),
    "link with semi-colon separated libraries <lpath>"),
  Flag("", ["vcpkg"], Req(fn(fl, s) fl(vcpkgRoot = s), "dir"), "vcpkg root directory"),
  Flag("", ["vcpkgtriplet"], Req(fn(fl, s) fl(vcpkgTriplet = s), "tt"), "vcpkg target triplet"),
  Flag("", ["conan"], Req(fn(fl, s) fl(conan = s), "cmd"), "conan command"),
  bflag("", ["autoinstall"], fn(fl, b) fl(autoInstallLibs = b), "automatically download required packages"),
  Flag("", ["csc"], Req(fn(fl, s) fl(csc = s), "cmd"), "use <cmd> as the csharp backend compiler"),
  Flag("", ["node"], Req(fn(fl, s) fl(node = s), "cmd"), "use <cmd> to execute node"),
  Flag("", ["wasmrun"], Req(fn(fl, s) fl(wasmrun = s), "cmd"), "use <cmd> to execute wasm"),
  Flag("", ["editor"], Req(fn(fl, s) fl(editor = s), "cmd"), "use <cmd> as editor"),
  Flag("", ["stack"], Req(fn(fl, s) match s.parse-size { Just(n) -> fl(stackSize = n); Nothing -> fl }, "size"), "set stack size (0 for platform default)"),
  Flag("", ["heap"], Req(fn(fl, s) match s.parse-size { Just(n) -> fl(heapSize = n); Nothing -> fl }, "size"), "set reserved heap size (0 for platform default)"),
  Flag("", ["output-entry"], Req(fn(fl, s) fl(outputEntryName = s), "name"), "set the name of the generated main entrypoint (e.g. 'main')"),
  Flag("", ["main-entry"], Req(fn(fl, s) fl(mainEntryName = s), "name"), "set the name of the main entrypoint function (e.g. 'test1' or 'main')"),
  Flag("", ["color"], Req(fn(fl, s) fl(colorScheme = catch(fn(_) fl.colorScheme, { fl.colorScheme.read-color-flags(s) })), "colors"), "set colors (or a theme as --color=light|dark)"),
  Flag("", ["redirect"], Req(fn(fl, s) fl(redirectOutput = s), "file"), "redirect output to <file>"),
  config-str("", ["console"], ["ansi","html","raw"], "fmt", fn(fl, s) fl(console = s), "console output format: <ansi|html|raw>"),
] ++ num-option(-1, "", ["lsport"], fn(fl, i) fl(languageServerPort = i), "language server localhost port") ++ [
  bflag("", ["lsstdio"], fn(fl, b) fl(languageServerStdio = b), "use language Server over stdio"),

  bflag("", ["html"], fn(fl, b) fl(outHtml = if b then 2 else 0), "generate documentation"),
  Flag("", ["htmlbases"], Req(fn(fl, s) fl(htmlBases = fl.htmlBases ++ s.read-html-bases), "bases"), "set link prefixes for documentation"),
  Flag("", ["htmlcss"], Req(fn(fl, s) fl(htmlCss = s), "link"), "set link to the css documentation style"),

  bflag("", ["showtime"], fn(fl, b) fl(showElapsed = b), "show elapsed time and rss after evaluation"),
  bflag("", ["showspan"], fn(fl, b) fl(showSpan = b), "show ending row/column too on errors"),
  bflag("", ["showkindsigs"], fn(fl, b) fl(showKindSigs = b), "show kind signatures of type definitions"),
  bflag("", ["showtypesigs"], fn(fl, b) fl(showTypeSigsField = b), "show type signatures of definitions"),
  bflag("", ["showhiddentypesigs"], fn(fl, b) fl(showHiddenTypeSigs = b), "(implies --showtypesigs) show hidden type signatures of definitions"),
  bflag("", ["showsynonyms"], fn(fl, b) fl(showSynonyms = b), "show expanded type synonyms in types"),
  bflag("", ["showcore"], fn(fl, b) fl(showCore = b), "show core"),
  bflag("", ["showicore"], fn(fl, b) fl(showInitialCore = b), "show initial core (right after type checking)"),
  bflag("", ["showfcore"], fn(fl, b) fl(showFinalCore = b), "show final core (with backend optimizations)"),
  bflag("", ["showcoretypes"], fn(fl, b) fl(showCoreTypes = b), "show full types in core"),
  bflag("", ["coreprovenance"], fn(fl, b) fl(coreProvenance = b), "write specialization provenance comments into interfaces"),
  bflag("", ["showtypeids"], fn(fl, b) fl(showTypeIds = b), "show numeric type variable ids"),
  bflag("", ["showcs"], fn(fl, b) fl(showAsmCS = b), "show generated c#"),
  bflag("", ["showjs"], fn(fl, b) fl(showAsmJS = b), "show generated javascript"),
  bflag("", ["showc"], fn(fl, b) fl(showAsmC = b), "show generated C"),
  bflag("", ["core"], fn(fl, b) fl(genCore = b), "generate a core file"),
  bflag("", ["checkcore"], fn(fl, b) fl(coreCheck = b), "check generated core"),
]

// upstream `hide`d flags (Options.hs:500-521): rarely-needed internal
// switches and deprecated aliases. Still fully functional, just not
// clutter in the main --help listing -- see `koka-flags/usage`.
pub val koka-flags-internal: list<flag<flags>> =
  fflag(["asan"], fn(fl, b) fl(asan = b), "compile with address, undefined, and leak sanitizer") ++
  fflag(["profile"], fn(fl, b) fl(profile = b), "compile with profiling support (-pg, frame pointers, debug info)") ++
  fflag(["stdalloc"], fn(fl, b) fl(useStdAlloc = b), "use the standard libc allocator") ++
  fflag(["allocstats"], fn(fl, b) fl(mimallocStats = b), "enable mimalloc statitistics") ++
  fnum(3, ["simplify"], fn(fl, i) fl(simplify = i), "enable 'n' core simplification passes") ++
  fnum(10, ["maxdup"], fn(fl, i) fl(simplifyMaxDup = i), "set 'n' as maximum code duplication threshold") ++
  fnum(10, ["inline"], fn(fl, i) fl(optInlineMax = i), "set 'n' as maximum inline threshold (=10)") ++
  fflag(["monadic"], fn(fl, b) fl(enableMon = b), "enable monadic translation") ++
  [bflag("", ["semi"], fn(fl, b) fl(semiInsert = b), "insert semicolons based on layout")] ++
  fflag(["binference"], fn(fl, b) fl(parcBorrowInference = b), "enable reuse inference (does not work cross-module!)") ++
  fflag(["reuse"], fn(fl, b) fl(parcReuse = b), "enable in-place update analysis") ++
  fflag(["dropspec"], fn(fl, b) fl(parcSpecialize = b), "enable drop specialization") ++
  fflag(["reusespec"], fn(fl, b) fl(parcReuseSpecialize = b), "enable reuse specialization") ++
  fflag(["trmc"], fn(fl, b) fl(optctail = b), "enable tail-recursion-modulo-cons optimization") ++
  fflag(["trmcctx"], fn(fl, b) fl(optctailCtxPath = b), "enable trmc context paths") ++
  fflag(["specialize"], fn(fl, b) fl(optSpecialize = b), "enable inline specialization") ++
  fflag(["unroll"], fn(fl, b) fl(optUnroll = if b then 1 else 0), "enable recursive definition unrolling") ++
  fflag(["eagerpatbind"], fn(fl, b) fl(optEagerPatBind = b), "load pattern fields as early as possible") ++
  fflag(["infchain"], fn(fl, b) fl(allowInfiniteChains = b), "allow infinite implicit chains") ++
  // deprecated
  [Flag("", ["cmake"], Req(fn(fl, s) fl(cmake = s), "cmd"), "use <cmd> to invoke cmake"),
   Flag("", ["cmakeopts"], Req(fn(fl, s) fl(cmakeArgs = s), "opts"), "pass <opts> to cmake")]

// every flag parses regardless of visible/internal
pub val koka-flags: list<flag<flags>> = koka-flags-visible ++ koka-flags-internal

// visible flags under the normal header, internal ones appended LAST under
// their own header (see module doc comment)
pub fun koka-flags/usage(header: string = "usage:\n koka [options] files\n\noptions:"): string
  koka-flags-visible.usage(header) ++ "\n" ++
    koka-flags-internal.usage("internal options (rarely needed, use with caution):")

// ----------------------------------------------------------------------
// parse-options (upstream `parseOptions`, Options.hs:878)
// ----------------------------------------------------------------------

// Split at "--" (everything after becomes
// `execOpts`), run `getOpt`, then determine `mode` from the accumulated
// flags -- `Left(err)` on any parse error, with every error concatenated.
pub fun parse-options(fl0: flags, args: list<string>): either<string, (flags, mode)>
  val (preOpts, postOpts) = args.span(fn(a) a != "--")
  val fl1 = match postOpts
    Cons(_, rest) -> fl0(execOpts = rest)
    Nil -> fl0
  val (fl, files, errs) = parse(fl1, koka-flags, preOpts)
  if errs.is-nil then
    val mode =
      if fl.wantHelp then ModeHelp
      elif fl.wantVersion then ModeVersion
      elif fl.wantInteractive then ModeInteractive(files)
      elif fl.wantLanguageServer then ModeLanguageServer(files)
      elif files.is-nil then ModeInteractive(files)
      else ModeCompiler(files)
    Right((fl, mode))
  else Left(errs.join(""))

// ----------------------------------------------------------------------
// environment probe (upstream `processInitialOptions` tail, Options.hs:831-978)
// ----------------------------------------------------------------------

// upstream `hostArch`/`getTargetArch`: normalize the OS-reported CPU arch
// name to koka's own vocabulary (x64/arm64/x86/...). Upstream additionally
// special-cases Windows-on-ARM (`PROCESSOR_IDENTIFIER`); not ported here.
pub fun host-arch(): ndet string
  match get-cpu-arch().to-lower
    "aarch64" -> "arm64"
    "armv8" -> "arm64"
    "armv9" -> "arm64"
    "x86_64" -> "x64"
    "x86-64" -> "x64"
    "amd64" -> "x64"
    "i386" -> "x86"
    "powerpc" -> "ppc"
    "powerpc64" -> "ppc64"
    "powerpc64le" -> "ppc64le"
    arch -> arch

pub fun host-os-name(): ndet string
  match get-os-name()
    "windows" -> "windows"
    "macos" -> "macos"
    _ -> "linux"

// search `PATH` (with `extra` dirs prepended and `extra-after` dirs appended) for
// an executable literally named `name` (upstream `searchPaths`/`findCC`/`findEditor`'s inner loop)
pub fun search-path(name: string, extra: list<string> = [], extra-after: list<string> = []): <ndet,fsys> maybe<string>
  val dirs = extra ++ get-env-value("PATH").default("").split(pathsep()) ++ extra-after
  fun try(ds: list<string>): <ndet,fsys> maybe<string>
    match ds
      Nil -> Nothing
      Cons(d, rest) ->
        val cand = (d ++ "/" ++ name ++ exe-extension())
        if cand.path.is-file then Just(cand) else try(rest.pretend-decreasing)
  try(dirs)

// upstream `detectCC`/`findCC`: search PATH for a working C compiler,
// preferring `$CC`, then platform-appropriate defaults, then a generic
// fallback list; "gcc" (unresolved) if nothing is found on PATH at all.
pub fun detect-cc(t: target): <ndet,fsys> string
  // find LLVM after an initial install, when its directory may not be on PATH yet
  val has-llvm = get-env-value("PATH").default("").split(pathsep()).any(fn(d) d.to-lower.find("llvm").is-just)
  val extra = if on-windows() && !has-llvm then ["C:\\Program Files\\LLVM\\bin"] else []
  val envCC = get-env-value("CC").default("")
  val candidates =
    (if t.is-target-wasm then ["emcc"] else []) ++
    (if envCC != "" then [envCC] else []) ++
    (if on-macos() then ["clang"] else []) ++
    (if on-windows() then ["clang-cl","cl"] else []) ++
    ["gcc","clang","icc","cc","g++","clang++"]
  fun try(cs: list<string>): <ndet,fsys> string
    match cs
      Nil -> "gcc"
      Cons(name, rest) ->
        val extra-after = if on-windows() && name.starts-with("clang").bool then ["C:/Program Files/LLVM/bin"] else []
        match search-path(name, extra, extra-after)
          Just(p) -> p
          Nothing -> try(rest.pretend-decreasing)
  try(candidates)

// Warn (not fail) when the resolved cc's path can't
// be located on disk / PATH
pub fun cc-check-exist(c: cc): <ndet,fsys,console> ()
  val found = if c.ccPath.starts-with("/").bool then c.ccPath.path.is-file
              else search-path(c.ccPath).is-just
  if !found then
    println("\nwarning: cannot find the C compiler: " ++ c.ccPath)
    if c.ccName == "cl" then println("   hint: run in an x64 Native Tools command prompt? or use the --cc=clang-cl flag?")
    if c.ccName == "clang-cl" then println("   hint: install clang for Windows from <https://github.com/llvm/llvm-project/releases/latest> ?")

// Resolve the `cc` value for
// toolchain `path`'s name, then adjust it per `fl` (target/arch/asan/
// profile/stdalloc/allocstats). Returns `(cc, asanEffective)` since `--fasan`
// silently degrades to a no-op (with a warning) outside clang/gcc.
pub fun cc-from-path(fl: flags, cmdPath: string): <ndet,console> (cc, bool)
  val name = cmdPath.path.basename
  val gcc = cc-gcc(name, cmdPath, True)
  val mingw = gcc(
      ccName = "mingw",
      ccLibFile = fn(lib) "lib" ++ lib ++ ".a",
      ccFlagStack = fn(stksize) if stksize > 0 then ["-Wl,--stack," ++ stksize.show] else [])
  val emcc0 = cc-gcc(name, cmdPath, False)
  val emcc = emcc0(
      ccFlagsCompile = emcc0.ccFlagsCompile ++ ["-D__wasi__"],
      ccFlagsLink = emcc0.ccFlagsLink ++ ["-sWASM_BIGINT=1","-sEXPORTED_RUNTIME_METHODS=ccall,cwrap"],
      ccFlagStack = fn(stksize) if stksize == 0 then [] else ["-s","TOTAL_STACK=" ++ stksize.show],
      ccFlagHeap = fn(hpsize) if hpsize == 0 then [] else ["-s","TOTAL_MEMORY=" ++ hpsize.show],
      ccTargetExe = fn(out) ["-o", out ++ target-exe-extension(fl.target)],
      ccTargetObj = fn(fname) ["-o", fname.strip-ext ++ target-obj-extension(fl.target)],
      ccObjFile = fn(fname) fname ++ target-obj-extension(fl.target),
      ccLibFile = fn(fname) target-lib-file(fl.target, fname))
  val macos = on-macos()
  val clang = gcc(ccFlagsWarn = gnu-warn ++
      ["-Wno-cast-qual","-Wno-undef","-Wno-reserved-id-macro","-Wno-unused-macros","-Wno-cast-align"] ++
      (if macos && fl.targetArch == "arm64" then ["-Wno-unknown-warning-option"] else []),
    ccFlagsOpt = fn(opt, cpuArch) (gcc.ccFlagsOpt)(opt, cpuArch) ++
      (if macos && opt >= 2 && cpuArch == "arm64" then ["-mno-outline","-Xclang","-fno-split-cold-code"] else []) ++
      (if opt >= 2 && cpuArch == "arm64" then ["-falign-functions=64"] else []))
  // PORT-SPECIFIC: clang for a wasm target is wasi-sdk's (clang with wasi-libc).
  // Upstream only has emcc for wasm, but emscripten's standalone WASI output has
  // no file system (its `open` accepts only the standard streams), which a
  // compiler cannot do without. WASI has no threads, process clocks, pids or
  // signals; wasi-libc's emulations stand in for the ones kklib links against.
  val wasi-emulated = ["pthread", "process-clocks", "getpid", "signal"]
  val clang-wasi = clang(
      ccFlagsCompile = clang.ccFlagsCompile ++
        wasi-emulated.map(fn(e) "-D_WASI_EMULATED_" ++ e.to-upper.replace-all("-", "_")),
      ccFlagsLink = clang.ccFlagsLink ++ wasi-emulated.map(fn(e) "-lwasi-emulated-" ++ e),
      ccFlagStack = fn(stksize) if stksize == 0 then [] else ["-Wl,-z,stack-size=" ++ stksize.show],
      ccFlagHeap = fn(hpsize) if hpsize == 0 then [] else ["-Wl,--initial-memory=" ++ hpsize.show],
      ccTargetExe = fn(out) ["-o", out ++ target-exe-extension(fl.target)])
  val generic = gcc(ccFlagsWarn = [])
  val msvc = cc-msvc(name, cmdPath)
  val clangcl = msvc(
      ccFlagsWarn = ["-Wno-everything"] ++ clang.ccFlagsWarn ++
        ["-Wno-extra-semi-stmt","-Wno-extra-semi","-Wno-float-equal"],
      ccFlagsLink = ["-Wno-unused-command-line-argument"] ++ msvc.ccFlagsLink,
      ccFlagsCompile = ["-D__clang_msvc__"] ++ msvc.ccFlagsCompile)

  val cc0 =
    if name.starts-with("clang-cl").bool then clangcl
    elif name.starts-with("mingw").bool then mingw
    elif name.starts-with("emcc").bool then emcc
    elif name.starts-with("clang").bool && fl.target.is-target-wasm then clang-wasi
    elif name.starts-with("clang").bool || name.starts-with("musl-clang").bool then clang
    elif name.starts-with("musl-gcc").bool || name.starts-with("musl-g++").bool then gcc
    elif name.starts-with("gcc").bool || name.starts-with("g++").bool then (if on-windows() then mingw else gcc)
    elif name.starts-with("cl").bool then msvc
    elif name == "icc" then gcc
    elif name == "cc" then generic
    else gcc

  val cc1 = cc0(
      ccFlagsCompile = cc0.ccFlagsCompile ++ fl.ccompCompileArgs,
      ccFlagsLink = cc0.ccFlagsLink ++ fl.ccompLinkArgs)

  if fl.target.is-target-wasm && !name.starts-with("emcc").bool && !name.starts-with("clang").bool then
    println("\nwarning: a wasm target should use the emscripten compiler (emcc) or wasi-sdk's clang,\n  but currently '" ++ cc1.ccPath ++ "' is used." ++
      "\n  hint: specify the compiler path using --cc=<emcc or wasi-sdk clang path>?")

  if fl.asan then
    if !(cc1.ccName.starts-with("clang").bool || cc1.ccName.starts-with("gcc").bool || cc1.ccName.starts-with("g++").bool) then
      println("warning: can only use address sanitizer with clang or gcc (--fasan is ignored)")
      (cc1, False)
    else
      val sanitize = if on-macos() then "-fsanitize=address,undefined" else "-fsanitize=address,undefined,leak"
      (cc1(ccName = cc1.ccName ++ "-asan",
           ccFlagsCompile = cc1.ccFlagsCompile ++ [sanitize,"-fno-omit-frame-pointer","-O0"],
           ccFlagsLink = cc1.ccFlagsLink ++ [sanitize]), True)
  elif fl.profile then
    val isClang = cc1.ccName.starts-with("clang").bool
    if isClang then
      (cc1(ccName = cc1.ccName ++ "-profile",
           ccFlagsCompile = cc1.ccFlagsCompile ++ ["-fprofile-instr-generate","-fcoverage-mapping","-g","-fno-omit-frame-pointer"],
           ccFlagsLink = cc1.ccFlagsLink ++ ["-fprofile-instr-generate"]), True)
    else
      (cc1(ccName = cc1.ccName ++ "-profile",
           ccFlagsCompile = cc1.ccFlagsCompile ++ ["-pg","-g3","-fno-omit-frame-pointer"],
           ccFlagsLink = cc1.ccFlagsLink ++ ["-pg"]), True)
  elif fl.useStdAlloc then
    (cc1(ccName = cc1.ccName ++ "-stdalloc"), False)
  elif fl.mimallocStats then
    (cc1(ccName = cc1.ccName ++ "-allocstats"), False)
  else
    (cc1, False)

// upstream `detectEditor`/`findEditor`
fun detect-editor(): <ndet,fsys> string
  fun try(cs: list<(string,string)>): <ndet,fsys> string
    match cs
      Nil -> ""
      Cons(entry, rest) ->
        val (name, opts) = entry
        match search-path(name)
          Just(_) -> name ++ " " ++ opts
          Nothing -> try(rest.pretend-decreasing)
  try([("code","--goto %f:%l:%c"), ("atom","%f:%l:%c")])

// upstream `rootDirFrom`/`getKokaDirs`: sniff the running binary's own
// location to find the root of the installation or the repository.
//
// An INSTALLED compiler lives at `<prefix>/bin/koka`, so its root is the
// prefix. Otherwise the binary is somewhere under a repository's build
// output (`.koka/v<version>[-<tag>]/<target>-<buildtype>-<hash>/`, or
// `.koka/bootstrap/` for a pinned one): walk up to the directory that HAS
// `kklib`, rather than counting levels, so where the binary sits inside the
// tree does not matter.
fun root-dir-from(bin-dir: path): io path
  if bin-dir.basename == "bin" then bin-dir.dirname.path
  else
    fun walk(d: path, fuel: int): io maybe<path>
      if fuel <= 0 then Nothing
      elif (d / "kklib".path).dir/is-directory then Just(d)
      else
        val up = d.dirname.path
        if up.string == d.string then Nothing else walk(up, fuel - 1)
    match walk(bin-dir, 8)
      Just(root) -> root
      Nothing    -> bin-dir.dirname.path.dirname.path.dirname.path

// The root directory is derived from the binary's own
// location, and `<root>/kklib` decides whether that root is this REPOSITORY
// (then it is itself the share dir, holding `lib` and `kklib`) or an INSTALL
// PREFIX (then the sources live in `<prefix>/share/koka/v<version>` and the
// compiled objects in `<prefix>/lib/koka/v<version>` -- the layout
// `util/bundle.kk` installs).
fun get-koka-dirs(libDir1: string, shareDir1: string): <io,comp-flags> (string, string, string, string)
  val binDir = appdir()
  val rootDir = root-dir-from(binDir)
  val isRootRepo = (rootDir / "kklib".path).dir/is-directory
  val libDir = if libDir1 != "" then libDir1
               elif isRootRepo then (rootDir / ".koka".path).string
               else (rootDir / "lib".path / "koka".path / ("v" ++ comp-flags.version).path).string
  val shareDir = if shareDir1 != "" then shareDir1
                 elif isRootRepo then rootDir.string
                 else (rootDir / "share".path / "koka".path / ("v" ++ comp-flags.version).path).string
  (rootDir.string, libDir, shareDir, binDir.string)

// upstream `discoverPackages` (Compile/Package.hs) -- see module doc comment;
// real workspace-package discovery is not ported.
pub fun discover-packages(_buildDir: string): packages
  Packages()

// Env-var -> flag-string table.
// (`koka_options`' extractor really does return the raw string as a SINGLE
// list element -- NOT word-split. This is the reference compiler's behaviour
// (`flagsEnv s = [s]`), not a port bug.)
pub val environment: list<(string,string,(string) -> list<string>,string)> = [
  ("koka_options", "options", fn(s) [s], "Add <options> to the command line"),
  ("koka_editor", "command", fn(s) ["--editor=" ++ s], "Use <cmd> as the editor (substitutes %l, %c, and %f)"),
  ("koka_vcpkg", "dir", fn(s) ["--vcpkg=" ++ s], "Set vcpkg root directory"),
  ("koka_lib_dir", "dir", fn(s) ["--libdir=" ++ s], "Set the koka compiled library directory (= '<prefix>/lib/koka/<ver>')"),
  ("koka_share_dir", "dir", fn(s) ["--sharedir=" ++ s], "Set the koka library sources directory (= '<prefix>/share/koka/<ver>')"),
  ("koka_build_dir", "dir", fn(s) ["--builddir=" ++ s], "Set the default koka build directory (= '.koka')"),
]

// upstream `getEnvOptions` (minus `getEnvCsc`'s Windows .NET-Framework
// registry-style probing -- the C# backend isn't ported, see module doc comment)
pub fun get-env-options(): ndet list<string>
  environment.flatmap fn(entry)
    val (envName, _, extract, _) = entry
    match get-env-value(envName)
      Just(s) | s != "" -> extract(s)
      _ -> []

// `parse-options` plus the
// full environment probe -- arch/os defaulting, evaluate/genRangeMap mode
// bias, build tag (WSL distro name), editor/package/koka-dir discovery,
// C-toolchain detection (`detect-cc`/`cc-from-path`/`cc-check-exist`), and
// `includePath` normalization (upstream prepends `.` and `<shareDir>/lib`;
// this port prepends `<shareDir>/lib`, `shareDir` sniffed by `get-koka-dirs`
// and defaulting to the repository root; the entry file's
// own directory, upstream's role for `.`, is prepended separately at each
// call site -- see `check-file-ex`'s `srcdirs`).
pub fun process-initial-options(flags0: flags, opts: list<string>): <io,comp-flags> either<string,(flags,mode)>
  match parse-options(flags0, opts)
    Left(err) -> Left(err)
    Right((fl1, mode)) ->
      // a wasm target runs on no host CPU, so neither the host's CPU flags
      // (`-march`) nor its `arch=` conditional imports apply to it
      val arch = if fl1.targetArch != "" then fl1.targetArch
                 elif fl1.target.is-target-wasm then "wasm" ++ (8 * fl1.targetPlatform.ptrSize).show
                 else host-arch()
      val os = if fl1.targetOS != "" then fl1.targetOS else host-os-name()
      val fl2 = fl1(targetArch = arch, targetOS = os)
      val fl3 = match mode
        ModeInteractive(_) -> fl2(evaluate = True)
        ModeLanguageServer(_) -> fl2(genRangeMap = True)
        _ -> fl2

      val buildDir = if fl3.buildDir != "" then fl3.buildDir else ".koka"
      val buildTag = if fl3.buildTag != "" then fl3.buildTag
                     else get-env-value("WSL_DISTRO_NAME").default("").to-lower
      val ed = if fl3.editor != "" then fl3.editor else detect-editor()
      val pkgs = discover-packages(buildDir)
      val (_rootDir, localLibDir, localShareDir, localBinDir) = get-koka-dirs(fl3.localLibDir, fl3.localShareDir)

      val ccmd = if fl3.ccompPath != "" then (if fl3.ccompPath == "mingw" then "gcc" else fl3.ccompPath)
                 else detect-cc(fl3.target)
      val (ccres, asanRes) = cc-from-path(fl3, ccmd)
      cc-check-exist(ccres)

      val shareLibDir = localShareDir ++ "/lib"
      // the C target needs kklib's headers; they sit next to `lib` under the
      // share dir, in the repository and in an install alike
      val kklibInclude = if fl3.kklibIncludeDir != "" then fl3.kklibIncludeDir
                         else match fl3.target
                           C -> kklib-include-of(localShareDir)
                           _ -> ""
      // the current directory and the standard library are always searched,
      // before the given includes, each once, and each by its real path, as
      // upstream `realPath`s them. The string `realpath` resolves `..` against
      // the current directory; normalizing a relative `../..` as a path first
      // would drop it.
      val includePath = (["."] ++ [shareLibDir] ++ fl3.includePath).map(fn(p) p.string/realpath.string)
                          .foldl([]) fn(acc, p) if acc.any(fn(q) q == p) then acc else acc ++ [p]

      val fl4 = fl3(
        packages = pkgs,
        buildDir = buildDir,
        buildTag = buildTag,
        localBinDir = localBinDir,
        localLibDir = localLibDir,
        localShareDir = localShareDir,
        kklibIncludeDir = kklibInclude,
        ccompPath = ccmd,
        ccomp = ccres,
        asan = asanRes,
        editor = ed,
        includePath = includePath,
        genRangeMap = fl3.outHtml > 0 || fl3.genRangeMap)
      // ProcessInitialOptions then
      // `processDerivedOptions flags1 flags1` (defaultFlags == the freshly
      // resolved flags themselves, at this top-level entry point)
      Right((process-derived-options(fl4, fl4), mode))

fun is-target-c(t: target): bool
  match t
    C -> True
    _ -> False

// Translate a host OS name to vcpkg's own vocabulary
fun triplet-os-name(osName: string): string
  match osName
    "linux-android" -> "android"
    "mingw32" -> "mingw-static"
    "darwin" -> "osx"
    "macos" -> "osx"
    os -> os

// Apply a further command line (the language
// server's clients send one as a single string) on top of the flags this
// session started with. It re-derives from `baseFlags` -- the PRE-derived
// snapshot -- so the derived settings are computed once from the final answer
// rather than layered on top of an already-derived set.
pub fun process-extra-options(flags0: flags, args: string): ndet either<string, (flags, mode)>
  val defaultFlags = flags0.baseFlags.default(flags0)
  match parse-options(defaultFlags, args.split(" ").filter(fn(w) w != ""))
    Left(err) -> Left(err)
    Right((flags1, mode)) -> Right((process-derived-options(defaultFlags, flags1), mode))

// Fields purely DERIVED
// from the already-resolved flags -- asan-implies-stdalloc, the vcpkg
// triplet, and the optimize-level-driven inline/unroll/specialize knobs.
// `baseFlags` snapshots the flags this was derived from, for
// `process-extra-options`.
// `ccompDefs` is what every C compile passes (link.kk `ccompile-args`, as
// upstream `ccompile`), kklib's included -- so all translation units agree on
// `KK_MIMALLOC`/`KK_DEBUG_FULL`, which gate the layout of `kk_context_t`.
pub fun process-derived-options(defaultFlags: flags, fl: flags): ndet flags
  val stdAlloc = if fl.asan then True else fl.useStdAlloc
  val cdefs: list<(string,string)> =
    fl.ccompDefs ++
    (if stdAlloc then [] else [("KK_MIMALLOC", fl.targetPlatform.ptrSize.show)]) ++
    (if fl.build-type <= DebugFull then [("KK_DEBUG_FULL","")] else []) ++
    (if fl.optctailCtxPath then [] else [("KK_CTAIL_NO_CONTEXT_PATH","")]) ++
    (if fl.targetPlatform.has-compressed-fields then [("KK_INTB_SIZE", fl.targetPlatform.fieldSize.show)] else []) ++
    (if !stdAlloc && fl.mimallocStats then [("MI_STAT","2")] else [])
  val triplet =
    if fl.vcpkgTriplet != "" then fl.vcpkgTriplet
    elif fl.target.is-target-wasm then "wasm" ++ (8 * fl.targetPlatform.ptrSize).show ++ "-emscripten"
    else fl.targetArch ++
      (if on-windows() then (if fl.ccomp.ccName.starts-with("mingw").bool then "-mingw-static" else "-windows-static-md")
       else "-" ++ triplet-os-name(fl.targetOS))
  fl(
    outBaseName = if fl.outBaseName == "" && fl.outFinalPath != "" then fl.outFinalPath.path.basename else fl.outBaseName,
    optSpecialize = if fl.optimize <= 0 then False else fl.optSpecialize,
    optInlineMax = if fl.optimize < 0 then 0
                   elif fl.optimize <= 1 then fl.optInlineMax / 3
                   else fl.optInlineMax,
    optctailCtxPath = fl.optctailCtxPath && fl.target.is-target-c,
    optUnroll = if fl.optUnroll < 0 then (if fl.optimize > 0 then 1 else 0) else fl.optUnroll,
    useStdAlloc = stdAlloc,
    vcpkgTriplet = triplet,
    ccompDefs = cdefs,
    baseFlags = Just(defaultFlags))
