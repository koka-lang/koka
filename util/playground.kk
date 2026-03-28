/*-----------------------------------------------------------------------------
Copyright 2024-2025, Tim Whiting, Microsoft Research, Daan Leijen.
This is free software; you can redistribute it and/or modify it under the
terms of the Apache License, Version 2.0. A copy of the License can be
found in the LICENSE file at the root of this distribution.

Setup the Koka web playground for local development.
Builds WASM compiler + LSP, precompiles stdlib, deploys web assets.

Usage:
  stack exec koka -- -e util/playground [-- [options]]
  koka -e util/playground [-- [options]]
-----------------------------------------------------------------------------*/
import std/os/path
import std/os/dir
import std/os/process
import std/os/env
import std/os/flags
import std/os/file

val header = "usage:\n koka -e util/playground [-- [options]]\n\noptions:"

struct pflags
  backend   : string = "all"
  verbose   : bool = False

val flag-descs : list<flag<pflags>> =
  fun set-backend( f : pflags, s : string ) : pflags { f(backend = s) }
  fun set-verbose( f : pflags, b : bool ) : pflags { f(verbose = b) }
  [ Flag("b", ["backend"], Req(set-backend, "TARGET"),
         "build target: all, wasm, lsp, precompile, deploy, web"),
    Flag("v", ["verbose"], Bool(set-verbose), "verbose output")
  ]

// ── Samples that don't work on the web playground ────────────────────────
val excluded-samples = [
  "samples/all.kk",                 // replaced by web-compatible version
  "samples/basic/rbtree.kk",        // FBIP BigInt bug in JS backend
  "samples/basic/rbtree-fbip.kk",   // same FBIP BigInt bug
  "samples/learn/lazycons.kk",      // experimental lazy constructors
  "samples/learn/implicits-talk.kk" // talk version, not a standalone sample
]

fun is-excluded( fpath : string ) : bool
  excluded-samples.any fn(e) { fpath.ends-with(e).is-just }

// ── Helpers ──────────────────────────────────────────────────────────────

fun info(msg : string) : console ()
  println("\x1b[32m[playground]\x1b[0m " ++ msg)

fun warn(msg : string) : console ()
  println("\x1b[33m[playground]\x1b[0m " ++ msg)

fun run-or-fail(cmd : string, msg : string = "") : io ()
  val code = run-system(cmd)
  if code != 0 then
    if msg.is-empty then
      throw("command failed with exit code " ++ code.show ++ ": " ++ cmd)
    else
      throw(msg)

fun find-wasm(name : string) : io string
  match run-system-read("find dist-newstyle -name '" ++ name ++ "' -path '*/wasm32-wasi/*' 2>/dev/null | head -1")
    Ok(s)   -> s.trim
    Error(_) -> ""

fun get-home() : ndet string
  match get-env-value("HOME")
    Just(h) -> h
    Nothing -> "~"

fun wasm-opt-path() : io string
  val home = get-home()
  val p = home ++ "/.ghc-wasm/binaryen/bin/wasm-opt"
  if is-file(p.path) then p
  else
    match run-system-read("which wasm-opt 2>/dev/null")
      Ok(s) | !s.trim.is-empty -> s.trim
      _     -> ""

// ── Collect .kk files from a directory, excluding broken samples ─────────
fun collect-samples( dir : path ) : <fsys,pure> list<path>
  list-directory-recursive(dir).filter fn(f)
    val s = f.string
    s.ends-with(".kk").is-just && !s.is-excluded

// ── Write a JSON array of strings to a file ──────────────────────────────
fun write-json-manifest( fpath : path, entries : list<string> ) : io ()
  val json = "[" ++ entries.map(fn(e) { "\"" ++ e ++ "\"" }).join(",") ++ "]"
  write-text-file(fpath, json)

// ── Build ────────────────────────────────────────────────────────────────

fun build-wasm(backend : string) : io ()
  info("Sourcing GHC WASM environment...")
  val home = get-home()
  val env-script = home ++ "/.ghc-wasm/env"
  if !is-file(env-script.path) then
    warn("~/.ghc-wasm/env not found. Install via:")
    warn("  FLAVOUR=9.12 sh <(curl https://gitlab.haskell.org/ghc/ghc-wasm-meta/-/raw/master/bootstrap.sh)")
    return ()

  val source-env = "unset GHC_PACKAGE_PATH && . " ++ env-script ++ " && "

  if backend == "all" || backend == "wasm" then
    info("Building Koka playground for WASM backend...")
    run-or-fail(source-env ++ "wasm32-wasi-cabal build lib:koka koka:exe:koka-playground")
    val wasm = find-wasm("koka-playground.wasm")
    val opt = wasm-opt-path()
    if !wasm.is-empty && !opt.is-empty then
      info("Optimizing playground WASM with wasm-opt -O4...")
      run-or-fail(source-env ++ opt ++ " -O4 " ++ wasm ++ " -o " ++ wasm.path.change-ext(".opt.wasm").string)
    info("WASM playground build done.")

  if backend == "all" || backend == "lsp" then
    info("Building Koka LSP server for WASM...")
    run-or-fail(source-env ++ "wasm32-wasi-cabal build koka:exe:koka-lsp-wasm")
    val lsp = find-wasm("koka-lsp-wasm.wasm")
    val opt = wasm-opt-path()
    if !lsp.is-empty && !opt.is-empty then
      info("Optimizing LSP WASM with wasm-opt -O4...")
      run-or-fail(source-env ++ opt ++ " -O4 " ++ lsp ++ " -o " ++ lsp.path.change-ext(".opt.wasm").string)
    info("WASM LSP build done.")

fun do-precompile() : io ()
  info("Building native Koka for precompilation...")
  val koka = match run-system-read("stack exec -- which koka")
    Ok(s)   -> s.trim
    Error(_) -> throw("could not find koka")
  info("Precompiling all std libraries...")
  ensure-dir("precompiled".path)
  run-or-fail(koka ++ " --target=js --outputdir=precompiled --library lib/toc.kk")
  // Precompile samples (discovered from filesystem, excluding broken ones)
  val samples = collect-samples("samples".path)
  if samples.is-cons then
    info("Precompiling " ++ samples.length.show ++ " samples...")
    val sample-args = samples.map(fn(f) { f.string }).join(" ")
    run-or-fail(koka ++ " --target=js --outputdir=precompiled --include=samples " ++ sample-args)
  info("Precompilation done.")

// ── Deploy ───────────────────────────────────────────────────────────────

val web-public : path = "web/playground/public".path

fun deploy() : io ()
  val pdir = web-public
  info("Deploying assets to " ++ pdir.string ++ "/...")
  ensure-dir(pdir / "lib")
  ensure-dir(pdir / "precompiled")

  // WASM compiler binary
  val wasm-file = find-wasm("koka-playground.opt.wasm")
  if !wasm-file.is-empty then
    copy-file(wasm-file.path, pdir / "koka-playground.wasm")
    info("Copied WASM compiler")
  else
    warn("WASM compiler binary not found")

  // WASM LSP binary
  val lsp-file = find-wasm("koka-lsp-wasm.opt.wasm")
  if !lsp-file.is-empty then
    copy-file(lsp-file.path, pdir / "koka-lsp.wasm")
    info("Copied WASM LSP")
  else
    warn("WASM LSP binary not found")

  // Stdlib sources
  copy-directory("lib/std".path, pdir / "lib" / "std")
  info("Copied stdlib sources")

  // Sample sources
  copy-directory("samples".path, pdir / "samples")
  info("Copied sample sources")

  // Cross-Origin Isolation service worker
  val coi-src = "web/playground/node_modules/coi-serviceworker".path / "coi-serviceworker.js"
  if is-file(coi-src) then
    copy-file(coi-src, pdir / "coi-serviceworker.js")
    info("Copied coi-serviceworker.js")
  else
    warn("coi-serviceworker.js not found — run 'npm install' in web/playground first")

  // Precompiled .kki + .mjs
  if is-directory("precompiled".path) then
    list-directory("precompiled".path).foreach fn(f)
      val name = f.nodir.string
      if name.ends-with(".kki").is-just || name.ends-with(".mjs").is-just then
        copy-file(f, pdir / "precompiled" / name)
    info("Copied precompiled files")
  else
    warn("No precompiled files. Run with --backend=precompile first")

  // Generate manifests
  info("Generating manifests...")
  generate-manifests()
  info("Manifests generated.")

fun strip-prefix( s : string, pre : string ) : string
  if s.starts-with(pre).is-just then s.slice.advance(pre.count).string
  else s

fun generate-manifest( dir : path, manifest-path : path, path-prefix : string, ext-filter : string = "" ) : io ()
  val files = list-directory-recursive(dir)
    .filter(fn(f) { is-file(f) })
    .map(fn(f) { f.string.strip-prefix(path-prefix) })
    .filter fn(s)
      !s.is-empty && (ext-filter.is-empty || s.ends-with(ext-filter).is-just)
  write-json-manifest(manifest-path, files)

fun generate-manifests() : io ()
  val pdir = web-public
  generate-manifest(pdir / "lib" / "std", pdir / "stdlib-manifest.json", (pdir / "lib").string ++ "/")
  generate-manifest(pdir / "precompiled", pdir / "precompiled-manifest.json", (pdir / "precompiled").string ++ "/")
  generate-manifest(pdir / "samples", pdir / "samples-manifest.json", (pdir / "samples").string ++ "/", ".kk")

// ── Web dev server ───────────────────────────────────────────────────────

fun start-web() : io ()
  info("Installing npm dependencies...")
  run-or-fail("cd web/playground && npm install")
  info("")
  info("=== Ready! ===")
  info("Starting dev server...")
  run-system("cd web/playground && npx vite --host")
  ()

// ── Main ─────────────────────────────────────────────────────────────────

pub fun main()
  if !is-directory("kklib".path) then
    println("Error: run from the koka root directory")
    println(flag-descs.usage(header))
    return ()

  // Generate cabal file
  run-or-fail("hpack", "hpack not found. Install via: cabal install hpack")

  val (flags, args, errs) = parse(Pflags(), flag-descs, get-args())
  if errs.is-cons then
    println(errs.join("\n") ++ "\n" ++ flag-descs.usage(header))
    return ()

  val backend = if args.is-cons then args.head.unjust else flags.backend

  if backend == "all" || backend == "wasm" || backend == "lsp" then
    build-wasm(backend)

  if backend == "all" || backend == "precompile" then
    do-precompile()

  if backend == "all" || backend == "deploy" || backend == "web" then
    deploy()

  if backend == "web" then
    start-web()

  if backend == "all" then
    info("")
    info("=== Setup complete! ===")
    info("")
    info("Start the dev server:")
    info("  cd web/playground && npm install && npx vite --host")
