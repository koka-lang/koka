//---------------------------------------------------------------------------
// Copyright 2024-2026, Microsoft Research, Daan Leijen. Tim Whiting.
//
// This is free software; you can redistribute it and/or modify it under the
// terms of the Apache License, Version 2.0. A copy of the License can be
// found in the LICENSE file at the root of this distribution.
//---------------------------------------------------------------------------
// Ported as of 2026-01-17; approx upstream baseline commit edc29e7 of Platform/cpp/Platform/Config.hs (backfilled 2026-07-23)
module compiler/platform/config

import std/os/env

// Upstream takes these from defines set by the build (`KOKA_VERSION` etc.); here they are a handler, so a tool built on the compiler can supply its own.
// `version` names the installed library directory (`share/koka/v<version>`) and the build directory, so it must match the `lib/` the compiler is used with.
pub effect val comp-flags: compiler-flags

pub fun @default-comp-flags(f)
  with handler val comp-flags = CompilerFlags(
    programName = "koka",
    version = "3.3.0",
    compiler = "koka",
    buildVariant = "release"
  )
  f()

pub type compiler-flags
  CompilerFlags(
    programName: string,
    version: string,
    compiler: string,
    buildVariant: string
  )

pub type file-config
  FileConfig(
    exeExt: string,
    dllExt: string,
    objExt: string,
    libExt: string,
    libPrefix: string
  )

// File extensions of the host. Upstream fixes these at build time; here they follow the running OS, with every non-Windows, non-macOS host treated as unix.
pub fun os-config(): ndet file-config
  match get-os-name()
    "windows" -> FileConfig(".exe", ".dll", ".obj", ".lib", "")
    "macos"   -> FileConfig("", ".dylib", ".o", ".a", "lib")
    _         -> FileConfig("", ".so", ".o", ".a", "lib")

pub val source-ext = ".kk"

// Is this a file separator? Both are, regardless of host OS.
pub fun is-path-sep(c: char): bool
  c == '/' || c == '\\'
