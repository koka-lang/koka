/*---------------------------------------------------------------------------
  Copyright 2012-2021, Microsoft Research, Daan Leijen.

  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the LICENSE file at the root of this distribution.
---------------------------------------------------------------------------*/

/*
Access to the program environment and commmand line arguments.

Print the environment: `get-env().map(fn(p) { p.fst + "=" + p.snd }).join("\n").print`
*/
module std/os/env

import std/os/path

pub alias env = list<(string,string)>

val environ : delayed<<ndet>,env> = delay{ os-get-env().list.to-tuples }

fun to-tuples( xs : list<string> ) : env
  match(xs)
    Cons(name,Cons(value,xx)) -> Cons((name,value), xx.to-tuples)
    Cons(name,Nil)            -> [(name,"")]
    Nil                       -> []

extern os-get-env() : ndet vector<string>    // vector as [name1,value1,...,nameN,valueN]
  c "kk_os_get_env"
  js inline "(typeof process !== 'undefined' ? (function(){ var env = []; Object.keys(process.env).forEach(function(name){ env.push(name); env.push(process.env[name]); }); return env; })() : [])"

// Get the environment variables for this program
pub fun get-env() : ndet env
  environ.force

// Returns the value of an environment variable `name` (or `Nothing` if not present)
pub fun get-env( name : string ) : ndet maybe<string>
  get-env().lookup(fn(nm){ nm == name })

val argv : delayed<<ndet>,list<string>> = 
  delay{ os-get-argv().list }

extern os-get-argv() : ndet vector<string>
  c  "kk_os_get_argv"
  cs "System.Environment.GetCommandLineArgs"
  js inline "(typeof process !== 'undefined' ? process.argv : [])"


// The unprocessed command line that was used to start this program.
// On ''Node'' the first arguments will often be of the form `["node","interactive.js",...]`.
pub fun get-argv() : ndet list<string>
  argv.force

// Return the arguments that were passed to program itself.
// Strips off the initial program from the unprocessed command line.
// i.e. If a program started as:
// ````
// > node myprogram.js --flag bla
// ````
// The `arguments` list will be `["--flag","bla"]`
pub fun get-args() : ndet list<string>
  val is-node = (host() == "node")
  match get-argv()
    Cons(x,xx) | is-node && x.path.stemname == "node" -> xx.drop(1)
    xs -> xs.drop(1)

// Return the main OS name: windows, linux, macos, unix, posix, ios, tvos, watchos, unknown.
// Sometimes has a _dash_ subsystem, like: unix-\<freebsd,openbsd,dragonfly,bsd\>, and windows-mingw.
pub extern get-os-name() : ndet string
  c  "kk_os_name"
  js inline "$std_core.host()"

// Return the main processor architecture: x64, x86, arm64, arm, riscv32, riscv64, alpha, ppc64, etc.
pub extern get-cpu-arch() : ndet string
  c "kk_cpu_arch"
  js inline "$std_core.host()"

// Return the available CPU's.
// This is the logical core count including hyper-threaded cores.
pub extern get-cpu-count() : ndet int
  c inline "kk_integer_from_int(kk_cpu_count(kk_context()),kk_context())"
  js inline "1"

// The current compiler version.
pub extern get-compiler-version() : ndet string
  c "kk_compiler_version"
  js inline "\"2\""

// The backend compiler name.
pub extern get-cc-name() : ndet string
  c "kk_cc_name"
  js inline "\"js\""
  cs inline "\"csc\""

// Is the byte-order little-endian?
// If not, it is big-endian; other byte orders are not supported.
pub extern get-cpu-is-little-endian() : ndet bool
  c "kk_cpu_is_little_endian"
  js inline "true"

// Return the processor architecture natural machine word size in bits.
//
// Note: Usually this equals the `get-cpu-object-bits` and `get-cpu-pointer-bits` on modern cpu's 
// but they can differ on segmented architectures.
// For example, on the old x86 FAR-NEAR model, the addresses are 32-bit but the maximum object size is 16-bit.
// Or on the more recent-[x32 ABI](https://en.wikipedia.org/wiki/X32_ABI) 
// the addresses and objects are 32-bits but the architecture has 64-bit registers.
pub extern get-cpu-arch-bits() : ndet int
  c inline "kk_integer_from_size_t(CHAR_BIT*sizeof(kk_intx_t),kk_context())"
  js inline "32"

// Return the processor maximum object size in bits (`8*sizeof(size_t)`). This is usually
// equal to the `get-cpu-arch-bits` but may be different on segmented architectures. 
pub extern get-cpu-size-bits() : ndet int
  c inline "kk_integer_from_size_t(CHAR_BIT*sizeof(size_t),kk_context())"
  js inline "32"

// Return the processor maximum address size in bits (`8*sizeof(vaddr_t)`). This is usually
// equal to the `get-cpu-pointer-bits` but may be different on capability architectures like ARM CHERI.
pub extern get-cpu-address-bits() : ndet int
  c inline "kk_integer_from_int(kk_cpu_address_bits(kk_context()),kk_context())"
  js inline "32"

// Return the processor maximum pointer size in bits (`8*sizeof(void*)`). This is usually
// equal to the `get-cpu-address-bits` but may be larger on capability architectures like ARM CHERI.
pub extern get-cpu-pointer-bits() : ndet int
  c inline "kk_integer_from_size_t(CHAR_BIT*sizeof(void*),kk_context())"
  js inline "32"

// Return the size of boxed values in the heap (`8*sizeof(kk_box_t)`). This is usually
// equal to `8*sizeof(void*)` but can be less if compressed pointers are used.
pub extern get-cpu-boxed-bits() : ndet int
  c inline "kk_integer_from_size_t(CHAR_BIT*sizeof(kk_intb_t),kk_context())"
  js inline "32"  
