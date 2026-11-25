//----------------------------------------------------------------------------
// Copyright 2024, Microsoft Research, Daan Leijen. Tim Whiting.
//
// This is free software; you can redistribute it and/or modify it under the
// terms of the Apache License, Version 2.0. A copy of the License can be
// found in the LICENSE file at the root of this distribution.
//----------------------------------------------------------------------------
// Updated as of 8/3/26: Commit eb98b61d
module compiler/common/syntax

pub type js-target
  JsDefault
  JsNode
  JsWeb

pub fun jstarget/(==)(j1: js-target, j2: js-target): bool
  match (j1, j2)
    (JsDefault, JsDefault) -> True
    (JsNode, JsNode) -> True
    (JsWeb, JsWeb) -> True
    _ -> False

pub type c-target
  CDefault
  LibC
  Wasm
  WasmJs
  WasmWeb

pub fun ctarget/(==)(c1: c-target, c2: c-target): bool
  match (c1, c2)
    (CDefault, CDefault) -> True
    (LibC, LibC) -> True
    (Wasm, Wasm) -> True
    (WasmJs, WasmJs) -> True
    (WasmWeb, WasmWeb) -> True
    _ -> False

// backend:  c js cs
// host:     c : libc wasm wasmweb  (wasm==wasi wasmweb==emscripten)
//           js: node web
//           cs: dotnet
// platform: 32 64 64c js cs
// arch:     x86 x64 arm32 arm64 riscv  -<variant>
// os:       windows linux macos unix   -<variant>
//
// target option:
//   c    c64 c32 c64c
//   js   jsnode jsweb
//   wasm wasm32 wasm64 wasmjs wasmweb
//   cs
pub value type target
  DefaultTarget
  CS
  JS(t: js-target)
  C(t: c-target)
  Unsupported

pub fun target/(==)(t1: target, t2: target): bool
  match (t1, t2)
    (CS, CS) -> True
    (JS(j1), JS(j2)) -> j1 == j2
    (C(c1), C(c2)) -> c1 == c2
    (DefaultTarget, DefaultTarget) -> True
    (Unsupported, Unsupported) -> True
    _ -> False

// PORT-SPECIFIC: an `extern import` entry the parser adds beside inlined content
// (from `file`, `header-file` or `header-end-file`), naming the file it came from.
// The module's output depends on that file, so `code-gen` lists it beside the
// interface for the rebuild check; it is never printed into an interface.
pub val file-dependency-key = "file-dependency"

pub fun is-target-wasm(t: target): bool
  match t
    C(Wasm) -> True
    C(WasmJs) -> True
    C(WasmWeb) -> True
    _ -> False

pub fun unsupported-external(fname: string): string
  trace("warning: unsupported external: " ++ fname)
  "kk_unsupported_external(" ++ fname.show ++ ")"

pub fun target/show(t: target): string
  match t
    C(CDefault) -> "c"
    C(LibC) -> "libc"
    C(Wasm) -> "wasm"
    C(WasmJs) -> "wasmjs"
    C(WasmWeb) -> "wasmweb"
    JS(JsNode) -> "jsnode"
    JS(JsWeb) -> "jsweb"
    JS(JsDefault) -> "js"
    CS -> "cs"
    DefaultTarget -> "default"
    Unsupported -> "unsupported"

pub struct platform
  ptrSize: int // sizeof(intptr_t)
  sizeSize: int // sizeof(size_t)
  fieldSize: int // sizeof(kk_field_t) usually intptr_t but may be smaller for compression
  headerSize: int // used for correct alignment calculation

pub val platform32 = Platform(4, 4, 4, 8)
pub val platform64 = Platform(8, 8, 8, 8)
pub val platform64c = Platform(8, 8, 4, 8) // Compressed fields
pub val platformJS = Platform(8, 4, 8, 0)
pub val platformCS = Platform(8, 4, 8, 0)
pub val platformNone = Platform(0, 0, 0, 0)

pub fun platform/(==)(p1: platform, p2: platform): bool
  p1.ptrSize == p2.ptrSize && p1.sizeSize == p2.sizeSize && p1.fieldSize == p2.fieldSize && p1.headerSize == p2.headerSize

pub fun platform/cmp(p1: platform, p2: platform): order
  val c = p1.ptrSize.cmp(p2.ptrSize)
  if !c.is-eq then return c
  val c2 = p1.sizeSize.cmp(p2.sizeSize)
  if !c2.is-eq then return c2
  val c3 = p1.fieldSize.cmp(p2.fieldSize)
  if !c3.is-eq then return c3
  p1.headerSize.cmp(p2.headerSize)

pub fun has-compressed-fields(p: platform): bool
  p.ptrSize != p.fieldSize

pub fun platform/show(p: platform): string
  if p == platform32 then "p32"
  elif p == platform64 then "p64"
  elif p == platform64c then "p64c"
  elif p == platformNone then "none"
  else p.show-detail

pub fun platform/show-detail(p: platform): string
  "p(sizeof(void*)=" ++ p.ptrSize.show ++ ", sizeof(size_t)=" ++ p.sizeSize.show ++ ", sizeof(kk_box_t)=" ++ p.fieldSize.show ++ ", sizeof(kk_header_t)=" ++ p.headerSize.show ++ ")"

pub fun aligned/sum(start: int, xs: list<int>): int
  xs.foldl(start, aligned/add)

fun aligned/add(x: int, y: int): int
  align-up(x, y) + y

fun align-up(x: int, y: int): int
  if y <= 0 then x
  else ((x + y - 1) / y) * y

pub type build-type
  DebugFull
  Debug
  RelWithDebInfo
  Release

pub fun build-type/(==)(b1: build-type, b2: build-type): bool
  match (b1, b2)
    (DebugFull, DebugFull) -> True
    (Debug, Debug) -> True
    (RelWithDebInfo, RelWithDebInfo) -> True
    (Release, Release) -> True
    _ -> False

// ordered debugfull < debug < relwithdebinfo < release (used by e.g.
// `buildType flags <= Debug`)
pub fun build-type/rank(b: build-type): int
  match b
    DebugFull -> 0
    Debug -> 1
    RelWithDebInfo -> 2
    Release -> 3

pub fun build-type/(<=)(b1: build-type, b2: build-type): bool
  b1.rank <= b2.rank

pub fun build-type/show(b: build-type): string
  match b
    DebugFull -> "debugfull"
    Debug -> "debug"
    RelWithDebInfo -> "drelease"
    Release -> "release"

// A `:target` refined with os, architecture, platform sizes, and arbitrary build defines
pub type target-platform
  TargetPlatform(
    tpl-target: target,
    tpl-os: string,
    tpl-arch: string,
    tpl-platform: platform,
    tpl-build-defs: list<string> // arbitrary build defines
  )

pub fun targetplatform/(==)(t1: target-platform, t2: target-platform): bool
  t1.tpl-target == t2.tpl-target && t1.tpl-os == t2.tpl-os && t1.tpl-arch == t2.tpl-arch &&
  t1.tpl-platform == t2.tpl-platform && t1.tpl-build-defs == t2.tpl-build-defs

fun target/show-backend(t: target): string
  match t
    C -> "c"
    JS -> "js"
    CS -> "cs"
    _ -> "default"

fun target/host-attr(t: target): list<string>
  match t
    C(LibC) -> ["host=libc"]
    C(Wasm) -> ["host=wasm"]
    C(WasmJs) -> ["host=wasmjs"]
    C(WasmWeb) -> ["host=wasmweb"]
    JS(JsNode) -> ["host=jsnode"]
    JS(JsWeb) -> ["host=jsweb"]
    _ -> []

pub fun targetplatform/show(tp: target-platform): string
  val attrs = [
    tp.tpl-target.host-attr,
    if tp.tpl-os == "" then [] else ["os=" ++ tp.tpl-os],
    if tp.tpl-arch == "" then [] else ["arch=" ++ tp.tpl-arch],
    if tp.tpl-platform == platformNone then [] else ["platform=" ++ tp.tpl-platform.show],
    if tp.tpl-build-defs.is-empty then [] else ["buildcfg=" ++ tp.tpl-build-defs.join("|")]
  ].concat
  tp.tpl-target.show-backend ++ (if attrs.is-empty then "" else "[" ++ attrs.join(",") ++ "]")

pub val target-platform-c64 = TargetPlatform(C(LibC), "", "", platform64, [])

pub val target-platform-default = target-platform-from-target(DefaultTarget)

pub fun target-platform-from-target(t: target): target-platform
  TargetPlatform(t, "", "", platformNone, [])

pub fun target-platform-is-default(tp: target-platform): bool
  match tp
    TargetPlatform(DefaultTarget, "", "", Platform(0, 0, 0, 0), Nil) -> True
    _ -> False

pub val host-ids: list<(string, target)> = [
  ("libc", C(LibC)),
  ("wasm", C(Wasm)),
  ("wasmjs", C(WasmJs)),
  ("wasmweb", C(WasmWeb)),
  ("jsnode", JS(JsNode)),
  ("jsweb", JS(JsWeb))
]

pub fun target-from-host(s: string): maybe<target>
  host-ids.lookup(fn(k) k == s)

pub val backend-ids: list<(string, target)> = [
  ("c", C(CDefault)),
  ("js", JS(JsDefault)),
  ("cs", CS),
  ("default", DefaultTarget)
]

pub fun target-from-backend(s: string): maybe<target>
  backend-ids.lookup(fn(k) k == s)

pub val platform-ids: list<(string, platform)> = [
  ("32", platform32), ("p32", platform32),
  ("64", platform64), ("p64", platform64),
  ("64c", platform64c), ("p64c", platform64c),
  ("js", platformJS), ("pjs", platformJS),
  ("cs", platformCS), ("pcs", platformCS),
  ("none", platformNone)
]

pub fun platform-from-string(s: string): maybe<platform>
  platform-ids.lookup(fn(k) k == s)

pub val target-platform-ids: list<(string, target-platform)> = [
  ("c", target-platform-default(tpl-target = C(LibC), tpl-platform = platform64)),
  ("c64", target-platform-default(tpl-target = C(LibC), tpl-platform = platform64)),
  ("c32", target-platform-default(tpl-target = C(LibC), tpl-platform = platform32)),
  ("c64c", target-platform-default(tpl-target = C(LibC), tpl-platform = platform64c)),
  ("js", target-platform-default(tpl-target = JS(JsNode), tpl-platform = platformJS)),
  ("jsnode", target-platform-default(tpl-target = JS(JsNode), tpl-platform = platformJS)),
  ("jsweb", target-platform-default(tpl-target = JS(JsWeb), tpl-platform = platformJS)),
  ("wasm", target-platform-default(tpl-target = C(Wasm), tpl-platform = platform32)),
  ("wasm32", target-platform-default(tpl-target = C(Wasm), tpl-platform = platform32)),
  ("wasm64", target-platform-default(tpl-target = C(Wasm), tpl-platform = platform64)),
  ("wasmjs", target-platform-default(tpl-target = C(WasmJs), tpl-platform = platform32)),
  ("wasmweb", target-platform-default(tpl-target = C(WasmWeb), tpl-platform = platform32)),
  ("cs", target-platform-default(tpl-target = CS, tpl-platform = platformCS))
]

pub fun target-platform-from-string(s: string): maybe<target-platform>
  target-platform-ids.lookup(fn(k) k == s)

pub fun match-target-platform(tp1: target-platform, tp2: target-platform): bool
  match-target(tp1.tpl-target, tp2.tpl-target) && match-str(tp1.tpl-os, tp2.tpl-os) &&
  match-str(tp1.tpl-arch, tp2.tpl-arch) && match-platform(tp1.tpl-platform, tp2.tpl-platform) &&
  match-build-defs(tp1.tpl-build-defs, tp2.tpl-build-defs)

pub fun match-os(s1: string, s2: string): bool
  match-str(s1, s2)

pub fun match-arch(s1: string, s2: string): bool
  match-str(s1, s2)

fun match-str(s1: string, s2: string): bool
  if s1 == "" then True
  else s1.split("-").list/is-prefix-of(s2.split("-"))

pub fun match-platform(p1: platform, p2: platform): bool
  match-int(p1.ptrSize, p2.ptrSize) && match-int(p1.sizeSize, p2.sizeSize) &&
  match-int(p1.fieldSize, p2.fieldSize) && match-int(p1.headerSize, p2.headerSize)

fun match-int(i1: int, i2: int): bool
  if i1 == 0 then True else i1 == i2

pub fun match-target(t1: target, t2: target): bool
  match (t1, t2)
    (DefaultTarget, _) -> True
    (C(CDefault), C(_)) -> True
    (JS(JsDefault), JS(_)) -> True
    _ -> t1 == t2

pub fun match-build-defs(defs1: list<string>, defs2: list<string>): bool
  defs1.all(fn(d) defs2.any(fn(d2) d2 == d))

// Visibility
pub type visibility
  Public
  Private

pub fun visibility/(==)(v1: visibility, v2: visibility): bool
  match (v1, v2)
    (Public, Public) -> True
    (Private, Private) -> True
    _ -> False

pub fun visibility/show(v: visibility): string
  match v
    Public -> "Public"
    Private -> "Private"

pub type handler-sort
  HandlerNormal
  HandlerInstance

pub fun handler-sort/show(h: handler-sort): string
  match h
    HandlerNormal -> "normal"
    HandlerInstance -> "named"

pub type operation-sort
  OpVal
  OpFun
  OpExcept
  OpControlRaw
  OpControl
  OpControlErr

// Automatically generated.
// Comparison of the `operation-sort` type.
pub fun operation-sort/cmp(this : operation-sort, other : operation-sort) : e order
  match (this, other)
    (OpVal, OpVal) -> Eq
    (OpVal, _) -> Lt
    (_, OpVal) -> Gt
    (OpFun, OpFun) -> Eq
    (OpFun, _) -> Lt
    (_, OpFun) -> Gt
    (OpExcept, OpExcept) -> Eq
    (OpExcept, _) -> Lt
    (_, OpExcept) -> Gt
    (OpControlRaw, OpControlRaw) -> Eq
    (OpControlRaw, _) -> Lt
    (_, OpControlRaw) -> Gt
    (OpControl, OpControl) -> Eq
    (OpControl, _) -> Lt
    (_, OpControl) -> Gt
    (OpControlErr, OpControlErr) -> Eq

pub fun operation-sort/(==)(o1: operation-sort, o2: operation-sort): bool
  match (o1, o2)
    (OpVal, OpVal) -> True
    (OpFun, OpFun) -> True
    (OpExcept, OpExcept) -> True
    (OpControl, OpControl) -> True
    (OpControlRaw, OpControlRaw) -> True
    (OpControlErr, OpControlErr) -> True
    _ -> False

pub fun op-sort/show(o: operation-sort): string
  match o
    OpVal -> "val"
    OpFun -> "fun"
    OpExcept -> "final ctl"
    OpControl -> "ctl"
    OpControlRaw -> "rawctl"
    OpControlErr -> ""

pub fun op-sort/string(o: operation-sort): string
  match o
    OpVal -> "val"
    OpFun -> "fun"
    OpExcept -> "brk"
    OpControl -> "ctl"
    OpControlRaw -> "rawctl"
    OpControlErr -> ""

pub fun read-op-sort(s: string): maybe<operation-sort>
  match s
    "val" -> Just(OpVal)
    "fun" -> Just(OpFun)
    "brk" -> Just(OpExcept)
    "ctl" -> Just(OpControl)
    "rawctl" -> Just(OpControlRaw)
    // legacy
    "except" -> Just(OpExcept)
    "control" -> Just(OpControl)
    "rcontrol" -> Just(OpControlRaw)
    _ -> Nothing

pub value type data-effect
  DataNoEffect
  DataEffect(is-named: bool, is-linear: bool)

pub fun dataeffect/show(k: data-effect): string
  match k
    DataNoEffect -> "noeffect"
    DataEffect(n, l) -> sep-by-space([if n then "named" else "", if l then "linear" else ""])

// DataKind
pub type data-kind
  Inductive
  CoInductive
  Retractive

pub fun datakind/show(k: data-kind): string
  match k
    Inductive -> "type"
    CoInductive -> "co type"
    Retractive -> "div type"

pub type data-def
  DataDefValue(repr: value-repr) // value type
  DataDefNormal // reference type
  DataDefLazy(fipT: fip-t) // Lazy reference type
  DataDefOpen(isExtend: bool)
  DataDefAuto(declaredAsStruct: bool) // value or reference type; determined by kind inference (in kind/repr.kk:create-data-def)

pub fun datadef/(==)(d1: data-def, d2: data-def): bool
  match (d1, d2)
    (DataDefValue(r1), DataDefValue(r2)) -> r1 == r2
    (DataDefNormal, DataDefNormal) -> True
    (DataDefLazy(f1), DataDefLazy(f2)) -> f1 == f2
    (DataDefOpen(e1), DataDefOpen(e2)) -> e1 == e2
    (DataDefAuto(s1), DataDefAuto(s2)) -> s1 == s2
    _ -> False

pub fun datadef/show(d: data-def): string
  match d
    DataDefValue(r) -> "value" ++ r.show
    DataDefNormal -> "reference"
    DataDefLazy(f) -> sep-by-space(["lazy", f.show])
    DataDefOpen(isExtend) -> if isExtend then "extend" else "open"
    DataDefAuto(isStruct) -> "auto" ++ (if isStruct then " struct" else "")

pub fun datadef/is-extend(d: data-def): bool
  match d
    DataDefOpen(isExtend) -> isExtend
    _ -> False
  
pub fun datadef/is-open(d: data-def): bool
  d.is-dataDefOpen

pub fun datadef/is-value(d: data-def): bool
  d.is-dataDefValue

pub fun datadef/is-normal(d: data-def): bool
  d.is-dataDefNormal

pub fun datadef/is-lazy(d: data-def): bool
  d.is-dataDefLazy

pub fun datadef/size(d: data-def, p: platform): int
  match d
    DataDefValue(r) -> size(p, r)
    _ -> p.fieldSize

pub type value-repr
  ValueRepr(
    rawSize: int, // size in bytes
    scan-count: int, // count of scannable fields 
    alignment: int // minimal alignment
  )

pub fun valuerepr/(==)(v1: value-repr, v2: value-repr): bool
  v1.rawSize == v2.rawSize && v1.scan-count == v2.scan-count && v1.alignment == v2.alignment

pub fun valuerepr/show(v: value-repr): string
  "{" ++ v.rawSize.show ++ "," ++ v.scan-count.show ++ "," ++ v.alignment.show ++ "}"

pub fun scan-size(p: platform, v: value-repr): (int, int)
  (p.size(v), v.scan-count)

pub fun valuerepr/size(p: platform, v: value-repr): int
  v.rawSize + (v.scan-count * p.fieldSize)

pub fun is-mixed(v: value-repr): bool
  v.rawSize > 0 && v.scan-count > 0

pub fun is-raw(v: value-repr): bool
  v.rawSize > 0 && v.scan-count == 0

pub val valuerepr/zero = ValueRepr(0,0,0)

pub fun value-repr-raw(m: int)
  ValueRepr(m,0,m)

pub fun value-repr-scan(n: int)
  ValueRepr(0,n,0)

pub type def-sort
  DefFun(param: list<param-info>, fipKind: fip-t=NoFip(False))
  DefVal
  DefVar

pub type param-info
  Borrow
  Own

pub fun def-fun-ex(pinfos: list<param-info>, fipKind: fip-t)
  if pinfos.all(_.is-own) then DefFun([], fipKind) else DefFun(pinfos, fipKind)

pub fun def-fun(pinfos: list<param-info>)
  def-fun-ex(pinfos, noFip)

pub fun defsort/show(s: def-sort): string
  match s
    DefFun -> "fun"
    DefVal -> "val"
    DefVar -> "var"

pub fun show-full(s: def-sort): string
  match s
    DefFun(_, f) -> [f.show, "fun"].sep-by-space
    DefVal -> "val"
    DefVar -> "var"

pub fun defsort/is-fun(s: def-sort): bool
  match s
    DefFun(_) -> True
    _ -> False

pub fun param/(==)(p1: param-info, p2: param-info): bool
  match (p1, p2)
    (Borrow, Borrow) -> True
    (Own, Own) -> True
    _ -> False

pub fun param/show(p: param-info): string
  match p
    Borrow -> "Borrow"
    Own -> "Own"

pub type def-inline
  InlineNever
  InlineAlways
  InlineAuto

pub fun definline/show(i: def-inline): string
  match i
    InlineNever -> "noinline"
    InlineAlways -> "inline"
    InlineAuto -> "autoinline"

// Fixities

// Operator fixity
pub type fixity
  FixInfix(prec: int, assoc: assoc) // Precedence and associativity
  FixPrefix
  FixPostfix

// Operator associativity
pub type assoc
  AssocNone
  AssocRight
  AssocLeft

pub fun fixity/(==)(f1: fixity, f2: fixity): bool
  match (f1, f2)
    (FixInfix(p1, a1), FixInfix(p2, a2)) -> p1 == p2 && a1 == a2
    (FixPrefix, FixPrefix) -> True
    (FixPostfix, FixPostfix) -> True
    _ -> False

pub fun fixity/show(f: fixity): string
  match f
    FixInfix(p, a) -> "FixInfix(" ++ p.show ++ ", " ++ a.show ++ ")"
    FixPrefix -> "FixPrefix"
    FixPostfix -> "FixPostfix"

pub fun assoc/(==)(a1: assoc, a2: assoc): bool
  match (a1, a2)
    (AssocNone, AssocNone) -> True
    (AssocRight, AssocRight) -> True
    (AssocLeft, AssocLeft) -> True
    _ -> False

pub fun assoc/show(a: assoc): string
  match a
    AssocNone -> "AssocNone"
    AssocRight -> "AssocRight"
    AssocLeft -> "AssocLeft"

// Fip
pub type fip-t
  Fip(alloc: fip-alloc)
  Fbip(alloc: fip-alloc, tail: bool)
  NoFip(tail: bool) // Ordering seems wrong as `NoFip False` should be top but now NoFip True is the top

pub fun fipt/show(f: fip-t): string
  fun show-n(fip-alloc: fip-alloc): string
    match fip-alloc
      AllocAtMost(0) -> ""
      AllocAtMost(n) -> "(" ++ n.show ++ ")"
      AllocFinitely -> "(n)"
      AllocUnlimited -> ""
  fun show-tail(b: bool): string
    if b then "tail" else ""
  match f
    Fip(a) -> "fip" ++ show-n(a)
    Fbip(a, t) -> [show-tail(t), "fbip" ++ show-n(a)].sep-by-space
    NoFip(t) -> show-tail(t)

pub fun fipt/(==)(f1: fip-t, f2: fip-t): bool
  match (f1, f2) 
    (Fip(a1), Fip(a2)) -> a1 == a2
    (Fbip(a1, t1), Fbip(a2, t2)) -> a1 == a2 && t1 == t2
    (NoFip(t1), NoFip(t2)) -> t1 == t2
    _ -> False

// Note that "fbip" and "fip(1)" are incomparable so this cannot have an `cmp` function
pub fun subsumes(f1: fip-t, f2: fip-t): bool
  match (f1, f2)
    (NoFip, Fip) -> True
    (NoFip(t1), f2) -> !t1 || f2.is-tail
    (Fbip(a1, t1), Fbip(a2, t2)) -> a1 >= a2 && (!t1 || t2)
    (Fbip(a1), Fip(a2)) -> a1 >= a2
    (Fip(a1), Fip(a2)) -> a1 >= a2
    _ -> False

pub value type fip-alloc
  AllocAtMost(i: int)
  AllocFinitely
  AllocUnlimited

pub fun fipa/cmp(f1: fip-alloc, f2: fip-alloc): order
  match (f1, f2)
    (AllocAtMost(n1), AllocAtMost(n2)) -> n1.cmp(n2)
    (_, AllocAtMost) -> Gt
    (AllocAtMost, AllocFinitely) -> Lt
    (AllocFinitely, AllocFinitely) -> Eq
    (AllocUnlimited, AllocFinitely) -> Gt
    (AllocUnlimited, AllocUnlimited) -> Eq
    (_, AllocUnlimited) -> Lt
    _ -> Lt

pub fun fipa/(==)(f1: fip-alloc, f2: fip-alloc): bool
  match (f1, f2)
    (AllocAtMost(n1), AllocAtMost(n2)) -> n1 == n2
    (AllocFinitely, AllocFinitely) -> True
    (AllocUnlimited, AllocUnlimited) -> True
    _ -> False

pub fun combine(f1: fip-alloc, f2: fip-alloc)
  match (f1, f2)
    (AllocAtMost(n), AllocAtMost(m)) -> AllocAtMost(n + m)
    _ -> AllocFinitely

pub val fipa/zero = AllocAtMost(0)

pub fun fipalloc/show(a: fip-alloc): string
  match a
    AllocAtMost(0) -> "AllocAtMost(0)"
    AllocAtMost(n) -> "AllocAtMost(" ++ n.show ++ ")"
    AllocFinitely -> "AllocFinitely"
    AllocUnlimited -> "AllocUnlimited"

pub val fipBot = Fip(AllocAtMost(0))
pub val fipTop = NoFip(False)
pub val fipNoAlloc = fipBot
pub val noFip = fipTop

pub fun is-top(f: fip-t): bool
  match f
    NoFip(False) -> True
    _ -> False

pub fun fipt/is-tail(f: fip-t): bool
  match f
    Fip(_) -> True
    Fbip(_, t) -> t
    NoFip(t) -> t

pub fun fipt/alloc(f: fip-t): fip-alloc
  match f
    Fip(a) -> a
    Fbip(a) -> a
    NoFip() -> AllocUnlimited

pub fun show-fip(f: fip-t)
  fun show-tail(b: bool): string
    if b then "tail" else ""
  fun show-n(n: fip-alloc): string
    match n
      AllocAtMost(0) -> ""
      AllocAtMost(n) -> "(" ++ n.show ++ ")"
      AllocFinitely -> "(n)"
      AllocUnlimited -> "" 
  match f
    Fip(n) -> "fip" ++ show-n(n)
    Fbip(n, t) -> [t.show-tail, "fbip" ++ show-n(n)].sep-by-space
    NoFip(t) -> t.show-tail

pub fun sep-by-space(xs: list<string>)
  xs.filter(_ != "").join(" ")

pub fun fipt/max(fip1: fip-t, fip2: fip-t)
  match (fip1, fip2)
    (NoFip(isTail1), NoFip(isTail2)) -> NoFip(isTail1 && isTail2)
    (NoFip(t1), _) -> NoFip(t1 && fip2.is-tail)
    (_, NoFip(t2)) -> NoFip(t2 && fip1.is-tail)
    (Fbip(a1, t1), Fbip(a2, t2)) -> Fbip(max(a1, a2), t1 && t2)
    (Fbip(a1, t1), Fip(a2)) -> Fbip(max(a1, a2), t1)
    (Fip(a1), Fbip(a2, t2)) -> Fbip(max(a1, a2), t2)
    (Fip(a1), Fip(a2)) -> Fip(max(a1, a2))

pub fun fipa/max(a1: fip-alloc, a2: fip-alloc)
  match (a1, a2)
    (AllocUnlimited, _) -> AllocUnlimited
    (_, AllocUnlimited) -> AllocUnlimited
    (AllocFinitely, _) -> AllocFinitely
    (_, AllocFinitely) -> AllocFinitely
    (AllocAtMost(n1), AllocAtMost(n2)) -> AllocAtMost(max(n1, n2))

pub fun member-doc(doc: string, header: string, members: list<string>): string
  if members.is-empty then doc 
  else 
    val mdoc = "// " ++ header ++ ":\n// ```koka\n" ++ members.map(fn(m) "// " ++ m ++ "\n").join ++ "// ```\n"
    if doc == "" then mdoc else doc ++ "\n// * * *\n" ++ mdoc
