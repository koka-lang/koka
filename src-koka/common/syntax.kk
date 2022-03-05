type jsTarget
  JsDefault
  JsNode
  JsWeb

type cTarget
  CDefault
  LibC
  Wasm
  WasmJs
  WasmWeb

type target
  CS
  JS(t: jsTarget)
  C(t: cTarget)
  Default

fun (==)(c1: cTarget, c2: cTarget): bool
  match (c1, c2)
    (CDefault, CDefault) -> True
    (LibC, LibC) -> True
    (Wasm, Wasm) -> True
    (WasmJs, WasmJs) -> True
    (WasmWeb, WasmWeb) -> True
    _ -> False

fun (==)(j1: jsTarget, j2: jsTarget): bool
  match (j1, j2)
    (JsDefault, JsDefault) -> True
    (JsNode, JsNode) -> True
    (JsWeb, JsWeb) -> True
    _ -> False

fun (==)(t1: target, t2: target): bool
  match (t1, t2)
    (CS, CS) -> True
    (JS(j1), JS(j2)) -> j1 == j2
    (C(c1), C(c2)) -> c1 == c2
    (Default, Default) -> True
    _ -> False

type buildType
  DebugFull
  Debug
  RelWithDebInfo
  Release

fun show(b: buildType): string
  match b
    DebugFull -> "DebugFull"
    Debug -> "Debug"
    RelWithDebInfo -> "RelWithDebInfo"
    Release -> "Release"


// The flavour of a type variable. Types in a "Type.Assumption" (Gamma) and inferred types in "Core.Core"
// are always of the 'Bound' flavour. 'Meta' and 'Skolem' type variables only ever occur during type inference.
// Kind and Type variables come in three flavours: 'Unifiable'
// variables can be unified, 'Skolem's are non-unifiable (fresh)
// variables, and 'Bound' variables are bound by a quantifier.
type flavour
  Bound
  Skolem
  Meta // used for pretty printing

fun (==)(f1: flavour, f2: flavour): bool
  match (f1, f2)
    (Meta, Meta) -> True
    (Skolem, Skolem) -> True
    (Bound, Bound) -> True
    _ -> False

type visibility
  Public
  Private

type handlerSort
  HandlerNormal
  HandlerInstance

pub fun show(h: handlerSort): string
  match h
    HandlerNormal -> "HandlerNormal"
    HandlerInstance -> "HandlerInstance"

pub fun isHandlerInstance(h: handlerSort): bool
  match h
    HandlerInstance -> True
    HandlerNormal -> False

pub fun isHandlerNormal(h: handlerSort): bool
  match h
    HandlerInstance -> False
    HandlerNormal -> True

type operationSort
  OpVal
  OpFun
  OpExcept
  OpControlRaw
  OpControl

type dataKind
  Inductive
  CoInductive
  Retractive

type dataDef
  DataDefValue(rawFields: int, scanFields: int) // size in bytes and count of scannable fields
  DataDefNormal
  DataDefAuto
  DataDefRec
  DataDefOpen

fun isRec(d: dataDef): bool
  match d
    DataDefRec -> True
    _ -> False
  
fun isOpen(d: dataDef): bool
  match d
    DataDefOpen -> True
    _ -> False
  
fun isValue(d: dataDef): bool
  match d
    DataDefValue(_, _) -> True
    _ -> False

type defSort
  DefFun(param: list<paramInfo>)
  DefVal
  DefVar

pub fun isFun(s: defSort): bool
  match s
    DefFun(_) -> True
    _ -> False

type paramInfo
  Borrow
  Own

type defInline
  InlineNever
  InlineAlways
  InlineAuto

type fixity
  FixInfix(prec: int, assoc: assoc)
  FixPrefix
  FixPostfix

type assoc
  AssocNone
  AssocRight
  AssocLeft