import common/syntax
import common/name
import common/range
import std/num/float64
import type/type

struct core
  name: name
  imports: kimports
  fixDefs: fixDefs
  typeDefs: typeDefGroups
  defs: defGroups
  externals: externals
  doc: string;

alias fixDefs = list<fixDef>;
type fixDef
  FixDef(name: name, fixity: fixity)

alias kimports = list<kimport>;
struct kimport
  name: name
  package: string
  visibility: visibility
  doc: string;

alias externals = list<external>;
type external
  External
    name: name
    typeScheme: ktype //scheme
    params: list<paramInfo>
    format: list<(target, string)>
    visibility: visibility
    range: range
    doc: string
  ExternalImport
    imports: list<(target,list<(string, string)>)>
    range: range

pub fun externalImportLookup(t: target, b: buildType, key: string, e: external): maybe<string>
  match e
    ExternalImport(imports, _) ->
      val keys = match imports.find(fn(i) -> i.fst == t)
        Just(kv) -> kv.snd
        Nothing -> Nil
      b.eimportLookup(key, keys)
    _ -> Nothing

pub fun eimportLookup(b: buildType, s: string, ls: list<(string, string)>): maybe<string>
  val l = ls.find(fn(i) -> i.fst == s ++ "-" ++ b.show)
  match l
    Just(i) -> Just(i.snd)
    Nothing -> 
      match ls.find(fn(i) -> i.fst == s)
        Just(i) -> Just(i.snd)
        Nothing -> Nothing

// pub fun lookupTarget(t: target, ims: list<(target, a)>): maybe<a>
//   val tgs = match t
//     C(WasmJs) -> [(t,C(WasmJs)), (Wasm,C(WasmJs)), (CDefault,Default)]
//     C(WasmWeb) -> [(t,C(WasmWeb)), (Wasm,C(WasmWeb)), (CDefault,Default)]
//     C(t1) -> [(t,C(t1)), (CDefault,Default)]
//     JS(j) -> [(t,JS(j)), (JsDefault,Default)]
//     _ -> [(t,Default)]
//   val r = tgs.map(fn(t1) -> ims.find((fn(t2) -> t2 == t1))).filter((fn(i) -> i != Nothing))
//   match r
//     Cons(Just(x, _)) -> x
//     _ -> Nothing


type typeDef
  Synonym(i: synInfo) // name, synonym info, and the visibility
  Data(info: dataInfo, isExtend: bool) //  name, info, visibility, and the visibilities of the constructors, the isExtend is true if this is an extension of the datatype.

alias typeDefGroups = list<typeDefGroup>
alias typeDefGroup = typeDefs
alias typeDefs = list<typeDef>

pub fun name(t: typeDef): name
  match t
    Synonym(info) -> info.name
    Data(info, _) -> info.name

pub fun isExtension(t: typeDef): bool
  match t
    Data(_, True) -> True
    _ -> False

pub fun vis(t: typeDef): visibility
  match t
    Synonym(info) -> info.vis
    Data(info, _) -> info.vis

// TODO: Flatten ?? Is this needed

type dataRepr
  DataEnum // only singletons (as an enumeration)
  DataIso // only one constructor with one field  (isomorpic)
  DataSingleStruct // only one constructor (no tag needed)
  DataStructAsMaybe // one constructor with one field, and one singleton (allows optimized boxed representation that avoids allocation)
  DataStruct // compatible constructors (all raw or regular types) and possibly singletons (need tag)
  // non-value types
  DataSingle(hasSingletons: bool) // only one constructor (no tag needed), hasSingletons true if it is a singleton as well
  DataAsMaybe // one constructor with one (non-recursive) field, and one singleton
  DataAsList // one constructor with fields, and one singleton (don't need a tag, for example can distinguish pointer vs enum)
  DataSingleNormal // one constructor with fields, and multiple singletons (distinguish one pointer vs enums)
  DataNormal(hasSingletons: bool)
  DataOpen

type conRepr
  ConEnum(name: name, repr: dataRepr, tag: int) // part of enumeration (none has fields)
  ConIso(name: name, repr: dataRepr, tag: int) // one constructor with one field
  ConSingleton(name: name, repr: dataRepr, tag: int) // constructor without fields (and not part of an enum)
  ConSingle(name: name, repr: dataRepr, tag: int) // there is only one constructor and it is not iso or singleton (and this is it)
  ConAsJust(name: name, repr: dataRepr, asNothing: name, tag: int) // constructor is the just node of a maybe-like datatype  (only use for DataAsMaybe, not for DataStructAsMaybe)
  ConStruct(name: name, repr: dataRepr, tag: int) // constructor as value type
  ConAsCons(name: name, repr: dataRepr, asNil: name, tag: int) // constructor is the cons node of a list-like datatype  (may have one or more fields)
  ConOpen(name: name, repr: dataRepr) // constructor of open data type
  ConNormal(name: name, repr: dataRepr, tag: int) // a regular constructor

fun isConSingleton(c: conRepr): bool
  match c
    ConSingleton(_, _, _) -> True
    _ -> False

fun isConNormal(c: conRepr): bool
  match c
    ConNormal(_, _) -> True
    _ -> False

fun isConIso(c: conRepr): bool
  match c
    ConIso(_, _, _) -> True
    _ -> False

fun isDataStruct(d: dataRepr): bool
  match d
    DataStruct -> True
    _ -> False

fun isDataMaybe(d: dataRepr): bool
  match d
    DataAsMaybe -> True
    _ -> False
  
fun isDataStructAsMaybe(d: dataRepr): bool
  match d
    DataStructAsMaybe -> True
    _ -> False

fun isConAsJust(c: conRepr): bool
  match c
    ConAsJust(_, _, _, _) -> True
    _ -> False
  
// Value data is not heap allocated and needs no header
fun isValue(d: dataRepr): bool
  match d
    DataEnum -> True
    DataIso -> True
    DataSingleStruct -> True
    DataStructAsMaybe -> True
    DataStruct -> True
    _ -> False

fun isValue(c: conRepr): bool
  c.repr.isValue

fun isValue(d: dataInfo): bool
  d.def.isValue





// Definition Groups

alias defGroups = list<defGroup>

type defGroup
  DefRec(defs: list<def>)
  DefNonRec(def: def)

// A value definition
struct def
  name: name
  t: ktype
  expr: expr
  vis: visibility // private public
  sort: defSort // fun / val / var
  kinline: defInline // auto / always, never
  nameRange: range
  doc: string;

struct inlineDef
  name: name
  expr: expr
  rec: bool
  kind: defInline
  cost: int
  sort: defSort // for borrow information
  paramSpecialize: list<bool>

// Expressions
// Since this is System-F, all binding sites are annotated with their type.
type expr
  // Core lambda calculus
  Lam(names: list<tName>, eff: ktype, e: expr)
  Var(name: tName, info: varInfo) // typed name and possible typeArity/parameter arity tuple for top-level functions
  App(f: expr, args: list<expr>) // always fully applied!
  TypeLam(args: list<typeVar>, e: expr) // Type (universal) abstraction/application
  TypeApp(f: expr, types: list<ktype>)
  // Literals, constants and labels
  Con(name: tName, repr: conRepr) // typed name and its representation
  ELit(lit: lit)
  // Let
  Let(defs: list<defGroup>, e: expr)
  // Case expressions
  Case(exprs: list<expr>, branches: list<branch>);

struct branch
  patterns: list<pattern> // length = length exprs in the match
  guards: list<guard>; // any number (>= 1) of guarded expressions

struct guard
  test: expr // boolean
  expr: expr; // bdoy of the branch

type pattern
  PatCon
    name: tName // names the constructor with full signature.
    pattern: list<pattern> // sub-patterns. fully materialized to match arity.
    pepr: conRepr // representation of ctor in backend.
    typeArgs: list<ktype> // zipped with patConPatterns
    kexists: list<typeVar> // closed under existentials here
    typeRes: ktype // result type
    info: conInfo // other constructor info
    skip: bool // skip testing for this constructor (as it should match already)
  PatVar(name: tName, pattern: pattern) // name / type of variable and named subpattern
  PatLit(lit: lit)
  PatWild

type lit
  LitInt(i: int)
  LitFloat(d: float64)
  LitChar(c: char)
  LitString(s: string)

type varInfo
  InfoNone
  InfoArity(t: int, p: int) // type parameters, parameters
  InfoExternal(b: list<(target, string)>) // inline body
  InfoReuse(p: pattern) 
  InfoConField(cName: tName, fName: tName) // constructor name, field name

struct tName
  getName: name
  tNameType: ktype


