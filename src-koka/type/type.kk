module type/type

import common/syntax
import common/name
import common/name-prim
import common/id
import common/range
import common/failure
import kind/kind

pub type ktype
  TForAll(vars: list<typeVar>, pred: list<pred>, r: ktype) // rho
  TFun(args: list<(name, ktype)>, e: ktype, t: ktype) // keffect
  TCon(constant: typeCon)
  TVar(variable: typeVar)
  TApp(t: ktype, args: list<ktype>)
  TSyn(syn: typeSyn, args: list<ktype>, t: ktype)

type pred
  PredSub(a: ktype, b: ktype)
  PredIFace(n: name, tps: list<ktype>)

// Various synonyms of types
// pub alias scheme = ktype
// pub alias sigma = ktype // polymorphic type
// pub alias tau = ktype // monomorphic type
// pub alias rho = ktype // unqualified type
// pub alias keffect = tau

// An inference type can contain type variables of flavour 'Meta' or 'Skolem'
// pub alias inferType = ktype;

// Variables in a type which have an identifier and kind
// One can ask for the free type variables in a type, and substitute them with 'Tau' types.
pub struct typeVar
  id: id
  kind: kind
  flavour: flavour;

// Type constants have a name and a kind
pub struct typeCon
  name: name
  kind: kind;

// A type synonym is quantified by type parameters
pub struct synInfo
  name: name
  kind: kind
  params: list<typeVar> // type parameters
  t: ktype // result type
  rank: synonymRank
  range: range
  vis: visibility
  doc: string;

// Type synonyms have an identifier, kind, and rank (= partial ordering among type synonyms)
pub struct typeSyn
  name: name
  kind: kind
  rank: synonymRank
  info: maybe<synInfo>

// The rank of a type synonym gives a relative ordering among them. This is used
// during unification to increase the chance of matching up type synonyms.
pub alias synonymRank = int


pub fun maxSynonymRank(t: ktype): synonymRank
  fun maxSynonymRanks(tps: list<ktype>): synonymRank
    tps.foldr(0, fn(r, t1) max(r, maxSynonymRank(t1.unsafe-decreasing)))
  match t
    TForAll(_, _, r) -> maxSynonymRank(r.unsafe-decreasing)
    TFun(args, eff, ty) -> maxSynonymRanks(Cons(ty, Cons(eff, args.map(fn (f) -> f.snd))).unsafe-decreasing)
    TCon(_) -> 0
    TVar(_) -> 0
    TApp(ty, tps) -> maxSynonymRanks(Cons(ty, tps).unsafe-decreasing)
    TSyn(syn, args, ty) -> max(syn.rank, maxSynonymRanks(Cons(ty, args).unsafe-decreasing))

// Information about types
pub struct dataInfo
  sort: dataKind
  name: name
  kind: kind
  params: list<typeVar>
  constrs: list<conInfo>
  range: range
  def: dataDef
  vis: visibility
  doc: string

pub fun isRec(d: dataInfo): bool
  d.def.isRec

pub fun isOpen(d: dataInfo): bool
  d.def.isOpen

pub fun isLiteral(d: dataInfo): bool
  val n = d.name
  n == nameTpInt || n == nameTpChar || n == nameTpString || n == nameTpFloat

// Constructor information: constructor name, name of the newtype, field types, and the full type of the constructor
pub struct conInfo
  name: name
  typeName: name
  // sort: name
  foralls: list<typeVar> // quantifiers
  kexists: list<typeVar> // existentials
  params: list<(name, ktype)> // field types
  t: ktype // scheme
  typeSort: dataKind // inductive, coinductive, retractive
  range: range
  paramRanges: list<range>
  paramVis: list<visibility>
  singleton: bool // only constructor of this type
  vis: visibility
  doc: string;

fun show(c: conInfo): string
  c.name.show


fun isBound(t: typeVar): bool
  t.flavour == Bound

// unifiable
fun isMeta(t: typeVar): bool
  t.flavour == Meta

// not unifiable
fun isSkolem(t: typeVar): bool
  t.flavour == Skolem

fun predType(p: pred): compilerEff ktype
  match p
    PredSub(t1, t2) -> typeFun([("sub".newName, t1)], typeTotal, t2)
    PredIFace(_, _) -> todo("type.kk: predType")



// TODO:

fun typeFun(args: list<(name, ktype)>, eff: ktype, res: ktype): ktype
  TFun(args, eff, res)


val typeTotal = TCon(tconTotal)
val tconTotal = TypeCon(nameEffectEmpty, kindEffect)