import common/range
import common/name
import common/name-prim
import common/syntax
import std/num/float64

struct program<t, k>
  source: source
  name: name
  nameRange: range
  typeDefs: list<typeDefGroup<t,k>>
  defs: defGroups<t>
  imports: imports
  externals: externals
  fixDefs: fixDefs
  doc: string
  
alias userProgram = program<userType, userKind>;
alias userExpr = expr<userType>;
alias userDef = def<userType>;
alias userDefGroup = defGroup<userType>;
alias userTypeDef = typeDef<userType, userType, userKind>;
alias userTypeDefGroup = typeDefGroup<userType, userKind>;
alias userUserCon = userCon<userType, userType, userKind>;
alias userPattern = pattern<userType>
alias userBranch = branch<userType>
alias userGuard = guard<userType>
alias userHandlerBranch = handlerBranch<userType>
alias userValueBinder = valueBinder<()>
alias userTypeBinder = typeBinder<userKind>

alias externals = list<external>;

type external
  External(name: name, t: userType, params: list<paramInfo>, nameRange: range, range: range, inlineCall: list<(target, externalCall)>, vis: visibility, doc: string)
  ExternalImport(eImport: list<(target,list<(string, string)>)>, range: range)

type externalCall
  ExternalInline(content: string)
  ExternalCall(call: string)

alias fixDefs = list<fixDef>;

struct fixDef
  name: name
  fixity: fixity
  range: range
  vis: visibility

alias imports = list<kimport>;

struct kimport
  name: name
  fullName: name
  range: range
  vis: visibility

alias typeDefGroups<t,k> = list<typeDefGroup<t,k>>

type typeDefGroup<t,k>
  TypeDefRec(defs: list<typeDef<t,t,k>>)
  TypeDefNonRec(def: typeDef<t,t,k>)

type typeDef<t,u,k>
  Synonym(binder: typeBinder<k>, params: list<typeBinder<k>>, synonym: t, range: range, vis: visibility, doc: string)
  DataType(binder: typeBinder<k>, params: list<typeBinder<k>>, constrs: list<userCon<t,u,k>>, range: range, vis: visibility, sort: dataKind, def: dataDef, isExtend: bool, doc: string)

struct typeBinder<k>
  name: name
  kind: k
  nameRange: range
  range: range;

struct userCon<t, u, k>
  name: name
  existentials: list<typeBinder<k>>
  params: list<(visibility, valueBinder<t, maybe<expr<u>>>)>
  result: maybe<t>
  nameRange: range
  range: range
  vis: visibility
  doc: string  

alias defGroups<t> = list<defGroup<t>>;
  
type defGroup<t>
  DefRec(defs: list<def<t>>)
  DefNonRec(def: def<t>)

alias defs<t> = list<def<t>>;

struct valueBinder<t, e>
  name: name
  binderType: t // Always present for constructors
  expr: e // always present for definitions as 'Expr t'
                // Function and constructor parameters use 'Maybe (Expr t)' for default values.
                // Pattern bindings ('PatVar') use unit '()'.
  nameRange: range
  range: range; // full range

struct def<t>
  binder: valueBinder<(), expr<t>>
  range: range
  visibilty: visibility
  sort: defSort
  defInline: defInline
  doc: string;

fun defIsVal(d: def<t>): bool
  !d.sort.isFun

fun guardTrue(): expr<t>
  Var(nameTrue, False, rangeNull)

type expr<t>
  Lam(binder: list<valueBinder<maybe<t>,maybe<expr<t>>>>, e: expr<t>, range: range)
  Let(def: defGroup<t>, e: expr<t>, range: range)
  Bind(def: def<t>, e: expr<t>, range: range)
  App(e: expr<t>, args: list<(maybe<(name, range)>, expr<t>)>, range: range)
  Var(name: name, isOp: bool, range: range)
  Literal(l: lit)
  Ann(e: expr<t>, t: t, range: range)
  Case(e: expr<t>, cases: list<branch<t>>, range: range)
  Parens(e: expr<t>, name: name, range: range)
  Inject(t: t, e: expr<t>, behind: bool, range: range)
  Handler(sort: handlerSort, scope: handlerScope, hOverride: handlerOverride, allowMask: maybe<bool>, hEffect: maybe<t>, localPars: list<valueBinder<maybe<t>, ()>>, initially: maybe<expr<t>>, hReturn: maybe<expr<t>>, finally: maybe<expr<t>>, branches: list<handlerBranch<t>>, declRange: range, range: range)

type handlerOverride
  HandlerOverride
  HandlerNoOverride

type handlerScope
  HandlerScoped
  HandlerNoScope

struct handlerBranch<t>
  branchName: name
  branchPars: list<valueBinder<maybe<t>, ()>>
  branchExpr: expr<t>
  branchSort: operationSort
  branchNameRange: range
  branchPatRange: range
  
struct branch<t>
  branchPattern: pattern<t>
  branchGuard: guard<t>
  
struct guard<t>
  guardTest: expr<t>
  guardExpr: expr<t>

type pattern<t>
  PatWild(range: range)
  PatVar(ref: valueBinder<maybe<t>, pattern<t>>)
  PatAnn(p: pattern<t>, t: t, range: range)
  PatCon(name: name, args: list<(maybe<(name, range)>, pattern<t>)>, nameRange: range, range: range)
  PatParens(p: pattern<t>, range: range)
  PatLit(value: lit)

type lit
  LitInt(value: int, range: range)
  LitFloat(value: float64, range: range)
  LitChar(value: char, range: range)
  LitString(value: string, range: range)

fun show(l: lit): string
  match l
    LitInt(v, r) -> v.show ++ " " ++ r.showFullRange
    LitFloat(v, r) -> v.show ++ " " ++ r.showFullRange
    LitChar(v, r) -> v.show ++ " " ++ r.showFullRange
    LitString(v, r) -> v ++ " " ++ r.showFullRange

// Types and kinds
alias kUserTypes<k> = list<kUserType<k>>

type userQuantifier
  QSome
  QForall
  QExists

// Higher ranked types
type kUserType<k>
  TpQuan(q: userQuantifier, b: typeBinder<k>, kind: kUserType<k>, range: range)
  TpQual(l: list<kUserType<k>>, kind: kUserType<k>)
  TpFun(a: list<(name, kUserType<k>)>, b: kUserType<k>, c: kUserType<k>, range: range)
  TpApp(a: kUserType<k>, b: list<kUserType<k>>, range: range)
  TpVar(name: name, range: range)
  TpCon(name: name, range: range)
  TpParens(t: kUserType<k>, range: range)
  TpAnn(t: kUserType<k>, kind: k)

alias userType = kUserType<userKind>

type userKind
  KindCon(name: name, range: range)
  KindArrow(from: userKind, to: userKind)
  KindParens(kind: userKind, range: range)
  KindNone // Flags no explicit kind annotation


fun main()
  val s = Source("name.kk", "Contents")
  val pos = Pos(s, 0, 0, 0)
  val p = PatLit(LitFloat(10.0, Range(pos, pos)))
  match p
    PatLit(v) -> v.show.println
    _ -> "Not Found".println
