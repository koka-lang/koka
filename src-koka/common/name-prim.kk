import common/name
import common/syntax

// Special
val nameExpr        = ".expr".newName
val nameType        = ".type".newName

val nameInteractiveModule  = "interactive".newName
val nameInteractive = "interactive".newName
val nameMain        = ".main".newName
val nameCopy        = ".copy".newName
val nameOpExpr      = ".opexpr".newName

// Primitive operations
val nameIf          = "if".newName
val nameCase        = "case".newName

val nameTrace   = "trace".preludeName
val nameLog     = "log".preludeName
val namePhantom = "phantom".preludeName


// Primitive constructors
val nameTpDelay          = "delay".preludeName

val namePatternMatchError = "error-pattern".preludeName
val nameMainConsole      = "main-console".preludeName
val nameSubStr1          = "substr1".preludeName
val nameDec              = "dec".preludeName


val nameTpArray     = "std/data/array".newName.qualify("array".newName)
val nameVector      = "unvlist".preludeName

val namesSameSize   = ["id","map","reverse","foldl","foldr"].map(preludeName)

// Lists
val nameNull        = "Nil".preludeName
val nameCons        = "Cons".preludeName
val nameTpList      = "list".preludeName

val nameIntConst    = ".int-const".preludeName


val nameIntAdd      = "int-add".preludeName
val nameIntSub      = "int-sub".preludeName

// Primitive Type Constructors
val nameTpOperation = "operation".preludeName
val nameTpHandlerBranch0 = "handler-branch0".preludeName
val nameTpHandlerBranch1 = "handler-branch1".preludeName

val nameTpValueOp   = "value".preludeName

val nameAssert      = "assert".preludeName

val nameTpCps       = "cps".preludeName
val nameInCps       = "incps".preludeName
val nameTpCont      = "cont".preludeName

val nameTpAsync     = "std/async".newName.qualify("async".newName)
val nameTpAsyncX    = "std/async".newName.qualify("asyncx".newName)

fun nameYieldOp(n:int): name
  preludeName (".yieldop" ++ (if (n == 0) then "" else "-x" ++ n.show))

val nameToAny       = ".toany".preludeName
val nameApplyK      = ".applyK".preludeName
val nameIsValidK    = ".isValidK".preludeName

fun nameMakeHandler(handlerSort: handlerSort, n: int): name
  preludeName (".make" ++ (if (!handlerSort.isHandlerNormal) then handlerSort.show else "") ++ "Handler" ++ n.show)

fun nameMakeHandlerRet(n: int): name
  preludeName (".makeHandlerRet" ++ n.show)

fun nameMakeContextTp(n: int): name
  preludeName ("resume-context" ++ (if (n==0) then "" else "1"))

val nameMakeNull    = ".null-any".preludeName
val nameConstNull   = "null-const".preludeName
val nameReturnNull   = "null-return".preludeName
val nameReturnNull1   = "null-return1".preludeName

val nameLift        = "lift".preludeName
val nameTpYld       = "yld".preludeName
val nameInject      = ".inject-effect".preludeName
val nameInjectExn   = "inject-exn".preludeName
val nameInjectResource = ".inject-resource".preludeName
val nameTpResourceTag = "resource-tag".preludeName
val nameConResourceTag = ".Resource-tag".preludeName

val nameTpOpMatch   = "opmatch".preludeName
val nameOpMatch     = ".conOpMatch".preludeName
val nameOpNoMatch   = ".conOpNoMatch".preludeName

val nameConEv       = "Ev".preludeName

val nameTpNull      = "null".preludeName
val nameTpIO        = "io".preludeName

val nameTpNamed     = "nmd".preludeName
val nameTpScope     = "scope".preludeName
val nameTpPartial   = "exn".preludeName
val nameTpPure      = "pure".preludeName

val nameTpException  = "exception".preludeName

val nameTpMDict     = nameDict.qualify("mdict".newName)
val nameTpDict      = nameDict.qualify("dict".newName)
val nameTpBuilder   = ("std/text/string").newName.qualify("builder".newName)

val nameTpCTailAcc    = "ctail".cfieldName
val nameTpCField      = "cfield".cfieldName
val nameCFieldHole    = ".cfield-hole".cfieldName
val nameCFieldOf      = ".cfield-of".cfieldName
val nameCTailUnit     = ".ctail-unit".cfieldName
val nameCTailCompose  = ".ctail-compose".cfieldName
val nameCTailApply    = ".ctail-apply".cfieldName
val nameCTailSetCtxPath=".ctail-set-context-path".cfieldName

fun cfieldName(n: string): name
  n.coreTypesName()

// std/core/hnd
val nameTpHTag      = "htag".coreHndName

fun nameTpClause(i:int): name
  ("clause" ++ i.show).coreHndName

val nameTpEv        = "ev".coreHndName
val nameTpEvv       = "evv".coreHndName
val nameTpEvIndex   = "ev-index".coreHndName

fun nameClause(sort, i: int): name
  ("clause-" ++ sort ++ i.show).coreHndName

val nameTpResumeContext = "resume-context".coreHndName

val nameHTag        = ".new-htag".coreHndName

fun namePerform(i: int)
  (".perform" ++ i.show).coreHndName

val nameEvvAt       = ".evv-at".coreHndName
val nameEvvLookup   = ".evv-lookup".coreHndName
val nameEvvIndex    = ".evv-index".coreHndName
val nameMaskAt      = ".mask-at".coreHndName
val nameMaskBuiltin = ".mask-builtin".coreHndName

fun nameOpenAt(i: int)
  (".open-at" ++ i.show).coreHndName

fun nameOpenNone(i: int)
  (".open-none" ++ i.show).coreHndName

fun nameOpen(i: int)
  (".open" ++ i.show).coreHndName

val nameEvvIsAffine = (".evv-is-affine").coreHndName

val nameHandle      = ".hhandle".coreHndName
val nameNamedHandle = ".named-handle".coreHndName

val nameYielding    = "yielding".coreHndName
val nameYieldExtend = "yield-extend".coreHndName
// preludeName  "bind"
val nameBind        = "yield-bind".coreHndName
val nameBind2       = "yield-bind2".coreHndName
val nameEffectOpen  =  ".open".coreTypesName // preludeName ".open"

val nameInitially   = "initially".coreHndName
val nameFinally     = "finally".coreHndName

fun isClauseTailName(n: name): maybe<int>
  val s = n.nameId
  if s.starts-with("clause-tail").bool && s.list.drop(11).all(is-digit) then
    s.list.drop(11).string.parse-int()
  else
    Nothing

// std/core/types

val nameDecreasing  = "unsafe-decreasing".coreTypesName
val nameUnsafeTotal = "unsafe-total".coreTypesName

val nameAssigned    = "assigned".newName
val nameIndex       = "[]".newName
val nameAssign      = ":=".coreTypesName
val nameRefSet      = "set".coreTypesName
val nameLocalSet    = "local-set".coreTypesName
val nameLocalGet    = "local-get".coreTypesName
val nameDeref       = "!".coreTypesName
val nameByref       = "byref".coreTypesName

val namePredHeapDiv = "hdiv".coreTypesName
val namePredEffDiv  = "ediv".coreTypesName
val nameReturn      = ".return".coreTypesName

val nameTpRef       = "ref".coreTypesName
val nameTpLocalVar  = "local-var".coreTypesName
val nameTpLocal     = "local".coreTypesName
val nameRef         = "ref".coreTypesName
val nameLocalNew    = "local-new".coreTypesName
val nameLocal       = "local-var".coreHndName
val nameRunLocal    = "local-scope".coreTypesName

val nameTpTotal     = "total".preludeName
val nameTpDiv       = "div".coreTypesName
val nameTpAlloc     = "alloc".coreTypesName
val nameTpRead      = "read".coreTypesName
val nameTpWrite     = "write".coreTypesName
val nameTpST        = "st".coreTypesName

val nameEffectEmpty = "<>".coreTypesName
val nameEffectExtend= "<|>".coreTypesName
val nameEffectAppend= ".<+>".newName  // only used during kind inference

val nameAnd         = "&&".coreTypesName
val nameOr          = "||".coreTypesName

val nameTpHandled   = "handled".coreTypesName
val nameTpHandled1  = "handled1".coreTypesName

val nameIdentity    = "id".coreTypesName

val nameByte        = "uint8".preludeName
val nameInt8        = "int8".preludeName
val nameInt16       = "int16".preludeName
val nameInt32       = "int32".preludeName
val nameInt64       = "int64".preludeName
val nameSSizeT      = "ssize_t".preludeName
val nameIntPtrT     = "intptr_t".preludeName

val nameUnit        = "()".coreTypesName
val nameTrue        = "True".coreTypesName
val nameFalse       = "False".coreTypesName

val nameJust        = "Just".coreTypesName
val nameNothing     = "Nothing".coreTypesName
val nameTpMaybe     = "maybe".coreTypesName

val nameOptional    = "Optional".coreTypesName
val nameOptionalNone= "None".coreTypesName
val nameTpOptional  = "optional".coreTypesName

val nameTpVoid      = "void".coreTypesName
val nameTpUnit      = "()".coreTypesName
val nameTpBool      = "bool".coreTypesName
val nameTpInt       = "int".coreTypesName

// val nameTpByte      = "uint8".coreTypesName
val nameTpInt8      = "int8".coreTypesName
val nameTpInt16     = "int16".coreTypesName
val nameTpInt32     = "int32".coreTypesName
val nameTpInt64     = "int64".coreTypesName
val nameTpSSizeT    = "ssize_t".coreTypesName
val nameTpIntPtrT   = "intptr_t".coreTypesName

val nameTpFloat     = "float64".coreTypesName
val nameTpFloat32   = "float32".coreTypesName
val nameTpFloat16   = "float16".coreTypesName

val nameTpChar      = "char".coreTypesName
val nameTpString    = "string".coreTypesName
val nameTpAny       = "any".coreTypesName
val nameTpVector    = "vector".coreTypesName

// These are internal only inserted by the boxing phase
val nameTpBox       = ".Box".coreTypesName
val nameBoxCon      = ".Box".coreTypesName
val nameBox         = ".box".coreTypesName
val nameUnbox       = ".unbox".coreTypesName

val nameTpReuse     = "reuse".coreTypesName
val nameReuseNull   = "no-reuse".coreTypesName
val nameDropReuse   = ".drop-reuse".coreTypesName
val nameFreeReuse   = ".free-reuse".coreTypesName
val nameAllocAt     = ".alloc-at".coreTypesName
val nameAssignReuse = ".assign-reuse".coreTypesName
val nameReuse       = ".reuse".coreTypesName
val nameReuseIsValid= ".reuse-is-valid".coreTypesName
val nameConFieldsAssign = ".con-fields-assign".coreTypesName
val nameConTagFieldsAssign = ".con-tag-fields-assign".coreTypesName
val nameKeep        = "keep".coreTypesName
val nameSetTag      = ".set-tag".coreTypesName

val nameDup         = ".dup".coreTypesName
val nameDrop        = ".drop".coreTypesName
val nameFree        = ".free".coreTypesName
val nameDecRef      = ".dec-ref".coreTypesName
val nameIsUnique    = ".is-unique".coreTypesName
val nameKeepMatch   = ".keep-match".coreTypesName
val nameDropMatch   = ".drop-match".coreTypesName
val nameReuseMatch  = ".reuse-match".coreTypesName

val nameReuseDrop   = ".reuse-drop".coreTypesName

val nameDropSpecial    = ".drop-special".coreTypesName

fun nameTuple(n: int): name
  coreTypesName("(" ++ replicate(",", n - 1).join ++ ")")

fun isNameTuple(n: name): bool
  val s = n.nameId.list
  n.nameModule == nameCoreTypes.nameId && s.length >= 2 && s.head.default(' ') == '(' && s.last.default(' ') == ')' && s.tail.init.all((fn(i) -> i == ','))

fun preludeName(s: string): name
  nameSystemCore.qualify(s.newName)

fun coreHndName(s: string): name
  nameCoreHnd.qualify(s.newName)

fun coreTypesName(s: string): name
  nameCoreTypes.qualify(s.newName)

val nameSystemCore  = "std/core".newName
val nameCoreHnd     = "std/core/hnd".newName
val nameCoreTypes   = "std/core/types".newName
val nameDict        = "std/data/dict".newName

fun isSystemCoreName(Name(m, _, _, _)): bool
  [nameSystemCore.nameId, nameCoreHnd.nameId, nameCoreTypes.nameId].any(fn(i) -> i == m)

fun isPrimitiveName(Name(m, _, _, _)): bool
  [nameCoreHnd.nameId, nameCoreTypes.nameId].any(fn(i) -> i == m)

fun isPrimitiveModule(Name(_, _, m, _)): bool
  [nameCoreHnd.nameId, nameCoreTypes.nameId].any(fn(i) -> i == m)

// Primitive Kind Constructors
val nameKindStar    = "V".newName
val nameKindLabel   = "X".newName
val nameKindFun = "->".newName
val nameKindPred    = "P".newName
val nameKindEffect  = "E".newName
val nameKindHeap    = "H".newName
val nameKindScope   = "S".newName
val nameKindHandled = "HX".newName
val nameKindHandled1 = "HX1".newName

fun main()
  nameTpValueOp.show.println
  nameKindHeap.show.println