-----------------------------------------------------------------------------
-- Copyright 2012-2021, Microsoft Research, Daan Leijen.
--
-- This is free software; you can redistribute it and/or modify it under the
-- terms of the Apache License, Version 2.0. A copy of the License can be
-- found in the LICENSE file at the root of this distribution.
-----------------------------------------------------------------------------
{-
    Primitive names.
-}
-----------------------------------------------------------------------------
module Common.NamePrim
          (
          -- * Interpreter
            nameExpr, nameMain, nameType
          , nameInteractiveModule
          , nameSystemCore, nameCoreTypes
          , isSystemCoreName
          , isPrimitiveModule -- no monadic lifting
          , nameCoreHnd
          , isPrimitiveName
          , nameOpExpr

          -- Core names
          , nameCoreFileLine, nameCoreFileModule, nameCoreFileFile

          -- * Operations
          , namePatternMatchError, nameMainConsole
          , nameCopy, copyNameOf
          , nameAssign, nameRefSet, nameAssigned
          , nameByref, nameDeref, nameIndex
          , nameDecreasing, nameSubStr1, nameDec
          , nameAssert

          , nameUnit
          , nameReturn, nameTrace, nameLog, namePhantom
          , nameEffectOpen
          , nameToAny
          , nameIsValidK
          , nameLift, nameBind, nameBind2
          , nameInject, nameInjectExn, nameInjectResource
          , nameTpResourceTag, nameConResourceTag
          , nameConEv

          , nameIntAdd, nameIntSub

          , nameEq, nameShow
          -- Effects
          , nameTpHTag, nameHTag
          , nameTpClause, namePerform
          , nameTpEvv, nameEvvAt, nameEvvLookup, nameEvvIndex
          , nameOpenAt, nameOpen, nameOpenNone
          , nameTpEv, nameHandle, nameNamedHandle
          , nameTpResumeContext
          , nameClause
          , nameIdentity
          , nameMaskAt, nameMaskBuiltin
          , isClauseTailName, nameClauseTailNoOp
          , nameTpEvIndex, nameYielding, nameYieldExtend
          , nameEvvIsAffine
          , nameInitially, nameFinally

          --
          , nameUnsafeTotal
          , nameIntConst
          , nameByte, nameInt16, nameInt32, nameInt64, nameSSizeT, nameIntPtrT

          , nameTpBox, nameUnbox, nameBox, nameBoxCon

          , nameDup, nameDrop, nameIsUnique, nameFree, nameDecRef
          , nameKeepMatch, nameDropMatch, nameReuseMatch
          , nameTpReuse, nameDropReuse, nameFreeReuse
          , nameReuseNull, nameAssignReuse, nameReuse, nameReuseIsValid
          , nameAllocAt, nameConFieldsAssign, nameConTagFieldsAssign, nameReuseDrop
          , nameDropSpecial, nameKeep, nameSetTag

          -- * TRMC optimization, constructor contexts
          , nameTpCCtxx, nameTpCCtx
          , nameCCtxCreate
          , nameCCtxHoleCreate
          , nameCCtxEmpty
          , nameCCtxApply
          , nameCCtxExtend
          , nameCCtxCompose
          , nameCCtxComposeExtend
          , nameCCtxSetCtxPath
          , nameTpFieldAddr, nameFieldAddrOf

          {-
          , nameTpCField,
          , nameTpCTailAcc
          , nameCFieldHole

          -- , nameCFieldSet
          , nameCFieldOf
          , nameCTailUnit
          , nameCTailCompose
          , nameCTailApply
          , nameCTailSetCtxPath
          -}

          -- * Constructors
          , nameTrue, nameFalse
          , nameJust, nameNothing
          , nameOptional, nameOptionalNone
          , nameTpDelay
          -- * Lists
          , nameListNil, nameCons, nameTpList
          -- * Type constructors
          , nameEffectEmpty, nameEffectExtend, nameEffectAppend

          , nameTpBool, nameTpInt, nameTpChar
          , nameTpFloat, nameTpFloat32, nameTpFloat16
          , nameTpString
          -- , nameTpByte
          , nameTpInt8, nameTpInt16, nameTpInt32, nameTpInt64
          , nameTpSSizeT,nameTpIntPtrT
          , nameTpAny
          , nameTpNull
          , nameTpException
          , nameTpMaybe
          , nameTpHandled, nameTpHandled1
          , nameTpOperation, nameYieldOp
          , nameTpCps, nameTpYld, nameTpCont
          , nameInCps
          , nameTpHandlerBranch0, nameTpHandlerBranch1
          , nameMakeNull, nameConstNull, nameReturnNull, nameReturnNull1
          , nameTpValueOp
          , nameTpNamed, nameTpScope


          , nameTpAsync, nameTpAsyncX
          , nameApplyK
          , nameMakeHandler, nameMakeHandlerRet
          , nameMakeContextTp
          , nameTpOpMatch, nameOpMatch, nameOpNoMatch
          , nameTpMDict, nameTpDict, nameTpBuilder

          , nameTpUnit, nameTpVoid

          , nameTpRef, nameRef
          , nameTpLocalVar, nameTpLocal
          , nameLocalVar, nameRunLocal, nameLocalSet, nameLocalGet, nameLocalNew


          , nameTpOptional
          , nameTpArray, nameTpVector, nameVector

          , nameTpTotal, nameTpDiv, nameTpPartial, nameTpPure
          , nameTpST
          , nameTpWrite, nameTpRead
          , nameTpIO
          , nameTpAlloc

          , nameTuple, isNameTuple
          , nameTpTuple, isNameTpTuple
          , nameAnd, nameOr

          , namePredHeapDiv, namePredEffDiv

          -- * Kind constructors
          , nameKindStar, nameKindFun
          , nameKindLabel
          , nameKindPred, nameKindEffect
          , nameKindHeap, nameKindScope
          , nameKindHandled1, nameKindHandled

          , namesSameSize

          ) where

import Data.Char (isDigit)
import Common.Name
import Common.Syntax
import Common.File( startsWith )

{--------------------------------------------------------------------------
  Special
--------------------------------------------------------------------------}
nameExpr        = newName "@expr"
nameType        = newName "@type"

nameInteractiveModule  = newModuleName "interactive"

nameMain        = newName "@main"
nameCopy        = newName "@copy"
nameOpExpr      = newName "@opexpr"

copyNameOf :: Name -> Name
copyNameOf typename
  = qualify (qualifier typename) (qualifyLocally (nameAsModuleName (unqualify typename)) nameCopy)


{--------------------------------------------------------------------------
  Primitive operations
--------------------------------------------------------------------------}
nameIf          = newName "if"
nameCase        = newName "case"

nameTrace   = preludeName "trace"
nameLog     = preludeName "log"
namePhantom = preludeName "phantom"

nameCoreFileFile   = qualify nameSystemCore (newLocallyQualified "" "file" "kk-file")
nameCoreFileLine   = qualify nameSystemCore (newLocallyQualified "" "file" "kk-line")
nameCoreFileModule = qualify nameSystemCore (newLocallyQualified "" "file" "kk-module")


{--------------------------------------------------------------------------
  Primitive constructors
--------------------------------------------------------------------------}
nameTpDelay          = preludeName "delay"

namePatternMatchError = preludeName "error-pattern"
nameMainConsole      = preludeName "main-console"
nameSubStr1          = preludeName "substr1"
nameDec              = preludeName "dec"


nameTpArray     = qualify (newName "std/data/array") (newName "array")
nameVector      = preludeName "unvlist"

namesSameSize   = map preludeName ["id","map","reverse","foldl","foldr"]

{--------------------------------------------------------------------------
  Lists
--------------------------------------------------------------------------}
nameListNil     = preludeName "Nil"
nameCons        = preludeName "Cons"
nameTpList      = preludeName "list"

nameIntConst    = preludeName "@int-const"


nameIntAdd      = preludeName "int-add"
nameIntSub      = preludeName "int-sub"

{--------------------------------------------------------------------------
  Primitive type constructors
--------------------------------------------------------------------------}
nameTpOperation = preludeName "operation"
nameTpHandlerBranch0 = preludeName "handler-branch0"
nameTpHandlerBranch1 = preludeName "handler-branch1"

nameTpValueOp   = preludeName "value"

nameAssert      = preludeName "assert"

nameTpCps       = preludeName "cps"
nameInCps       = preludeName "incps"
nameTpCont      = preludeName "cont"

nameTpAsync     = newQualified "std/async" "async"
nameTpAsyncX    = newQualified "std/async" "asyncx"

nameYieldOp n    = preludeName ("@yieldop" ++ (if (n == 0) then "" else "-x" ++ show n))
nameToAny       = preludeName "@toany"
nameApplyK      = preludeName "@applyK"
nameIsValidK    = preludeName "@isValidK"
nameMakeHandler handlerSort n
  = preludeName ("@make" ++ (if (not (isHandlerNormal handlerSort)) then show handlerSort else "") ++ "Handler" ++ show n)
nameMakeHandlerRet n
  = preludeName ("@makeHandlerRet" ++ show n)

nameMakeContextTp n = preludeName ("resume-context" ++ (if (n==0) then "" else "1"))

nameMakeNull    = preludeName "@null-any"
nameConstNull   = preludeName "null-const"
nameReturnNull   = preludeName "null-return"
nameReturnNull1   = preludeName "null-return1"

nameLift        = preludeName "lift"
nameTpYld       = preludeName "yld"
nameInject      = preludeName "@inject-effect"
nameInjectExn   = preludeName "inject-exn"
nameInjectResource = preludeName "@inject-resource"
nameTpResourceTag = preludeName "resource-tag"
nameConResourceTag = preludeName "@Resource-tag"

nameTpOpMatch   = preludeName "opmatch"
nameOpMatch     = preludeName "@conOpMatch"
nameOpNoMatch   = preludeName "@conOpNoMatch"

nameConEv       = preludeName "Ev"

nameTpNull      = preludeName "null"
nameTpIO        = preludeName "io"

nameTpNamed     = preludeName "nmd"
nameTpScope     = preludeName "scope"
nameTpPartial   = preludeName "exn"
nameTpPure      = preludeName "pure"

nameTpException  = preludeName "exception"

nameTpMDict     = qualify nameDict (newName "mdict")
nameTpDict      = qualify nameDict (newName "dict")
nameTpBuilder   = qualify (newName "std/text/string") (newName "builder")

{-
nameTpCTailAcc    = cfieldName "ctail"
nameTpCField      = cfieldName "cfield"
nameCFieldHole    = cfieldName "@cfield-hole"
nameCFieldOf      = cfieldName "@cfield-of"
nameCTailUnit     = cfieldName "@ctail-unit"
nameCTailCompose  = cfieldName "@ctail-compose"
nameCTailApply    = cfieldName "@ctail-apply"
nameCTailSetCtxPath=cfieldName "@ctail-set-context-path"
-}

cfieldName name   = coreTypesName name

nameTpCCtxx       = cfieldName "cctx"
nameTpCCtx        = cfieldName "ctx"

nameCCtxCreate    = cfieldName "@cctx-create"
nameCCtxHoleCreate= cfieldName "@cctx-hole-create"
nameCCtxExtend    = cfieldName "@cctx-extend"
nameCCtxComposeExtend = cfieldName "@cctx-compose-extend"
nameCCtxEmpty     = cfieldName "cctx-empty"
nameCCtxSetCtxPath= cfieldName "@cctx-setcp"

nameCCtxApply     = cfieldName "([])"
nameCCtxCompose   = cfieldName "(++)"


nameTpFieldAddr   = cfieldName "field-addr"
nameFieldAddrOf   = cfieldName "@field-addr-of"

{--------------------------------------------------------------------------
  std/core/hnd
--------------------------------------------------------------------------}
nameTpHTag      = coreHndName "htag"
nameTpClause i  = coreHndName ("clause" ++ show i)
nameTpEv        = coreHndName "ev"
nameTpEvv       = coreHndName "evv"
nameTpEvIndex   = coreHndName "ev-index"
nameClause sort i = coreHndName ("clause-" ++ sort ++ show i)
nameTpResumeContext = coreHndName "resume-context"

nameHTag        = coreHndName "@new-htag"
namePerform i   = coreHndName ("@perform" ++ show i)
nameEvvAt       = coreHndName "@evv-at"
nameEvvLookup   = coreHndName "@evv-lookup"
nameEvvIndex    = coreHndName "@evv-index"
nameMaskAt      = coreHndName "@mask-at"
nameMaskBuiltin = coreHndName "@mask-builtin"
nameOpenAt i    = coreHndName ("@open-at" ++ show i)
nameOpenNone i  = coreHndName ("@open-none" ++ show i)
nameOpen i      = coreHndName ("@open" ++ show i)
nameEvvIsAffine = coreHndName ("@evv-is-affine")

nameHandle      = coreHndName "@hhandle"
nameNamedHandle = coreHndName "@named-handle"

nameYielding    = coreHndName "yielding"
nameYieldExtend = coreHndName "yield-extend"
nameBind        = coreHndName "yield-bind" -- preludeName "bind"
nameBind2       = coreHndName "yield-bind2"
nameEffectOpen  = coreTypesName "@open" -- preludeName "@open"

nameInitially   = coreHndName "initially"
nameFinally     = coreHndName "finally"

nameClauseTailNoOp n = coreHndName ("clause-tail-noop" ++ show n)

isClauseTailName :: Name -> Maybe Int
isClauseTailName name  | nameModule name /= nameModule nameCoreHnd  = Nothing
isClauseTailName name
  = let s = nameLocal name
    in if (s `startsWith` "clause-tail" && all isDigit (drop 11 s))
        then Just (read (drop 11 s))
        else Nothing



{--------------------------------------------------------------------------
  std/core/types
--------------------------------------------------------------------------}
nameDecreasing  = coreTypesName "unsafe-decreasing"
nameUnsafeTotal = coreTypesName "unsafe-total"

nameAssigned    = newName "assigned"
nameIndex       = newName "index"
nameAssign      = coreTypesName "assign"
nameRefSet      = coreTypesName "set"
nameLocalSet    = coreTypesName "local-set"
nameLocalGet    = coreTypesName "local-get"
nameDeref       = qualifyLocally (newModuleName "ref") (coreTypesName "!")
nameByref       = coreTypesName "byref"

namePredHeapDiv = coreTypesName "hdiv"
namePredEffDiv  = coreTypesName "ediv"
nameReturn      = newHiddenName "return"

nameTpRef       = coreTypesName "ref"
nameTpLocalVar  = coreTypesName "local-var"
nameTpLocal     = coreTypesName "local"
nameRef         = coreTypesName "ref"
nameLocalNew    = coreTypesName "local-new"
nameLocalVar    = coreHndName   "local-var"
nameRunLocal    = coreTypesName "local-scope"

nameTpTotal     = nameEffectEmpty -- coreTypesName "total"
nameTpDiv       = coreTypesName "div"
nameTpAlloc     = coreTypesName "alloc"
nameTpRead      = coreTypesName "read"
nameTpWrite     = coreTypesName "write"
nameTpST        = coreTypesName "st"

nameEffectEmpty = coreTypesName "total"
nameEffectExtend= coreTypesName "effect-extend"
nameEffectAppend= newName "@effect-append"  -- only used during kind inference

nameAnd         = coreTypesName "&&"
nameOr          = coreTypesName "||"

nameTpHandled   = coreTypesName "handled"
nameTpHandled1  = coreTypesName "handled1"

nameIdentity    = coreTypesName "id"

nameByte        = preludeName "uint8"
nameInt8        = preludeName "int8"
nameInt16       = preludeName "int16"
nameInt32       = preludeName "int32"
nameInt64       = preludeName "int64"
nameSSizeT      = preludeName "ssize_t"
nameIntPtrT     = preludeName "intptr_t"

nameUnit        = coreTypesName "Unit"
nameTrue        = coreTypesName "True"
nameFalse       = coreTypesName "False"

nameJust        = coreTypesName "Just"
nameNothing     = coreTypesName "Nothing"
nameTpMaybe     = coreTypesName "maybe"

nameOptional    = coreTypesName "Optional"
nameOptionalNone= coreTypesName "None"
nameTpOptional  = coreTypesName "optional"

nameTpVoid      = coreTypesName "void"
nameTpUnit      = coreTypesName "unit"
nameTpBool      = coreTypesName "bool"
nameTpInt       = coreTypesName "int"

-- nameTpByte      = coreTypesName "uint8"
nameTpInt8      = coreTypesName "int8"
nameTpInt16     = coreTypesName "int16"
nameTpInt32     = coreTypesName "int32"
nameTpInt64     = coreTypesName "int64"
nameTpSSizeT    = coreTypesName "ssize_t"
nameTpIntPtrT   = coreTypesName "intptr_t"

nameTpFloat     = coreTypesName "float64"
nameTpFloat32   = coreTypesName "float32"
nameTpFloat16   = coreTypesName "float16"

nameTpChar      = coreTypesName "char"
nameTpString    = coreTypesName "string"
nameTpAny       = coreTypesName "any"
nameTpVector    = coreTypesName "vector"

-- These are internal only inserted by the boxing phase
nameTpBox       = coreTypesName "@Box"
nameBoxCon      = coreTypesName "@Box"
nameBox         = coreTypesName "@box"
nameUnbox       = coreTypesName "@unbox"

nameTpReuse     = coreTypesName "reuse"
nameReuseNull   = coreTypesName "no-reuse"
nameDropReuse   = coreTypesName "@drop-reuse"
nameFreeReuse   = coreTypesName "@free-reuse"
nameAllocAt     = coreTypesName "@alloc-at"
nameAssignReuse = coreTypesName "@assign-reuse"
nameReuse       = coreTypesName "@reuse"
nameReuseIsValid= coreTypesName "@reuse-is-valid"
nameConFieldsAssign = coreTypesName "@con-fields-assign"
nameConTagFieldsAssign = coreTypesName "@con-tag-fields-assign"
nameKeep        = coreTypesName "keep"
nameSetTag      = coreTypesName "@set-tag"

nameDup         = coreTypesName "@dup"
nameDrop        = coreTypesName "@drop"
nameFree        = coreTypesName "@free"
nameDecRef      = coreTypesName "@dec-ref"
nameIsUnique    = coreTypesName "@is-unique"
nameKeepMatch   = coreTypesName "@keep-match"
nameDropMatch   = coreTypesName "@drop-match"
nameReuseMatch  = coreTypesName "@reuse-match"

nameReuseDrop   = coreTypesName "@reuse-drop"

nameDropSpecial    = coreTypesName "@drop-special"

nameTuple :: Int -> Name
nameTuple n     = if (n <= 1) then nameUnit else coreTypesName ("Tuple" ++ show n) -- ("(" ++ (replicate (n-1) ',') ++ ")")

nameTpTuple :: Int -> Name
nameTpTuple n   = if (n <= 1) then nameTpUnit else coreTypesName ("tuple" ++ show n) -- ("(" ++ (replicate (n-1) ',') ++ ")")


isNameTuple :: Name -> Bool
isNameTuple name
  = (name == nameUnit) ||
    (nameModule name == nameModule nameCoreTypes && (startsWith s "Tuple" && all isDigit (drop 5 s)))
    -- length s >= 2 && head s == '(' && last s == ')' && all (==',') (tail (init s))
  where
    s = nameLocal name

isNameTpTuple :: Name -> Bool
isNameTpTuple name
  = (name == nameTpUnit) ||
    (nameModule name == nameModule nameCoreTypes && (startsWith s "tuple" && all isDigit (drop 5 s)))
    -- length s >= 2 && head s == '(' && last s == ')' && all (==',') (tail (init s))
  where
    s = nameLocal name

nameShow = newName "show"
nameEq = newName "=="

preludeName s
  = qualify nameSystemCore (newName s)

coreHndName s
  = qualify nameCoreHnd (newName s)

coreTypesName s
  = qualify nameCoreTypes (newName s)

nameSystemCore  = newModuleName "std/core"
nameCoreHnd     = newModuleName "std/core/hnd"
nameCoreTypes   = newModuleName "std/core/types"
nameDict        = newModuleName "std/data/dict"

isSystemCoreName name
  = let m = nameModule name
    in  m `elem` [nameModule nameSystemCore, nameModule nameCoreHnd, nameModule nameCoreTypes]

isPrimitiveName name
  = let m = nameModule name
    in  m `elem` [nameModule nameCoreHnd, nameModule nameCoreTypes]

isPrimitiveModule name
  = nameModule name `elem` [nameModule nameCoreHnd, nameModule nameCoreTypes]

{--------------------------------------------------------------------------
  Primitive kind constructors
--------------------------------------------------------------------------}
nameKindStar    = newName "V"
nameKindLabel   = newName "X"
nameKindFun     = newName "->"
nameKindPred    = newName "P"
nameKindEffect  = newName "E"
nameKindHeap    = newName "H"
nameKindScope   = newName "S"
nameKindHandled = newName "HX"
nameKindHandled1 = newName "HX1"
