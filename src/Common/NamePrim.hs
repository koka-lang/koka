-----------------------------------------------------------------------------
-- Copyright 2012 Microsoft Corporation.
--
-- This is free software; you can redistribute it and/or modify it under the
-- terms of the Apache License, Version 2.0. A copy of the License can be
-- found in the file "license.txt" at the root of this distribution.
-----------------------------------------------------------------------------
{-
    Primitive names.
-}
-----------------------------------------------------------------------------
module Common.NamePrim
          (
          -- * Interpreter
            nameExpr, nameMain, nameType
          , nameInteractive, nameInteractiveModule
          , nameSystemCore, nameCoreTypes, isSystemCoreName
          , nameOpExpr

          -- * Operations
          , namePatternMatchError, nameMainConsole
          , nameCopy
          , nameAssign, nameRefSet, nameAssigned
          , nameByref, nameDeref, nameIndex
          , nameDecreasing, nameSubStr1, nameDec

          , nameUnit
          , nameReturn, nameTrace, nameLog, namePhantom
          , nameEffectOpen
          , nameToAny
          , nameEnsureK
          , nameIsValidK
          , nameLift, nameBind, nameBind2
          , nameInject, nameInjectExn, nameInjectResource
          , nameTpResourceTag, nameConResourceTag
          , nameConEv

          -- Effects
          , nameTpHTag, nameHTag
          , nameTpClause, namePerform
          , nameTpEvv, nameEvvAt, nameEvvLookup, nameEvvIndex
          , nameOpenAt, nameOpen, nameOpenNone
          , nameTpEv, nameHandle, nameNamedHandle
          , nameClause
          , nameIdentity
          , nameMaskAt, nameMaskBuiltin
          , isClauseTailName, nameClauseTailNoYield
          , nameTpEvIndex, nameYielding, nameYieldExtend
          , nameEvvIsAffine
          , nameInitially, nameFinally

          --
          , nameUnsafeTotal
          , nameIntConst, nameInt32, nameSizeT

          , nameTpBox, nameUnbox, nameBox, nameBoxCon

          , nameDup, nameDrop, nameIsUnique, nameFree, nameDecRef
          , nameKeepMatch, nameDropMatch, nameReuseMatch
          , nameTpReuse, nameDropReuse, nameFreeReuse
          , nameReuseNull, nameAssignReuse, nameReuse, nameReuseIsValid
          , nameAllocAt, nameConFieldsAssign

          -- * CTail optimization
          , nameTpCField, nameTpCTailAcc
          , nameCFieldHole
          , nameCFieldSet
          , nameCFieldOf
          , nameCTailNil
          , nameCTailLink
          , nameCTailResolve

          -- * Constructors
          , nameTrue, nameFalse
          , nameJust, nameNothing
          , nameOptional, nameOptionalNone
          , nameTpDelay
          -- * Lists
          , nameNull, nameCons, nameTpList
          -- * Type constructors
          , nameEffectEmpty, nameEffectExtend, nameEffectAppend

          , nameTpBool, nameTpInt, nameTpChar
          , nameTpFloat, nameTpFloat32
          , nameTpString
          , nameTpInt32, nameTpInt64, nameTpInt16, nameTpInt8, nameTpByte, nameTpSizeT
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
          , nameLocal, nameRunLocal, nameLocalSet, nameLocalGet, nameLocalNew


          , nameTpOptional
          , nameTpArray, nameTpVector, nameVector

          , nameTpTotal, nameTpDiv, nameTpPartial, nameTpPure
          , nameTpST
          , nameTpWrite, nameTpRead
          , nameTpIO
          , nameTpAlloc

          , nameTuple, isNameTuple
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
nameExpr        = newName ".expr"
nameType        = newName ".type"

nameInteractiveModule  = newName "interactive"
nameInteractive = newName "interactive"
nameMain        = newName ".main"
nameCopy        = newName ".copy"
nameOpExpr      = newName ".opexpr"

{--------------------------------------------------------------------------
  Primitive operations
--------------------------------------------------------------------------}
nameIf          = newName "if"
nameCase        = newName "case"

nameTrace   = preludeName "trace"
nameLog     = preludeName "log"
namePhantom = preludeName "phantom"


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
nameNull        = preludeName "Nil"
nameCons        = preludeName "Cons"
nameTpList      = preludeName "list"

nameIntConst    = preludeName ".int-string"

{--------------------------------------------------------------------------
  Primitive type constructors
--------------------------------------------------------------------------}
nameTpOperation = preludeName "operation"
nameTpHandlerBranch0 = preludeName "handler-branch0"
nameTpHandlerBranch1 = preludeName "handler-branch1"

nameTpValueOp   = preludeName "value"

nameTpCps       = preludeName "cps"
nameInCps       = preludeName "incps"
nameTpCont      = preludeName "cont"
nameEnsureK     = preludeName "ensureK"
nameTpAsync     = qualify (newName "std/async") (newName "async")
nameTpAsyncX    = qualify (newName "std/async") (newName "asyncx")

nameYieldOp n    = preludeName (".yieldop" ++ (if (n == 0) then "" else "-x" ++ show n))
nameToAny       = preludeName ".toany"
nameApplyK      = preludeName ".applyK"
nameIsValidK    = preludeName ".isValidK"
nameMakeHandler handlerSort n
  = preludeName (".make" ++ (if (not (isHandlerNormal handlerSort)) then show handlerSort else "") ++ "Handler" ++ show n)
nameMakeHandlerRet n
  = preludeName (".makeHandlerRet" ++ show n)

nameMakeContextTp n = preludeName ("resume-context" ++ (if (n==0) then "" else "1"))

nameMakeNull    = preludeName ".null-any"
nameConstNull   = preludeName "null-const"
nameReturnNull   = preludeName "null-return"
nameReturnNull1   = preludeName "null-return1"

nameLift        = preludeName "lift"
nameTpYld       = preludeName "yld"
nameInject      = preludeName ".inject-effect"
nameInjectExn   = preludeName "inject-exn"
nameInjectResource = preludeName ".inject-resource"
nameTpResourceTag = preludeName "resource-tag"
nameConResourceTag = preludeName ".Resource-tag"

nameTpOpMatch   = preludeName "opmatch"
nameOpMatch     = preludeName ".conOpMatch"
nameOpNoMatch   = preludeName ".conOpNoMatch"

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

nameTpCTailAcc    = cfieldName "ctail"
nameTpCField      = cfieldName "cfield"
nameCFieldHole    = cfieldName ".cfield-hole"
nameCFieldSet     = cfieldName "cfield-set"   -- private (not hidden)
nameCFieldOf      = cfieldName ".cfield-of"
nameCTailNil      = cfieldName ".ctail-nil"
nameCTailLink     = cfieldName ".ctail-link"
nameCTailResolve  = cfieldName ".ctail-resolve"
cfieldName name   = coreTypesName name

{--------------------------------------------------------------------------
  std/core/hnd
--------------------------------------------------------------------------}
nameTpHTag      = coreHndName "htag"
nameTpClause i  = coreHndName ("clause" ++ show i)
nameTpEv        = coreHndName "ev"
nameTpEvv       = coreHndName "evv"
nameTpEvIndex   = coreHndName "ev-index"
nameClause sort i = coreHndName ("clause-" ++ sort ++ show i)

nameHTag        = coreHndName ".new-htag"
namePerform i   = coreHndName (".perform" ++ show i)
nameEvvAt       = coreHndName ".evv-at"
nameEvvLookup   = coreHndName ".evv-lookup"
nameEvvIndex    = coreHndName ".evv-index"
nameMaskAt      = coreHndName ".mask-at"
nameMaskBuiltin = coreHndName ".mask-builtin"
nameOpenAt i    = coreHndName (".open-at" ++ show i)
nameOpenNone i  = coreHndName (".open-none" ++ show i)
nameOpen i      = coreHndName (".open" ++ show i)
nameEvvIsAffine = coreHndName (".evv-is-affine")

nameHandle      = coreHndName ".hhandle"
nameNamedHandle = coreHndName ".named-handle"

nameYielding    = coreHndName "yielding"
nameYieldExtend = coreHndName "yield-extend"
nameBind        = coreHndName "yield-bind" -- preludeName "bind"
nameBind2       = coreHndName "yield-bind2"
nameEffectOpen  = coreHndName ".open" -- preludeName ".open"

nameInitially   = coreHndName "initially"
nameFinally     = coreHndName "finally"

nameClauseTailNoYield n = coreHndName ("clause-tail-noyield" ++ show n)

isClauseTailName :: Name -> Maybe Int
isClauseTailName name  | nameModule name /= nameId nameCoreHnd  = Nothing
isClauseTailName name
  = let s = nameId name
    in if (s `startsWith` "clause-tail" && all isDigit (drop 11 s))
        then Just (read (drop 11 s))
        else Nothing



{--------------------------------------------------------------------------
  std/core/types
--------------------------------------------------------------------------}
nameDecreasing  = coreTypesName "unsafe-decreasing"
nameUnsafeTotal = coreTypesName "unsafe-total"

nameAssigned    = newName "assigned"
nameIndex       = newName "[]"
nameAssign      = coreTypesName ":="
nameRefSet      = coreTypesName "set"
nameLocalSet    = coreTypesName "local-set"
nameLocalGet    = coreTypesName "local-get"
nameDeref       = coreTypesName "!"
nameByref       = coreTypesName "byref"

namePredHeapDiv = coreTypesName "hdiv"
namePredEffDiv  = coreTypesName "ediv"
nameReturn      = coreTypesName ".return"

nameTpRef       = coreTypesName "ref"
nameTpLocalVar  = coreTypesName "local-var"
nameTpLocal     = coreTypesName "local"
nameRef         = coreTypesName "ref"
nameLocalNew    = coreTypesName "local-new"
nameLocal       = coreHndName   "local-var"
nameRunLocal    = coreTypesName "local-scope"

nameTpTotal     = preludeName "total"
nameTpDiv       = coreTypesName "div"
nameTpAlloc     = coreTypesName "alloc"
nameTpRead      = coreTypesName "read"
nameTpWrite     = coreTypesName "write"
nameTpST        = coreTypesName "st"

nameEffectEmpty = coreTypesName "<>"
nameEffectExtend= coreTypesName "<|>"
nameEffectAppend= newName ".<+>"  -- only used during kind inference

nameAnd         = coreTypesName "&&"
nameOr          = coreTypesName "||"

nameTpHandled   = coreTypesName "handled"
nameTpHandled1  = coreTypesName "handled1"

nameIdentity    = coreTypesName "id"

nameInt32       = preludeName "int32"
nameSizeT       = preludeName "size_t"

nameUnit        = coreTypesName "()"
nameTrue        = coreTypesName "True"
nameFalse       = coreTypesName "False"

nameJust        = coreTypesName "Just"
nameNothing     = coreTypesName "Nothing"
nameTpMaybe     = coreTypesName "maybe"

nameOptional    = coreTypesName "Optional"
nameOptionalNone= coreTypesName "None"
nameTpOptional  = coreTypesName "optional"

nameTpVoid      = coreTypesName "void"
nameTpUnit      = coreTypesName "()"
nameTpBool      = coreTypesName "bool"
nameTpInt       = coreTypesName "int"
nameTpInt32     = coreTypesName "int32"
nameTpInt64     = coreTypesName "int64"
nameTpInt16     = coreTypesName "int16"
nameTpInt8      = coreTypesName "int8"
nameTpSizeT     = coreTypesName "size_t"
nameTpByte      = coreTypesName "uint8"
nameTpFloat     = coreTypesName "double"
nameTpFloat32   = coreTypesName "float32"
nameTpChar      = coreTypesName "char"
nameTpString    = coreTypesName "string"
nameTpAny       = coreTypesName "any"
nameTpVector    = coreTypesName "vector"

-- These are internal only inserted by the boxing phase
nameTpBox       = coreTypesName ".Box"
nameBoxCon      = coreTypesName ".Box"
nameBox         = coreTypesName ".box"
nameUnbox       = coreTypesName ".unbox"

nameTpReuse     = coreTypesName "reuse"
nameReuseNull   = coreTypesName "no-reuse"
nameDropReuse   = coreTypesName ".drop-reuse"
nameFreeReuse   = coreTypesName ".free-reuse"
nameAllocAt     = coreTypesName ".alloc-at"
nameAssignReuse = coreTypesName ".assign-reuse"
nameReuse       = coreTypesName ".reuse"
nameReuseIsValid= coreTypesName ".reuse-is-valid"
nameConFieldsAssign = coreTypesName ".con-fields-assign"

nameDup         = coreTypesName ".dup"
nameDrop        = coreTypesName ".drop"
nameFree        = coreTypesName ".free"
nameDecRef      = coreTypesName ".dec-ref"
nameIsUnique    = coreTypesName ".is-unique"
nameKeepMatch   = coreTypesName ".keep-match"
nameDropMatch   = coreTypesName ".drop-match"
nameReuseMatch  = coreTypesName ".reuse-match"

nameTuple :: Int -> Name
nameTuple n     = coreTypesName ("(" ++ (replicate (n-1) ',') ++ ")")

isNameTuple :: Name -> Bool
isNameTuple name
  = nameModule name == nameId nameCoreTypes && length s >= 2 && head s == '(' && last s == ')' && all (==',') (tail (init s))
  where
    s = nameId name


preludeName s
  = qualify nameSystemCore (newName s)

coreHndName s
  = qualify nameCoreHnd (newName s)

coreTypesName s
  = qualify nameCoreTypes (newName s)

nameSystemCore  = newName "std/core"
nameCoreHnd     = newName "std/core/hnd"
nameCoreTypes   = newName "std/core/types"
nameDict        = newName "std/data/dict"

isSystemCoreName name
  = let m = nameModule name
    in  m `elem` [nameId nameSystemCore, nameId nameCoreHnd, nameId nameCoreTypes]

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
