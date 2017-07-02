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
          , nameSystemCore
--          , nameCore
          , nameOpExpr

          -- * Operations
          , namePatternMatchError, nameMainConsole
          , nameCopy
          , nameAssign, nameRefSet, nameAssigned
          , nameByref, nameDeref, nameIndex
          , nameDecreasing, nameSubStr1, nameDec

          , nameUnit
          , nameReturn, nameTrace, nameLog
          , nameEffectOpen
          , nameToAny
          , nameEnsureK
          , nameIsValidK
          , nameUnsafeTotal
          , nameIntConst, nameInt32

          -- * Constructors
          , nameTrue, nameFalse
          , nameJust, nameNothing
          , nameOptional, nameOptionalNone
          , nameTpDelay
          -- * Lists
          , nameNull, nameCons, nameTpList
          -- * Type constructors
          , nameEffectEmpty, nameEffectExtend
          , nameEffectAppend

          , nameTpBool, nameTpInt, nameTpChar
          , nameTpFloat
          , nameTpString
          , nameTpInt32
          , nameTpAny
          , nameTpNull
          , nameTpException
          , nameTpHandled, nameTpHandled1
          , nameTpOperation, nameYieldOp, nameYieldOp1
          , nameTpCps, nameTpYld, nameTpCont
          , nameInCps

          , nameTpAsync
          , nameApplyK
          , nameMakeHandler, nameMakeHandlerRet
          , nameTpOpMatch, nameOpMatch, nameOpNoMatch
          , nameTpMDict, nameTpDict, nameTpBuilder, nameTpTime

          , nameTpUnit, nameTpVoid
          , nameTpRef, nameRef
          , nameTpOptional
          , nameTpArray, nameTpVector

          , nameTpTotal, nameTpDiv, nameTpPartial, nameTpPure
          , nameTpST
          , nameTpWrite, nameTpRead
          , nameTpIO
          , nameTpAlloc
          
          , nameTuple, isNameTuple

          , namePredHeapDiv

          -- * Kind constructors
          , nameKindStar, nameKindFun
          , nameKindLabel
          , nameKindPred, nameKindEffect
          , nameKindHeap
          , nameKindHandled1, nameKindHandled

          , toShortModuleName

          , namesSameSize
          ) where

import Common.Name



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
nameUnit        = preludeName "()"

namePredHeapDiv :: Name
namePredHeapDiv = preludeName "hdiv"

nameReturn :: Name
nameReturn = preludeName ".return"

nameTrace  = preludeName "trace"
nameLog    = preludeName "log"

nameEffectOpen :: Name
nameEffectOpen = newName ".open"

{--------------------------------------------------------------------------
  Primitive constructors
--------------------------------------------------------------------------}
nameTrue        = preludeName "True"
nameFalse       = preludeName "False"

nameJust        = preludeName "Just"
nameNothing     = preludeName "Nothing"

nameOptional         = preludeName "Optional"
nameOptionalNone     = preludeName "None"
nameTpOptional       = preludeName "optional"

nameTpDelay          = preludeName "delay"

namePatternMatchError = preludeName "error-pattern"
nameMainConsole      = preludeName "main-console"
nameSubStr1          = preludeName "substr1"
nameDec              = preludeName "dec"

nameAssign      = preludeName ":="
nameAssigned    = newName "assigned"
nameRefSet      = preludeName "set"

nameDeref       = preludeName "!"
nameByref       = preludeName ".&"
nameIndex       = newName "[]"

nameTpArray     = qualify (newName "std/array") (newName "array") 
nameTpVector    = preludeName "vector"

namesSameSize   = map preludeName ["id","map","reverse","foldl","foldr"]
nameDecreasing  = preludeName "unsafe-decreasing"
nameUnsafeTotal = preludeName "unsafe-total"

{--------------------------------------------------------------------------
  Lists
--------------------------------------------------------------------------}
nameNull        = preludeName "Nil"
nameCons        = preludeName "Cons"
nameTpList      = preludeName "list"

nameIntConst    = preludeName ".int-string"
nameInt32       = preludeName "int32"

{--------------------------------------------------------------------------
  Primitive type constructors
--------------------------------------------------------------------------}
nameEffectEmpty = preludeName "<>"
nameEffectExtend= preludeName "<|>"
nameEffectAppend= newName ".<+>"

nameTpHandled   = preludeName "handled"
nameTpHandled1  = preludeName "handled1"
nameTpOperation = preludeName "operation"

nameTpCps       = preludeName "cps"
nameInCps       = preludeName "incps"
nameTpYld       = preludeName "yld"
nameTpCont      = preludeName "cont"
nameEnsureK     = preludeName "ensureK"
nameTpAsync     = qualify (newName "std/async") (newName "async")

nameYieldOp     = preludeName ".yieldop"
nameYieldOp1    = preludeName ".yieldop1"
nameToAny       = preludeName ".toany"
nameApplyK      = preludeName ".applyK"
nameIsValidK    = preludeName ".isValidK"
nameMakeHandler shallow n 
  = preludeName (".make" ++ (if shallow then "Shallow" else "") ++ "Handler" ++ show n)
nameMakeHandlerRet n 
  = preludeName (".makeHandlerRet" ++ show n)

nameTpOpMatch   = preludeName "opmatch"
nameOpMatch     = preludeName ".conOpMatch"
nameOpNoMatch   = preludeName ".conOpNoMatch"


nameTpBool      = preludeName "bool"
nameTpInt       = preludeName "int"
nameTpInt32     = preludeName "int32"
nameTpFloat     = preludeName "double"
nameTpChar      = preludeName "char"
nameTpString    = preludeName "string"
nameTpAny       = preludeName "any"
nameTpNull      = preludeName "null"

nameTpIO        = preludeName "io"
nameTpUnit      = preludeName "()"
nameTpRef       = preludeName "ref"
nameRef         = preludeName "ref"

nameTpTotal     = preludeName "total"
nameTpPartial   = preludeName "exn"
nameTpDiv       = preludeName "div"
nameTpPure      = preludeName "pure"


nameTpAlloc        = preludeName "alloc"
nameTpRead         = preludeName "read"
nameTpWrite        = preludeName "write"
nameTpST           = preludeName "st"

nameTpVoid       = preludeName "void"
nameTpException  = preludeName "exception"

nameTpMDict     = qualify nameDict (newName "mdict")
nameTpDict      = qualify nameDict (newName "dict")
nameTpBuilder   = qualify (newName "std/string") (newName "builder")
nameTpTime      = qualify (newName "std/time") (newName "time")

nameTuple :: Int -> Name
nameTuple n     = preludeName ("(" ++ (replicate (n-1) ',') ++ ")")

isNameTuple :: Name -> Bool
isNameTuple name
  = nameModule name == nameId nameSystemCore && length s >= 2 && head s == '(' && last s == ')' && all (==',') (tail (init s))
  where
    s = nameId name

preludeName s
  = qualify nameSystemCore (newName s)

nameSystemCore  = newName "std/core"
nameCore        = newName "core"
nameDict        = newName "std/dict"

toShortModuleName :: Name -> Name
toShortModuleName name
  = let short = last (splitModuleName name) in
    if (short == nameCore) then nameSystemCore else short  -- so primitives can be qualified correctly
    
{--------------------------------------------------------------------------
  Primitive kind constructors
--------------------------------------------------------------------------}
nameKindStar    = newName "V"
nameKindLabel   = newName "X"
nameKindFun     = newName "->"
nameKindPred    = newName "P"
nameKindEffect  = newName "E"
nameKindHeap    = newName "H"
nameKindHandled = newName "HX"
nameKindHandled1 = newName "HX1"