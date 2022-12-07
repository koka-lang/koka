-----------------------------------------------------------------------------
-- Copyright 2012-2021, Microsoft Research, Daan Leijen.
--
-- This is free software; you can redistribute it and/or modify it under the
-- terms of the Apache License, Version 2.0. A copy of the License can be
-- found in the LICENSE file at the root of this distribution.
-----------------------------------------------------------------------------
{-
    Common syntactical constructs (for Syntax.Syntax and Core.Core)
-}
-----------------------------------------------------------------------------
module Common.Syntax( Visibility(..)
                    , Assoc(..)
                    , Fixity(..)
                    , DataKind(..)
                    , DefSort(..), isDefFun, defFun
                    , ParamInfo(..)
                    , DefInline(..)
                    , Target(..), CTarget(..), JsTarget(..), isTargetC, isTargetJS, isTargetWasm
                    , isPublic, isPrivate
                    , DataDef(..)
                    , dataDefIsRec, dataDefIsOpen, dataDefIsValue
                    , HandlerSort(..)
                    , isHandlerInstance, isHandlerNormal
                    , OperationSort(..), readOperationSort
                    , Platform(..), platform32, platform64, platformCS, platformJS, platform64c
                    , platformHasCompressedFields
                    , alignedSum, alignedAdd, alignUp
                    , BuildType(..)
                    ) where

{--------------------------------------------------------------------------
  Backend targets
--------------------------------------------------------------------------}
data JsTarget = JsDefault | JsNode | JsWeb                 deriving (Eq,Ord)
data CTarget  = CDefault | LibC | Wasm | WasmJs | WasmWeb deriving (Eq,Ord)

data Target = CS | JS JsTarget| C CTarget | Default deriving (Eq,Ord)

isTargetC (C _) = True
isTargetC _     = False

isTargetJS (JS _) = True
isTargetJS _      = False

isTargetWasm :: Target -> Bool
isTargetWasm target 
  = case target of
      C Wasm    -> True
      C WasmJs  -> True
      C WasmWeb -> True
      _         -> False


instance Show Target where
  show tgt = case tgt of
               CS        -> "cs"
               JS JsWeb  -> "jsweb"
               JS JsNode -> "jsnode"
               JS _      -> "js"
               C  Wasm   -> "wasm"
               C  WasmJs -> "wasmjs"
               C  WasmWeb-> "wasmweb"
               C  LibC   -> "libc"
               C  _      -> "c"
               Default   -> ""

data Platform = Platform{ sizePtr   :: Int -- sizeof(intptr_t)
                        , sizeSize  :: Int -- sizeof(size_t)
                        , sizeField :: Int -- sizeof(kk_field_t), usually uintptr_t but may be smaller for compression
                        }

platform32, platform64, platform64c, platformJS, platformCS :: Platform
platform32  = Platform 4 4 4
platform64  = Platform 8 8 8
platform64c = Platform 8 8 4  -- compressed fields
platformJS  = Platform 8 4 8
platformCS  = Platform 8 4 8


platformHasCompressedFields (Platform sp _ sf) = (sp /= sf)

instance Show Platform where
  show (Platform sp ss sf) = "Platform(sizeof(void*)=" ++ show sp ++ 
                             ",sizeof(size_t)=" ++ show ss ++ 
                             ",sizeof(kk_box_t)=" ++ show sf ++ 
                             ")" 


alignedSum :: Int -> [Int] -> Int
alignedSum start xs = foldl alignedAdd start xs
     
alignedAdd :: Int -> Int -> Int
alignedAdd x y = (alignUp x y) + y
     
alignUp :: Int -> Int -> Int
alignUp x y  | y <= 0  = x
alignUp x y  = ((x + y - 1) `div` y)*y    



data BuildType = DebugFull | Debug | RelWithDebInfo | Release
               deriving (Eq,Ord)

instance Show BuildType where
  show DebugFull      = "debugfull"
  show Debug          = "debug"
  show RelWithDebInfo = "drelease"
  show Release        = "release"
  

{--------------------------------------------------------------------------
  Visibility
--------------------------------------------------------------------------}
data Visibility = Public | Private
                deriving (Eq,Ord,Show)

isPublic Public = True
isPublic _      = False

isPrivate Private = True
isPrivate _       = False


data HandlerSort
  = HandlerNormal | HandlerInstance
  deriving (Eq)

instance Show (HandlerSort) where
  show hsort = case hsort of
                 HandlerNormal -> "normal"
                 HandlerInstance -> "named"

isHandlerInstance (HandlerInstance) = True
isHandlerInstance _ = False

isHandlerNormal (HandlerNormal) = True
isHandlerNormal _ = False


data OperationSort 
  = OpVal | OpFun | OpExcept | OpControlRaw | OpControl
  deriving (Eq,Ord)
  
instance Show OperationSort where
  show opsort = case opsort of
                  OpVal -> "val"
                  OpFun -> "fun"
                  OpExcept -> "brk"
                  OpControl -> "ctl"
                  OpControlRaw -> "rawctl"
                  
readOperationSort :: String -> Maybe OperationSort
readOperationSort s 
  = case s of 
      "val" -> Just OpVal
      "fun" -> Just OpFun
      "brk" -> Just OpExcept
      "ctl"    -> Just OpControl
      "rawctl" -> Just OpControlRaw
      "except" -> Just OpExcept
      "control"  -> Just OpControl
      "rcontrol" -> Just OpControlRaw
      _ -> Nothing
  
{--------------------------------------------------------------------------
  DataKind
--------------------------------------------------------------------------}
data DataKind = Inductive | CoInductive | Retractive
              deriving (Eq)

instance Show DataKind where
  show Inductive = "type"
  show CoInductive = "cotype"
  show Retractive = "rectype"

data DataDef = DataDefValue{ rawFields :: Int {- size in bytes -}, scanFields :: Int {- count of scannable fields -}}
             | DataDefNormal
             | DataDefAuto   -- Value or Normal; determined by kind inference
             | DataDefRec
             | DataDefOpen
             deriving Eq

instance Show DataDef where
  show dd = case dd of
              DataDefValue m n -> "val(raw:" ++ show m ++ ",scan:" ++ show n ++ ")"
              DataDefNormal{}  -> "normal"
              DataDefRec       -> "rec"
              DataDefOpen      -> "open"
              DataDefAuto      -> "auto"

dataDefIsRec ddef
  = case ddef of
      DataDefValue{}   -> False
      DataDefNormal    -> False
      DataDefAuto      -> False
      _  -> True

dataDefIsOpen ddef
  = case ddef of
      DataDefOpen -> True
      _ -> False

dataDefIsValue ddef
  = case ddef of
      DataDefValue _ _ -> True
      _ -> False

{--------------------------------------------------------------------------
  Definition kind
--------------------------------------------------------------------------}

data DefSort
  = DefFun [ParamInfo] | DefVal | DefVar
  deriving Eq

data ParamInfo 
  = Borrow
  | Own
  deriving(Eq,Show)  

isDefFun (DefFun _)  = True
isDefFun _           = False

defFun :: [ParamInfo] -> DefSort
defFun pinfos = if all (==Own) pinfos then DefFun [] else DefFun pinfos

instance Show DefSort where
  show ds = case ds of
              DefFun _ -> "fun"
              DefVal -> "val"
              DefVar -> "var"


data DefInline
  = InlineNever | InlineAlways | InlineAuto
  deriving (Eq,Ord)

instance Show DefInline where
  show di = case di of
              InlineNever  -> "noinline"
              InlineAlways -> "inline"
              InlineAuto   -> "autoinline"

{--------------------------------------------------------------------------
  Fixities
--------------------------------------------------------------------------}

-- | Operator fixity
data Fixity = FixInfix  Int Assoc -- ^ precedence and associativity
            | FixPrefix
            | FixPostfix
            deriving (Eq,Show)

-- | Operator associativity
data Assoc  = AssocNone
            | AssocRight
            | AssocLeft
            deriving (Eq,Show)
