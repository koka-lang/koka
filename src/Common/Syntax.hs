-----------------------------------------------------------------------------
-- Copyright 2012-2023, Microsoft Research, Daan Leijen.
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
                    , DefSort(..), isDefFun, defFun, defFunEx, defSortShowFull
                    , ParamInfo(..)
                    , DefInline(..)
                    , Fip(..), FipAlloc(..), fipIsTail, fipAlloc, noFip, isNoFip
                    , Target(..), CTarget(..), JsTarget(..), isTargetC, isTargetJS, isTargetWasm
                    , isPublic, isPrivate
                    , DataDef(..)
                    , dataDefIsRec, dataDefIsOpen, dataDefIsValue, dataDefSize
                    , ValueRepr(..)
                    , valueReprIsMixed, valueReprIsRaw, valueReprNew, valueReprZero
                    , valueReprRaw, valueReprSize, valueReprScan, valueReprSizeScan
                    , HandlerSort(..)
                    , isHandlerInstance, isHandlerNormal
                    , OperationSort(..), readOperationSort
                    , Platform(..), platform32, platform64, platformCS, platformJS, platform64c
                    , platformHasCompressedFields
                    , alignedSum, alignedAdd, alignUp
                    , BuildType(..)
                    ) where

import Data.List(intersperse)

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
                        , sizeField :: Int -- sizeof(kk_field_t), usually intptr_t but may be smaller for compression
                        , sizeHeader:: Int -- used for correct alignment calculation
                        }

platform32, platform64, platform64c, platformJS, platformCS :: Platform
platform32  = Platform 4 4 4 8
platform64  = Platform 8 8 8 8
platform64c = Platform 8 8 4 8  -- compressed fields
platformJS  = Platform 8 4 8 0
platformCS  = Platform 8 4 8 0


platformHasCompressedFields (Platform sp _ sf _) = (sp /= sf)

instance Show Platform where
  show (Platform sp ss sf sh) = "Platform(sizeof(void*)=" ++ show sp ++ 
                                        ",sizeof(size_t)=" ++ show ss ++ 
                                        ",sizeof(kk_box_t)=" ++ show sf ++ 
                                        ",sizeof(kk_header_t)=" ++ show sh ++ 
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
  = OpVal | OpFun | OpExcept | OpControlRaw | OpControl | OpControlErr
  deriving (Eq,Ord)
  
instance Show OperationSort where
  show opsort = case opsort of
                  OpVal -> "val"
                  OpFun -> "fun"
                  OpExcept -> "brk"
                  OpControl -> "ctl"
                  OpControlRaw -> "rawctl"
                  OpControlErr -> ""
                  
readOperationSort :: String -> Maybe OperationSort
readOperationSort s 
  = case s of 
      "val" -> Just OpVal
      "fun" -> Just OpFun
      "brk" -> Just OpExcept
      "ctl"    -> Just OpControl
      -- legacy
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

data DataDef = DataDefValue !ValueRepr  -- value type
             | DataDefNormal            -- reference type
             | DataDefRec
             | DataDefOpen
             | DataDefAuto              -- Value or Normal; determined by kind inference             
             deriving Eq

instance Show DataDef where
  show dd = case dd of
              DataDefValue v   -> "val" ++ show v
              DataDefNormal    -> "normal"
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
      DataDefValue{} -> True
      _ -> False

dataDefSize :: Platform -> DataDef -> Int
dataDefSize platform ddef
  = case ddef of
      DataDefValue v -> valueReprSize platform v
      _              -> sizeField platform


{--------------------------------------------------------------------------
  Definition kind
--------------------------------------------------------------------------}

data ValueRepr = ValueRepr{ valueReprRawSize    :: !Int {- size in bytes -}, 
                            valueReprScanCount  :: !Int {- count of scannable fields -},
                            valueReprAlignment  :: !Int {- minimal alignment -}
                            -- valueReprSize       :: !Int {- full size, always rawSize + scanFields*sizeField platform -}
                          }
               deriving (Eq,Ord)

instance Show ValueRepr where
  show (ValueRepr raw scan align) 
    = "{" ++ concat (intersperse "," (map show [raw,scan,align])) ++ "}"

valueReprSizeScan :: Platform -> ValueRepr -> (Int,Int)
valueReprSizeScan platform vrepr
  = (valueReprSize platform vrepr, valueReprScanCount vrepr)

valueReprSize :: Platform -> ValueRepr -> Int
valueReprSize platform (ValueRepr raw scan align) = raw + (scan * sizeField platform)

valueReprIsMixed :: ValueRepr -> Bool
valueReprIsMixed v  = (valueReprRawSize v > 0) && (valueReprScanCount v > 0)

valueReprIsRaw :: ValueRepr -> Bool
valueReprIsRaw v  = (valueReprRawSize v > 0) && (valueReprScanCount v == 0)

valueReprNew :: Int -> Int -> Int -> ValueRepr
valueReprNew rawSize scanCount align
  = ValueRepr rawSize scanCount align -- (rawSize + (scanCount * sizeField platform))

valueReprZero :: ValueRepr
valueReprZero = ValueRepr 0 0 0

valueReprRaw :: Int -> ValueRepr
valueReprRaw m  = ValueRepr m 0 m

valueReprScan :: Int -> ValueRepr
valueReprScan n = ValueRepr 0 n 0

{--------------------------------------------------------------------------
  Definition kind
--------------------------------------------------------------------------}

data DefSort
  = DefFun { defFunParamInfos :: [ParamInfo],
             defFunFip        :: Fip } 
  | DefVal 
  | DefVar
  deriving Eq

data ParamInfo 
  = Borrow
  | Own
  deriving(Eq,Show)  

isDefFun (DefFun {})  = True
isDefFun _           = False

defFunEx :: [ParamInfo] -> Fip -> DefSort
defFunEx pinfos fip = if all (==Own) pinfos then DefFun [] fip else DefFun pinfos fip

defFun :: [ParamInfo] -> DefSort
defFun pinfos = defFunEx pinfos noFip

defSortShowFull :: DefSort -> String
defSortShowFull ds
  = case ds of
      DefFun pinfos fip -> show fip ++ "fun"
      DefVal -> "val"
      DefVar -> "var"


instance Show DefSort where
  show ds = case ds of
              DefFun{} -> "fun"
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


{--------------------------------------------------------------------------
  Fip
--------------------------------------------------------------------------}
data Fip = Fip   { fipAlloc_ :: FipAlloc }
         | Fbip  { fipAlloc_ :: FipAlloc, fipTail :: Bool }
         | NoFip { fipTail :: Bool }
         deriving (Eq,Ord)

data FipAlloc = AllocAtMost Int | AllocFinitely | AllocUnlimited
         deriving (Eq)

instance Ord FipAlloc where
  compare a1 a2
    = case (a1, a2) of
        (AllocAtMost n, AllocAtMost m) -> compare n m
        (_            , AllocAtMost _) -> GT

        (AllocAtMost n, AllocFinitely) -> LT
        (AllocFinitely, AllocFinitely) -> EQ
        (AllocUnlimited, AllocFinitely) -> GT

        (AllocUnlimited, AllocUnlimited) -> EQ
        (_             , AllocUnlimited) -> LT

instance Semigroup FipAlloc where
  AllocAtMost n <> AllocAtMost m = AllocAtMost (n + m)
  _ <> _ = AllocFinitely

instance Monoid FipAlloc where
  mempty = AllocAtMost 0

noFip :: Fip
noFip = NoFip False

isNoFip (NoFip _) = True
isNoFip _         = False

fipIsTail :: Fip -> Bool
fipIsTail fip 
  = case fip of 
      Fbip _ t -> t
      NoFip t  -> t
      _        -> True

fipAlloc :: Fip -> FipAlloc
fipAlloc fip 
  = case fip of
      Fip n    -> n
      Fbip n _ -> n
      NoFip _  -> AllocUnlimited

instance Show Fip where
  show fip  = case fip of
                Fip n       -> "fip" ++ showN n
                Fbip n t    -> showTail t ++ "fbip" ++ showN n
                NoFip t     -> showTail t
            where
              showN (AllocAtMost 0) = " "
              showN (AllocAtMost n) = "(" ++ show n ++ ") "
              showN AllocFinitely   = "(n) "
              showN AllocUnlimited  = ""

              showTail True  = "tail "
              showTail _     = " "

              
         
