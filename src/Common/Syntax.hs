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
{-# LANGUAGE InstanceSigs #-}
module Common.Syntax( Visibility(..)
                    , Assoc(..)
                    , Fixity(..)
                    , DataKind(..)
                    , DefSort(..), isDefFun, defFun, defFunEx, defSortShowFull
                    , ParamInfo(..)
                    , DefInline(..)
                    , Fip(..), FipAlloc(..), fipSubsumes, fipIsTail, fipAlloc, noFip, isNoFip, isFipTop, fipMax, fipTop, fipBot, fipNoAlloc
                    , Target(..), CTarget(..), JsTarget(..), isTargetC, isTargetJS, isTargetWasm
                    , isPublic, isPrivate
                    , DataDef(..)
                    , dataDefIsOpen, dataDefIsExtend, dataDefIsValue, dataDefSize, dataDefIsNormal, dataDefIsLazy
                    , DataEffect(..)
                    , ValueRepr(..)
                    , valueReprIsMixed, valueReprIsRaw, valueReprNew, valueReprZero
                    , valueReprRaw, valueReprSize, valueReprScan, valueReprSizeScan
                    , HandlerSort(..)
                    , isHandlerInstance, isHandlerNormal
                    , OperationSort(..), readOperationSort, opSortString
                    , Platform(..), platform32, platform64, platformCS, platformJS, platform64c, platformNone
                    , platformHasCompressedFields
                    , alignedSum, alignedAdd, alignUp
                    , BuildType(..)
                    , sepBySpace, memberDoc
                    , TargetPlatform(..), targetPlatformFromTarget
                    , targetIds, targetFromString
                    , lookupBestTarget
                    , targetPlatformDefault, targetPlatformIsDefault
                    -- , targetPlatformTryMatch
                    ) where
import Debug.Trace
import Data.Tuple(swap)
import Data.Maybe(catMaybes)
import Data.List(intersperse,sort,intercalate)

{--------------------------------------------------------------------------
  Backend targets

  backend:  c js cs
  host:     c : libc wasm wasmweb  (wasm==wasi wasmweb==emscripten)
            js: node web  
            cs: dotnet
  platform: 32 64 64c js cs
  arch:     x86 x64 arm32 arm64 riscv  -<variant>
  os:       windows linux macos unix   -<variant>

  target option:   
    c    c64 c32 c64c
    js   jsnode jsweb
    wasm wasm32 wasm64 wasmjs wasmweb
    cs

--------------------------------------------------------------------------}
data JsTarget = JsDefault | JsNode | JsWeb                deriving (Eq,Ord)
data CTarget  = CDefault | LibC | Wasm | WasmJs | WasmWeb deriving (Eq,Ord)

data Target = Default | CS | JS !JsTarget| C !CTarget | Unsupported    deriving (Eq,Ord)

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

targetIds :: [(Target,String)]
targetIds = [
  (CS,"cs"),
  (JS JsWeb,"jsweb"),
  (JS JsNode,"jsnode"),
  (JS JsDefault,"js"),
  (C Wasm,"wasm"),
  (C WasmJs,"wasmjs"),
  (C WasmWeb,"wasmweb"),
  (C LibC,"libc"),
  (C CDefault,"c"),
  (Unsupported,"unsupported")
  ]

targetFromString :: String -> Target
targetFromString id
  = case lookup id (map swap targetIds) of
      Just t  -> t
      Nothing -> Default

instance Show Target where
  show tgt = case lookup tgt targetIds of
               Just s -> s
               _      -> ""


data Platform = Platform{ sizePtr   :: !Int -- sizeof(intptr_t)
                        , sizeSize  :: !Int -- sizeof(size_t)
                        , sizeField :: !Int -- sizeof(kk_field_t), usually intptr_t but may be smaller for compression
                        , sizeHeader:: !Int -- used for correct alignment calculation
                        } deriving (Eq,Ord)

platform32, platform64, platform64c, platformJS, platformCS :: Platform
platform32  = Platform 4 4 4 8
platform64  = Platform 8 8 8 8
platform64c = Platform 8 8 4 8  -- compressed fields
platformJS  = Platform 8 4 8 0
platformCS  = Platform 8 4 8 0
platformNone = Platform 0 0 0 0

instance Show Platform where
  show p
    = if p==platform32 then "p32"
      else if p==platform64 then "p64"
      else if p==platform64c then "p64c"
      else if p==platformNone then "none"
      else platformShow p


platformHasCompressedFields (Platform sp _ sf _) = (sp /= sf)

platformShow (Platform sp ss sf sh) = "p(sizeof(void*)=" ++ show sp ++
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

data TargetPlatform = TargetPlatform{ tplTarget :: !Target, tplOS :: !String, tplArch :: !String, tplPlatform :: Platform }
                    deriving (Eq)

instance Ord TargetPlatform where
  compare :: TargetPlatform -> TargetPlatform -> Ordering
  compare (TargetPlatform t1 os1 arch1 p1) (TargetPlatform t2 os2 arch2 p2)
    = case compare t1 t2 of
        EQ   -> compare (os1,arch1,p1) (os2,arch2,p2)
        ltgt -> ltgt

instance Show TargetPlatform where
  show (TargetPlatform tgt os arch p)
    = showTarget tgt ++ (if null attrs then "" else "[" ++ intercalate "," attrs ++ "]")
    where
      attrs = concat $
        [hostAttr tgt,
         if os=="" then [] else ["os=" ++ os],
         if arch=="" then [] else ["arch=" ++ arch],
         if p==platformNone then [] else ["platform=" ++ show p]
        ]

      showTarget t
        = case t of
            C _  -> "c"
            JS _ -> "js"
            CS   -> "cs"
            _    -> "default"

      hostAttr t
        = case t of
            C LibC    -> ["host=libc"]
            C Wasm    -> ["host=wasm"]
            C WasmJs  -> ["host=wasmjs"]
            C WasmWeb -> ["host=wasmweb"]
            JS JsNode -> ["host=jsnode"]
            JS JsWeb  -> ["host=jsweb"]
            _         -> []


targetPlatformDefault :: TargetPlatform
targetPlatformDefault = targetPlatformFromTarget Default

targetPlatformFromTarget :: Target -> TargetPlatform
targetPlatformFromTarget target = TargetPlatform target "" "" platformNone

targetPlatformIsDefault :: TargetPlatform -> Bool
targetPlatformIsDefault (TargetPlatform Default "" "" (Platform 0 0 0 0)) = True
targetPlatformIsDefault _ = False

lookupBestTarget :: Ord a => TargetPlatform -> [(TargetPlatform,a)] -> Maybe a
lookupBestTarget tpl xs
  = let targets = let target = tplTarget tpl
                  in case target of
                      C WasmJs      -> [target,C Wasm,C CDefault]
                      C WasmWeb     -> [target,C Wasm,C CDefault]
                      C CDefault    -> [target]
                      C _           -> [target,C CDefault]
                      JS JsDefault  -> [target]
                      JS _          -> [target,JS JsDefault]
                      _             -> [target]                      
    in case catMaybes (map (\t -> targetPlatformBestMatch (tpl{ tplTarget = t }) xs) targets) of
         (x:_) -> Just x
         _     -> Nothing

targetPlatformBestMatch :: Ord a => TargetPlatform -> [(TargetPlatform,a)] -> Maybe a
targetPlatformBestMatch eguard xs
  = case filter (\(e,_) -> targetPlatformTryMatch eguard e) (reverse (sort xs)) of
      ((_,x):_) -> Just x
      _         -> Nothing

targetPlatformTryMatch :: TargetPlatform -> TargetPlatform -> Bool
targetPlatformTryMatch tpl1@(TargetPlatform target1 os1 arch1 p1) tpl2@(TargetPlatform target2 os2 arch2 p2)
  = let match = matchTarget target1 target2 && matchString os1 os2 && matchString arch1 arch2 && matchPlatform p1 p2
    in -- trace ("try match: " ++ show (tpl1,tpl2) ++ " == " ++ show match) $ 
       match    
  where
    matchTarget t1 t2       
      = case (t1,t2) of
          (Default,_)           -> True
          (_,Default)           -> True
          (C CDefault, C _)     -> True
          (JS JsDefault, JS _)  -> True
          (_,_)                 -> t1 == t2

    matchPlatform (Platform i1 i2 i3 i4) (Platform j1 j2 j3 j4)
      = matchInt i1 j1 && matchInt i2 j2 && matchInt i3 j3 && matchInt i4 j4

    matchString "" s2  = True
    matchString s1 ""  = True
    matchString s1 s2  = (s1==s2)

    matchInt 0 i2      = True
    matchInt i1 0      = True
    matchInt i1 i2     = (i1==i2)


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
                  OpExcept -> "final ctl"
                  OpControl -> "ctl"
                  OpControlRaw -> "raw ctl"
                  OpControlErr -> ""

-- Cannot have `-` or ` ` in the name
opSortString :: OperationSort -> String
opSortString opsort
  = case opsort of
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
      "rawctl" -> Just OpControlRaw
      -- legacy
      "rawctl" -> Just OpControlRaw
      "except" -> Just OpExcept
      "control"  -> Just OpControl
      "rcontrol" -> Just OpControlRaw
      _ -> Nothing


data DataEffect = DataNoEffect
                | DataEffect{ dataEffectIsNamed  :: !Bool
                            , dataEffectIsLinear :: !Bool
                            }
                deriving (Show)


{--------------------------------------------------------------------------
  DataKind
--------------------------------------------------------------------------}
data DataKind = Inductive | CoInductive | Retractive
              deriving (Eq)

instance Show DataKind where
  show Inductive = "type"
  show CoInductive = "co type"
  show Retractive = "div type"

data DataDef = DataDefValue !ValueRepr  -- value type
             | DataDefNormal            -- reference type
             | DataDefLazy !Fip         -- lazy reference type
             -- | DataDefEffect{ dataDefIsLinear :: !Bool, dataDefIsNamed :: !Bool }  -- effect types
             | DataDefOpen{ isExtend :: !Bool }
             | DataDefAuto{ dataDefDeclaredAsStruct :: !Bool }  -- value or reference type: determined by kind inference in (Kind/Repr.hs:createDataDef)
             deriving Eq

instance Show DataDef where
  show dd = case dd of
              DataDefValue v       -> "value" ++ show v
              DataDefNormal        -> "reference"
              DataDefLazy fip      -> sepBySpace ["lazy",show fip]
              DataDefOpen isExtend -> if isExtend then "extend" else "open"
              DataDefAuto isStruct -> "auto" ++ (if isStruct then " struct" else "")
              -- DataDefEffect linear named -> (if linear then "linear " else "") ++ (if named then "named " else "") ++ "effect"

dataDefIsExtend :: DataDef -> Bool
dataDefIsExtend ddef
  = case ddef of
      DataDefOpen isExtend -> isExtend
      _  -> False

dataDefIsOpen ddef
  = case ddef of
      DataDefOpen{} -> True
      _ -> False

dataDefIsValue ddef
  = case ddef of
      DataDefValue{} -> True
      _ -> False

dataDefIsNormal ddef
  = case ddef of
      DataDefNormal   -> True
      _ -> False

dataDefIsLazy ddef
  = case ddef of
      DataDefLazy _ -> True
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
  = DefFun { defFunParamInfos :: ![ParamInfo],
             defFunFip        :: !Fip }
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
      DefFun pinfos fip -> sepBySpace [show fip,"fun"]
      DefVal      -> "val"
      DefVar      -> "var"


instance Show DefSort where
  show ds = case ds of
              DefFun{} -> "fun"
              DefVal   -> "val"
              DefVar   -> "var"


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
data Fixity = FixInfix  !Int !Assoc -- ^ precedence and associativity
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

data Fip = Fip   { fipAlloc_ :: !FipAlloc }
         | Fbip  { fipAlloc_ :: !FipAlloc, fipTail :: !Bool }
         | NoFip { fipTail :: !Bool }
         deriving (Eq)

-- | Note that "fbip" and "fip(1)" are incomparable, so this can not be an Ord instance
fipSubsumes :: Fip -> Fip -> Bool
fipSubsumes (NoFip _) (Fip _) = True
fipSubsumes (NoFip t1) f2 = not t1 || fipTail f2
fipSubsumes (Fbip a1 t1) (Fbip a2 t2) = a1 >= a2 && (not t1 || t2)
fipSubsumes (Fbip a1 _) (Fip a2) = a1 >= a2
fipSubsumes (Fip a1) (Fip a2) = a1 >= a2
fipSubsumes _ _ = False

data FipAlloc = AllocAtMost !Int | AllocFinitely | AllocUnlimited
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

noFip, fipNoAlloc :: Fip
noFip = fipTop
fipNoAlloc = fipBot

isNoFip (NoFip _) = True
isNoFip _         = False

fipBot     = Fip (AllocAtMost 0)
fipTop     = NoFip False

isFipTop (NoFip False) = True
isFipTop _             = False


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
                Fbip n t    -> sepBySpace [showTail t,"fbip" ++ showN n]
                NoFip t     -> showTail t
            where
              showN (AllocAtMost 0) = ""
              showN (AllocAtMost n) = "(" ++ show n ++ ")"
              showN AllocFinitely   = "(n)"
              showN AllocUnlimited  = ""

              showTail True  = "tail"
              showTail _     = ""

sepBySpace :: [String] -> String
sepBySpace xs
  = concat $ intersperse " " $ filter (not . null) xs

fipMax :: Fip -> Fip -> Fip
fipMax fip1 fip2
  = case (fip1,fip2) of
      (NoFip isTail1, NoFip isTail2) -> NoFip (isTail1 && isTail2)
      (NoFip t1, _)                  -> NoFip (t1 && fipIsTail fip2)
      (_,NoFip t2)                   -> NoFip (fipIsTail fip1 && t2)

      (Fbip a1 t1, Fbip a2 t2)       -> Fbip (fipAllocMax a1 a2) (t1 && t2)
      (Fbip a1 t1, Fip a2)           -> Fbip (fipAllocMax a1 a2) t1
      (Fip a1, Fbip a2 t2)           -> Fbip (fipAllocMax a1 a2) t2

      (Fip a1, Fip a2)               -> Fip (fipAllocMax a1 a2)

fipAllocMax :: FipAlloc -> FipAlloc -> FipAlloc
fipAllocMax a1 a2
  = case (a1,a2) of
      (AllocUnlimited,_)     -> AllocUnlimited
      (_,AllocUnlimited)     -> AllocUnlimited
      (AllocFinitely,_)      -> AllocFinitely
      (_,AllocFinitely)      -> AllocFinitely
      (AllocAtMost n1,AllocAtMost n2) -> AllocAtMost (max n1 n2)


memberDoc :: String -> String -> [String] -> String
memberDoc doc header []  = doc
memberDoc doc header members
  = if null doc then mdoc else doc ++ "\n// * * *\n" ++ mdoc
  where
    mdoc = "// " ++ header ++ ":\n// ```koka\n" ++
           unlines (map ("// "++) members) ++
           "// ```\n"
