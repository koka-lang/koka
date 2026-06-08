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
                    , targetPlatformDefault, targetPlatformIsDefault
                    , targetPlatformFromString, targetFromBackend, targetFromHost, platformFromString
                    , matchTargetPlatform, matchTarget, matchOS, matchArch, matchPlatform
                    , targetPlatformIds
                    , unsupportedExternal
                    -- , targetPlatformTryMatch
                    ) where
import Debug.Trace
import Data.Tuple(swap)
import Data.Maybe(catMaybes)
import Data.List(intersperse,sort,intercalate,isPrefixOf)
import Common.File(splitOn)

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


unsupportedExternal :: String -> String
unsupportedExternal fname
  = trace ("warning: unsupported external: " ++ fname) $
    "kk_unsupported_external(" ++ show fname ++ ")"

instance Show Target where
  show tgt = case tgt of
                C CDefault-> "c"
                C LibC    -> "libc"
                C Wasm    -> "wasm"
                C WasmJs  -> "wasmjs"
                C WasmWeb -> "wasmweb"
                JS JsNode -> "jsnode"
                JS JsWeb  -> "jsweb"
                JS JsDefault -> "js"
                CS        -> "cs"
                Default   -> "default"


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


targetFromHost :: String -> Maybe Target
targetFromHost s
  = lookup s hostIds

hostIds :: [(String,Target)]
hostIds = [
  ("libc",C LibC),
  ("wasm",C Wasm),
  ("wasmjs",C WasmJs),
  ("wasmweb",C WasmWeb),
  ("jsnode",JS JsNode),
  ("jsweb",JS JsWeb)
  ]

targetFromBackend :: String -> Maybe Target
targetFromBackend s
  = lookup s backendIds

backendIds :: [(String,Target)]
backendIds = [
  ("c",C CDefault),
  ("js", JS JsDefault),
  ("cs",CS),
  ("default",Default)
  ]

platformFromString :: String -> Maybe Platform
platformFromString s
  = lookup s platformIds

platformIds :: [(String,Platform)]
platformIds = [
  ("32",platform32), ("p32",platform32),
  ("64",platform64), ("p64",platform64),
  ("64c",platform64c), ("p64c",platform64c),
  ("js",platformJS), ("pjs",platformJS),
  ("cs",platformCS), ("pcs",platformCS),
  ("none",platformNone)      
  ]

targetPlatformFromString :: String -> Maybe TargetPlatform
targetPlatformFromString s
  = lookup s targetPlatformIds
  
targetPlatformIds :: [(String,TargetPlatform)]
targetPlatformIds = [
  ("c",      targetPlatformDefault{ tplTarget=C LibC, tplPlatform=platform64 }),
  ("c64",    targetPlatformDefault{ tplTarget=C LibC, tplPlatform=platform64 }),
  ("c32",    targetPlatformDefault{ tplTarget=C LibC, tplPlatform=platform32 }),
  ("c64c",   targetPlatformDefault{ tplTarget=C LibC, tplPlatform=platform64c }),
  ("js",     targetPlatformDefault{ tplTarget=JS JsNode, tplPlatform=platformJS }),
  ("jsnode", targetPlatformDefault{ tplTarget=JS JsNode, tplPlatform=platformJS }),
  ("jsweb",  targetPlatformDefault{ tplTarget=JS JsWeb, tplPlatform=platformJS }),
  ("wasm",   targetPlatformDefault{ tplTarget=C Wasm, tplPlatform=platform32 }),
  ("wasm32", targetPlatformDefault{ tplTarget=C Wasm, tplPlatform=platform32 }),
  ("wasm64", targetPlatformDefault{ tplTarget=C Wasm, tplPlatform=platform64 }),
  ("wasmjs", targetPlatformDefault{ tplTarget=C WasmJs, tplPlatform=platform32 }),
  ("wasmweb",targetPlatformDefault{ tplTarget=C WasmWeb, tplPlatform=platform32 }),
  ("cs",     targetPlatformDefault{ tplTarget=CS, tplPlatform=platformCS })
  ]

matchTargetPlatform :: TargetPlatform -> TargetPlatform -> Bool
matchTargetPlatform (TargetPlatform b1 os1 arch1 pl1) (TargetPlatform b2 os2 arch2 pl2)
  = matchTarget b1 b2 && matchStr os1 os2 && matchStr arch1 arch2 && matchPlatform pl1 pl2

matchOS :: String -> String -> Bool
matchOS = matchStr

matchArch :: String -> String -> Bool
matchArch = matchStr

matchStr :: String -> String -> Bool
matchStr "" _ = True
matchStr s1 s2  = let ss1 = splitOn (\c -> c == '-') s1
                      ss2 = splitOn (\c -> c == '-') s2
                  in ss1 `isPrefixOf` ss2

matchPlatform :: Platform -> Platform -> Bool
matchPlatform (Platform i1 i2 i3 i4) (Platform j1 j2 j3 j4)
  = matchInt i1 j1 && matchInt i2 j2 && matchInt i3 j3 && matchInt i4 j4

matchInt 0 i2      = True
matchInt i1 i2     = (i1==i2)

matchTarget :: Target -> Target -> Bool
matchTarget t1 t2      
  = case (t1,t2) of
      (Default,_)           -> True
      (C CDefault, C _)     -> True
      (JS JsDefault, JS _)  -> True
      (_,_)                 -> t1 == t2

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
