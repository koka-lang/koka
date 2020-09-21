-----------------------------------------------------------------------------
-- Copyright 2012 Microsoft Corporation.
--
-- This is free software; you can redistribute it and/or modify it under the
-- terms of the Apache License, Version 2.0. A copy of the License can be
-- found in the file "license.txt" at the root of this distribution.
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
                    , DefInline(..)
                    , Target(..)
                    , Host(..)
                    , isPublic, isPrivate
                    , DataDef(..)
                    , dataDefIsRec, dataDefIsOpen, dataDefIsValue
                    , HandlerSort(..)
                    , isHandlerInstance, isHandlerNormal
                    , OperationSort(..), readOperationSort
                    , Platform(..), platform32, platform64, platformCS, platformJS
                    , alignedSum, alignedAdd, alignUp
                    ) where

{--------------------------------------------------------------------------
  Backend targets
--------------------------------------------------------------------------}
data Target = CS | JS | C | CHeader | Default deriving (Eq,Ord)

instance Show Target where
  show CS = "cs"
  show JS = "js"
  show C  = "c"
  show CHeader = "c header"
  show Default = ""

data Host = Node | Browser deriving (Eq,Ord)

instance Show Host where
  show Node    = "node"
  show Browser = "browser"


data Platform = Platform{ sizePtr  :: Int -- sizeof(intptr_t)
                        , sizeSize :: Int -- sizeof(size_t)
                        }

platform32, platform64 :: Platform
platform32 = Platform 4 4
platform64 = Platform 8 8 
platformJS = Platform 8 4
platformCS = Platform 8 4

instance Show Platform where
  show (Platform sp ss) = "Platform(sizeof(void*)=" ++ show sp ++ ",sizeof(size_t)=" ++ show ss ++ ")"

alignedSum :: Int -> [Int] -> Int
alignedSum start xs = foldl alignedAdd start xs
     
alignedAdd :: Int -> Int -> Int
alignedAdd x y = (alignUp x y) + y
     
alignUp :: Int -> Int -> Int
alignUp x y  | y <= 0  = x
alignUp x y  = ((x + y - 1) `div` y)*y    


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
                 HandlerInstance -> "instance"

isHandlerInstance (HandlerInstance) = True
isHandlerInstance _ = False

isHandlerNormal (HandlerNormal) = True
isHandlerNormal _ = False


data OperationSort 
  = OpVal | OpFun | OpControlRaw | OpControl
  deriving (Eq,Ord)
  
instance Show OperationSort where
  show opsort = case opsort of
                  OpVal -> "val"
                  OpFun -> "fun"
                  OpControlRaw -> "rcontrol"
                  OpControl -> "control"
  
readOperationSort :: String -> Maybe OperationSort
readOperationSort s 
  = case s of 
      "val" -> Just OpVal
      "fun" -> Just OpFun
      "rcontrol" -> Just OpControlRaw
      "control"  -> Just OpControl
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

data DataDef = DataDefValue{ rawFields :: Int, scanFields :: Int }
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
  = DefFun | DefVal | DefVar
  deriving (Eq,Ord)

isDefFun (DefFun )  = True
isDefFun _          = False

defFun :: DefSort
defFun = DefFun

instance Show DefSort where
  show ds = case ds of
              DefFun -> "fun"
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
