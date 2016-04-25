------------------------------------------------------------------------------
-- Copyright 2012 Microsoft Corporation.
--
-- This is free software; you can redistribute it and/or modify it under the
-- terms of the Apache License, Version 2.0. A copy of the License can be
-- found in the file "license.txt" at the root of this distribution.
-----------------------------------------------------------------------------
{-
    Definition of kinds and helper functions.
-}
-----------------------------------------------------------------------------
module Kind.Kind( -- * Kinds
                    Kind(..)
                  , KindCon
                 -- * Standard kinds
                  , kindStar, kindPred, kindEffect, kindArrow, kindHeap, kindHandled
                  , kindFun, kindArrowN, kindLabel, extractKindFun
                  , builtinKinds
                  , isKindFun
                  , isKindStar
                  , isKindEffect, isKindHandled
                  ) where

import Common.Name
import Common.NamePrim

{--------------------------------------------------------------------------
  Kinds
--------------------------------------------------------------------------}
-- | Kinds
data Kind
  = KCon     KindCon        -- ^ Kind constants: "*","->","!","H","P"
  | KApp     Kind Kind      -- ^ Application (only allowed for functions as yet)
  deriving (Eq,Ord, Show)

-- | Kind constant
type KindCon  = Name

-- | Kind and Type variables come in three flavours: 'Unifiable'
-- variables can be unified, 'Skolem's are non-unifiable (fresh)
-- variables, and 'Bound' variables are bound by a quantifier.
data Flavour  = Bound 
              | Skolem
              | Meta    -- used for pretty printing
              deriving(Eq, Show)



{--------------------------------------------------------------------------
  Standard kinds
--------------------------------------------------------------------------}
-- | Kind @*@
kindStar :: Kind
kindStar
  = KCon nameKindStar

-- | Kind @Label@
kindLabel :: Kind
kindLabel
  = KCon nameKindLabel

-- | Kind arrow @->@
kindArrow :: Kind
kindArrow
  = KCon nameKindFun

kindArrowN :: Int -> Kind
kindArrowN n
  = foldr kindFun (kindFun kindEffect kindStar) (replicate n kindStar)

kindPred :: Kind
kindPred
  = KCon nameKindPred

kindEffect :: Kind
kindEffect
  = KCon nameKindEffect

kindHeap :: Kind
kindHeap
  = KCon nameKindHeap

kindHandled :: Kind
kindHandled
  = KCon nameKindHandled

kindExtend :: Kind 
kindExtend 
  = kindFun kindLabel (kindFun kindEffect kindEffect)

-- | Create a (kind) function from a kind to another kind.
kindFun :: Kind -> Kind -> Kind
kindFun k1 k2
  = KApp (KApp kindArrow k1) k2

isKindFun :: Kind -> Bool
isKindFun k
  = case k of
      KApp (KApp k0 k1) k2  -> k0 == kindArrow
      _ -> False
      

extractKindFun :: Kind -> ([Kind],Kind)
extractKindFun k
  = case k of
      KApp (KApp k0 k1) k2 | k0 == kindArrow
        -> let (args,res) = extractKindFun k2
           in (k1:args,res)
      _ -> ([],k)

isKindStar, isKindEffect, isKindHandled :: Kind -> Bool
isKindStar k
  = k == kindStar
isKindEffect k
  = k == kindEffect
isKindHandled k
  = k == kindHandled

-- | Standard kind constants with their kind.
builtinKinds :: [(Name,Kind)]
builtinKinds
  = [(nameKindStar, kindStar)
    ,(nameKindFun, kindArrow)
    ,(nameKindPred, kindPred)
    ,(nameKindEffect, kindEffect)
    ,(nameKindLabel, kindLabel)
    ,(nameKindHeap, kindHeap)
    ,(nameKindHandled, kindHandled)
    ]
