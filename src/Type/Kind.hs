-----------------------------------------------------------------------------
-- Copyright 2012-2021, Microsoft Research, Daan Leijen.
--
-- This is free software; you can redistribute it and/or modify it under the
-- terms of the Apache License, Version 2.0. A copy of the License can be
-- found in the LICENSE file at the root of this distribution.
-----------------------------------------------------------------------------
module Type.Kind ( HasKind( getKind )
                 , handledToLabel
                 , HandledSort(..)
                 , getHandledEffect, getHandledEffectX, isHandledEffect
                 , containsHandledEffect
                 , extractHandledEffect
                 , labelIsLinear
                 , effectIsAffine
                 ) where

import Data.Maybe( isJust )
import Common.NamePrim( nameTpHandled, nameTpHandled1, nameTpPartial, nameTpScope )
import Common.Failure( failure )
import Kind.Kind
import Type.Type


effectIsAffine :: Effect -> Bool
effectIsAffine eff
  = let (labs,tl) = extractOrderedEffect eff
    in (isEffectEmpty tl && all labelIsAffine labs)


labelIsLinear :: Effect -> Bool
labelIsLinear effect
  = case expandSyn effect of
      TApp (TCon tc) [hx] | (typeConName tc == nameTpHandled1)
        -> True
      TApp (TCon tc) [hx] | typeConName tc /= nameTpHandled
        -- allow `alloc<global>` etc.
        -> let k = getKind effect
           in (isKindLabel k || isKindEffect k)
      TCon tc@(TypeCon cname _)  -- builtin effects?
        -> let k = getKind tc
           in (isKindLabel k || isKindEffect k)  -- too liberal? (does not include exn)
      _ -> False

labelIsAffine :: Effect -> Bool
labelIsAffine effect
  = case expandSyn effect of
      TApp (TCon tc) [hx] | (typeConName tc == nameTpHandled1)
        -> True
      TApp (TCon tc) [TCon tcx] | (typeConName tc == nameTpHandled && typeConName tcx == nameTpPartial)
        -- allow exn
        -> True
      TApp (TCon tc) [hx] | typeConName tc /= nameTpHandled
        -- allow `alloc<global>` etc.
        -> let k = getKind effect
           in (isKindLabel k || isKindEffect k)
      TCon _   -- builtin effects
        -> True
      _ -> False

extractHandledEffect eff
  = let (ls,tl) = extractOrderedEffect eff
    in (filter isHandledEffect ls, tl)


handledToLabel :: Type -> Type
handledToLabel e
  = if (isKindHandled1 (getKind e))
     then TApp tconHandled1 [e]
     else TApp tconHandled [e]

containsHandledEffect :: [Name] -> Effect -> Bool
containsHandledEffect exclude eff
  = let (ls,_) = extractEffectExtend eff
    in any (isJust . getHandledEffectX exclude) ls

data HandledSort = ResumeOnce | ResumeMany
                 deriving (Eq,Show)

isHandledEffect :: Type -> Bool
isHandledEffect tp -- isJust (getHandledEffect tp)
  = case expandSyn tp of
      TApp (TCon (TypeCon name _)) [t]
        -> (name == nameTpHandled || name == nameTpHandled1)
      _ -> False


getHandledEffect :: Type -> Maybe (HandledSort,Name)
getHandledEffect tp
  = getHandledEffectX [] tp

getHandledEffectX exclude tp
  = case expandSyn tp of
      TApp (TCon (TypeCon name _)) [t]
        | name == nameTpHandled  -> getHandledEffectX exclude t
        | name == nameTpHandled1 -> getHandledEffectX exclude t
      TApp (TCon (TypeCon hxName _)) _
        | isKindHandled (getKind tp)  && not (hxName `elem` exclude) -> Just (ResumeMany,hxName)
        | isKindHandled1 (getKind tp) && not (hxName `elem` exclude) -> Just (ResumeOnce,hxName)
        -- For named & scoped handlers
        -- TODO: use scope and scope1 to support linear named + scoped effect handlers. 
        | hxName == nameTpScope  && not (hxName `elem` exclude) -> Just (ResumeMany,hxName)
      TCon (TypeCon hxName kind)
        | isKindHandled kind  && not (hxName `elem` exclude) -> Just (ResumeMany,hxName)
        | isKindHandled1 kind && not (hxName `elem` exclude) -> Just (ResumeOnce,hxName)
      _ -> Nothing

{--------------------------------------------------------------------------
  Get the kind of a type.
--------------------------------------------------------------------------}
class HasKind a where
  -- | Return the kind of type
  getKind :: a -> Kind


instance HasKind Kind where
  getKind k = k

instance HasKind TypeVar where
  getKind (TypeVar id k _) = k

instance HasKind TypeCon where
  getKind (TypeCon id k) = k


instance HasKind TypeSyn where
  getKind (TypeSyn id k rank _) = k

instance HasKind Type where
  getKind tau
    = case tau of
        TForall _ _ tp -> getKind tp
        TFun _ _ _     -> kindStar
        TVar v         -> getKind v
        TCon c         -> getKind c
        TSyn syn xs tp -> -- getKind tp {- this is wrong for partially applied type synonym arguments, see "kind/alias3" test -}
                          -- if (null xs) then getKind tp else
                          kindApply xs (getKind syn)
        TApp tp args   -> kindApply args (getKind tp)
                          {- case collect [] (getKind tp) of
                            (kres:_) -> kres
                            _  -> failure ("Type.Kind: illegal kind in type application? " ++ show (getKind tp) )
                          -}
    where
      collect :: [Kind] -> Kind -> [Kind]
      collect acc (KApp (KApp arr k1) k2) | arr == kindArrow  = collect (k1:acc) k2
      collect acc k  = k:acc

      kindApply [] k   = k
      kindApply (_:rest) (KApp (KApp arr k1) k2)  = kindApply rest k2
      kindApply _  k   = failure ("Type.Kind.kindApply: illegal kind in application? " ++ show k)
