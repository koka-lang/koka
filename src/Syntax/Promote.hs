------------------------------------------------------------------------------
-- Copyright 2012 Microsoft Corporation.
--
-- This is free software; you can redistribute it and/or modify it under the
-- terms of the Apache License, Version 2.0. A copy of the License can be
-- found in the file "license.txt" at the root of this distribution.
-----------------------------------------------------------------------------
{-
    Peform implicit promotions:

    * Quantify over free type variables in type signatures of definitions.

    * Change unbound, unparenthesized type variables that only occur in label positions
      to label constants.

    * Change unparenthesized variables in label positions to label constants.
      (Or should that be: /
      Change unbound unparenthesized variables that only occur in label positions to
      label constants./)
-}
-----------------------------------------------------------------------------

module Syntax.Promote( promote, promoteType, quantify, promoteFree ) where

import Data.List( partition )
import Common.NamePrim( nameTpOptional, nameEffectEmpty, nameEffectExtend )
import Common.Name
import Common.Range
import qualified Common.NameSet as S
import Syntax.Syntax

promoteType :: UserType -> UserType
promoteType tp
  = let free = freeTypeVars tp
    in if (S.null free)
        then tp
        else let (someVars,forallVars) = partition isSomeVar (S.toList free)
             in quantify QSome (map toTypeBinder someVars) $
                quantify QForall (map toTypeBinder forallVars) $
                tp

promoteFree :: [UserTypeBinder] -> [UserType] -> [UserTypeBinder]
promoteFree bound tps
  = map toTypeBinder (S.toList (S.difference (freeTypeVars tps) (S.fromList (map tbinderName bound))))

{--------------------------------------------------------------------------
  Type variable promotion
  Take annotations on parameters and result type, and transform it into
  a single type annotation using some and forall qualifiers.

  ie.

  function some(a) forall(b) foo( x : a, y : b, z ) : c { .. }

  ~> 

  foo : some(a,a1) forall(b,c) (a,b,a1) -> total c  = fun(x,y,z){ .. }
--------------------------------------------------------------------------}

promote :: [TypeBinder UserKind] -> [TypeBinder UserKind] -> Maybe (Maybe UserType,UserType) -> UserExpr -> UserExpr
promote somePars forallPars mbResTp expr
  = let (argresTps,expr') = argresTypes expr
    in if (all (isRight . snd) argresTps)
        then -- no annotation, no promotion
             expr
        else promoteEx somePars forallPars argresTps expr'
  where 
    isRight (Right rng) = True
    isRight (Left tp)   = False

    argresTypes :: UserExpr -> ([(Name,Either UserType Range)],UserExpr)
    argresTypes (Parens expr r)     = let (es,expr') = argresTypes expr in (es,Parens expr' r)
    argresTypes (Ann expr tp r)     = let (es,expr') = argresTypes expr in (es,Ann expr' tp r)
    argresTypes (Lam args expr rng) = let (es,expr') = resType expr 
                                          (fs,args') = unzip (map (\binder
                                                                      -> case binderType binder of
                                                                          Nothing -> ((binderName binder, Right rng), binder) 
                                                                          Just tp -> 
                                                                            let optTp = case binderExpr binder of 
                                                                                          Nothing -> tp
                                                                                          Just _  -> TpApp (TpCon nameTpOptional (getRange tp)) [tp] (getRange tp)
                                                                            in ((binderName binder, Left optTp), binder{binderType = Nothing})) args)
                                      in (fs ++ es, Lam args' expr' rng)

    argresTypes expr                = ([],expr)

    resType :: Expr UserType -> ([(Name,Either UserType Range)],Expr UserType)
    resType (Parens expr r)     = let (es,expr') = resType expr in (es,Parens expr' r)
    {-
    resType (Ann expr tp rng)   = case mbResTp of 
                                    Just (teff,tres) -> ([Left teff, Left tres], Ann expr tp rng)
                                    Nothing          -> ([Right (getRange tp), Left tp], Ann expr tp rng)
    -}
    resType expr                = case mbResTp of
                                    Just (mbteff,tres) -> ([(nameNil,case mbteff of{ Nothing -> Right (getRange expr); Just teff -> Left teff }), (nameNil,Left tres)],expr)
                                    Nothing            -> ([(nameNil, Right (getRange expr)), (nameNil, Right (getRange expr))],expr)


promoteEx :: [TypeBinder UserKind] -> [TypeBinder UserKind] -> [(Name,Either UserType Range)] -> UserExpr -> UserExpr
promoteEx somePars forallPars argresTypes body
  = let -- promote quantified variables of TpCon to TpVar
        quantified   = S.fromList (map getName somePars) `S.union` S.fromList (map getName forallPars)
        argresTypes1 = map (promoteTVars quantified) argresTypes
        
        -- create full type by inserting some types if no parameter was annotated
        (namess,argresTypes2) = unzip (map insertSome (zip [1..] argresTypes1))
        (resType:effType:rargTypes) = reverse argresTypes2
        names = concat namess
        effect = {- extend -} (snd effType)
        funTp = TpFun (reverse rargTypes) effect (snd resType) (getRange body) -- TODO: better range?

        -- quantify over free type variables
        implicit = S.toList (S.difference (freeTypeVars funTp) (S.union quantified (S.fromList names)))

        -- create fully quantified type
        (impSome,impForall) = partition isSomeVar implicit
        fullTp = quantify QSome (somePars ++ (map toTypeBinder (names ++ impSome))) $
                 quantify QForall (forallPars ++ (map toTypeBinder impForall)) $
                 funTp
     in 
        Ann body fullTp (combineRanged body fullTp)

  where
    insertSome :: (Int,(Name,Either UserType Range)) -> ([Name],(Name,UserType))
    insertSome (i,(name,eitherTp))
      = case eitherTp of
          Left tp   -> ([],(name,tp))
          Right rng -> let tname = newImplicitTypeVarName i
                       in ([tname],(name,TpVar tname rng))

isSomeVar :: Name -> Bool
isSomeVar name
  = isWildcard name

extend :: UserType -> UserType
extend tp
  = let (ls,tl) = extract tp
    in case tl of
         TpCon name _  | name == nameEffectEmpty
           -> makeEffectExtends ls (TpVar (newName "_.e") rangeNull)
         TpCon _ _  | null ls
           -> makeEffectExtends [tp] (TpVar (newName "_.e") rangeNull)
         _ -> tp

makeEffectExtends ls tl
  = foldr makeEffectExtend tl ls
  where
    makeEffectExtend label ext
      = TpApp (TpCon nameEffectExtend (combineRanged (getRange label) ext)) [label,ext] (combineRanged (getRange label) ext)


extract :: UserType -> ([UserType],UserType)
extract tp
  = case tp of
      TpApp (TpCon name _) [lab,rest] _  | name == nameEffectExtend
        -> let (labs,tl) = extract rest
           in (lab:labs,tl)
      _ -> ([],tp)

quantify :: UserQuantifier -> [UserTypeBinder] -> UserType -> UserType
quantify quan tbinders tp
  = foldr (\tb t -> TpQuan quan tb t (combineRanged tb t)) tp tbinders

toTypeBinder :: Name -> UserTypeBinder
toTypeBinder name
  = TypeBinder name KindNone rangeNull rangeNull

promoteTVars :: S.NameSet -> (Name,Either UserType Range) -> (Name,Either UserType Range)
promoteTVars vars (name,Right rng) 
  = (name,Right rng)
promoteTVars vars (name,Left tp)
  = (name, Left (promoteTpVars vars tp))

promoteTpVars :: S.NameSet -> UserType -> UserType
promoteTpVars vars tp
  = case tp of
     TpQuan quant tb tp rng         -> TpQuan quant tb (promoteTpVars (S.delete (getName tb) vars) tp) rng
     TpQual     preds tp            -> TpQual (map (promoteTpVars vars) preds) (promoteTpVars vars tp)
     TpFun      args effect tp rng  -> TpFun [(name,promoteTpVars vars tp) | (name,tp) <- args] (promoteTpVars vars effect) (promoteTpVars vars tp) rng
     TpApp      tp args range       -> TpApp (promoteTpVars vars tp) (map (promoteTpVars vars) args) range
     TpVar      name range          -> TpVar name range
     TpCon      name rng            -> if (S.member name vars) then TpVar name rng else TpCon name rng
     TpParens   tp range            -> TpParens (promoteTpVars vars tp) range
     TpAnn      tp kind             -> TpAnn (promoteTpVars vars tp) kind
  
