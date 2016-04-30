-----------------------------------------------------------------------------
-- Copyright 2012 Microsoft Corporation.
--
-- This is free software; you can redistribute it and/or modify it under the
-- terms of the Apache License, Version 2.0. A copy of the License can be
-- found in the file "license.txt" at the root of this distribution.
-----------------------------------------------------------------------------
{-    Core simplification 
-}
-----------------------------------------------------------------------------

module Core.Simplify (simplify) where

import Common.Range
import Common.Syntax
import Common.NamePrim( nameEffectOpen )
import Type.Type
import Type.TypeVar
import Core.Core
import qualified Common.NameMap as M
import qualified Data.Set as S

-- data Env = Env{ inlineMap :: M.NameMap Expr }
-- data Info = Info{ occurrences :: M.NameMap Int }

class Simplify a where
  simplify :: a -> a

{--------------------------------------------------------------------------
  Top-down optimizations 

  These optimizations must be careful to call simplify recursively 
  when necessary.
--------------------------------------------------------------------------}

topDown :: Expr -> Expr

-- Inline simple let-definitions
topDown expr@(Let (DefNonRec (Def{defName=x,defType=tp,defExpr=e}):dgs) body) 
  | isCheap e || (isTotal e && occursAtMostOnce x body)
  = simplify $ [((TName x tp), e)] |~> Let dgs body

topDown expr@(Let (dg:dgs) body) 
  = let expr' = topDown (Let dgs body)
    in case expr' of
         Let dgs' body' -> Let (simplify dg : dgs') body'
         _ -> Let [simplify dg] expr'


-- Remove effect open applications
{-
topDown (App (TypeApp (Var openName _) _) [arg])  | getName openName == nameEffectOpen
  = topDown arg
-}

-- Direct function applications
topDown (App (Lam pars eff body) args) | length pars == length args
  = simplify $ Let (zipWith makeDef pars args) body
  where
    makeDef (TName par parTp) arg 
      = DefNonRec (Def par parTp arg Private DefVal rangeNull "") 

-- No optimization applies
topDown expr
  = expr

isCheap :: Expr -> Bool
isCheap expr
  = case expr of
      Var{} -> True
      Con{} -> True
      Lit{} -> True
      TypeLam _ body -> isCheap body
      TypeApp body _ -> isCheap body
      _     -> False
{-
isTotal :: Expr -> Bool
isTotal expr
  = case expr of
      Var{} -> True
      Con{} -> True
      Lit{} -> True
      Lam{} -> True
      TypeLam _ body -> isCheap body
      TypeApp body _ -> isCheap body
      _ -> False
-}
occursAtMostOnce :: Name -> Expr -> Bool
occursAtMostOnce name expr
  = case M.lookup name (occurrences expr) of
      Nothing -> True
      Just i  -> i<=1


occurrences :: Expr -> M.NameMap Int
occurrences expr
  = case expr of
      Var v _ -> M.singleton (getName v) 1
      Con{} -> M.empty
      Lit{} -> M.empty
      App f args
        -> ounions (occurrences f : map occurrences args)
      Lam pars eff body 
        -> foldr M.delete (occurrences body) (map getName pars)
      TypeLam _ body -> occurrences body
      TypeApp body _ -> occurrences body
      Let dgs body -> foldr occurrencesDefGroup (occurrences body) dgs
      Case scruts bs -> ounions (map occurrences scruts ++ map occurrencesBranch bs)

occurrencesBranch :: Branch -> M.NameMap Int
occurrencesBranch (Branch pat guards)
  = foldr M.delete (ounions (map occurrencesGuard guards)) (map getName (S.elems (bv pat)))

occurrencesGuard (Guard g e)
  = ounion (occurrences g) (occurrences e) 

ounion :: M.NameMap Int -> M.NameMap Int -> M.NameMap Int
ounion oc1 oc2
  = M.unionWith (+) oc1 oc2

ounions :: [M.NameMap Int] -> M.NameMap Int
ounions ocs
  = M.unionsWith (+) ocs

occurrencesDefGroup :: DefGroup -> M.NameMap Int -> M.NameMap Int
occurrencesDefGroup dg oc
  = case dg of
      DefNonRec def -> ounion (M.delete (defName def) oc) (occurrences (defExpr def))
      DefRec defs   -> foldr M.delete (ounions (oc : map (occurrences . defExpr) defs)) 
                                      (map defName defs)

{--------------------------------------------------------------------------
  Bottom-up optimizations 

  These optimizations can assume their children have already been simplified.
--------------------------------------------------------------------------}

bottomUp :: Expr -> Expr


-- replace "(/\a. body) t1" with "body[a |-> t1]"
bottomUp expr@(TypeApp (TypeLam tvs body) tps) 
  = if (length tvs == length tps)
     then let sub = subNew (zip tvs tps)
          in sub |-> body
     else expr

-- eta-contract "/\a. (body a)" to "body"
bottomUp expr@(TypeLam tvs (TypeApp body tps))
  = if (length tvs == length tps && all varEqual (zip tvs tps) && all (\tv -> not (tvsMember tv (ftv body))) tvs)
     then body
     else expr
  where
    varEqual (tv,TVar tw) = tv == tw
    varEqual _            = False

-- No optimization applies
bottomUp expr
  = expr

{--------------------------------------------------------------------------
  Definitions 
--------------------------------------------------------------------------}

instance Simplify DefGroup where
  simplify (DefRec    defs) = DefRec    (simplify defs)
  simplify (DefNonRec def ) = DefNonRec (simplify def)

instance Simplify Def where
  simplify (Def name tp expr vis isVal nameRng doc) = Def name tp (simplify expr) vis isVal nameRng doc

instance Simplify a => Simplify [a] where
  simplify = map simplify

{--------------------------------------------------------------------------
  Expressions 
--------------------------------------------------------------------------}

instance Simplify Expr where
  simplify e 
    = bottomUp $
      case topDown e of
        Lam tnames eff expr-> Lam tnames eff (simplify expr)
        Var tname info     -> Var tname info
        App e1 e2          -> App (simplify e1) (simplify e2)
        TypeLam tv expr    -> TypeLam tv (simplify expr)
        TypeApp expr tp    -> TypeApp (simplify expr) tp
        Con tname repr     -> Con tname repr
        Lit lit            -> Lit lit
        Let defGroups expr -> Let (simplify defGroups) (simplify expr)
        Case exprs branches-> Case (simplify exprs) (simplify branches) 

instance Simplify Branch where
  simplify (Branch patterns guards) = Branch patterns (map simplify guards)

instance Simplify Guard where
  simplify (Guard test expr) = Guard (simplify test) (simplify expr)
