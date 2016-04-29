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

import Common.NamePrim( nameEffectOpen )
import Type.Type
import Type.TypeVar
import Core.Core

class Simplify a where
  simplify :: a -> a

{--------------------------------------------------------------------------
  Top-down optimizations 

  These optimizations must be careful to call simplify recursively 
  when necessary.
--------------------------------------------------------------------------}

topDown :: Expr -> Expr

-- Inline simple let-definitions
{-
topDown expr@(Let (DefGroups [DefNonRec (Def x tp e)]) e')
  = simplify $ [(x, e)] |~> e'
-}

-- Remove effect open applications
{-
topDown (App (TypeApp (Var openName _) _) [arg])  | getName openName == nameEffectOpen
  = topDown arg
-}

-- No optimization applies
topDown expr
  = expr

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
