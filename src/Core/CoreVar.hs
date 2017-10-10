  -----------------------------------------------------------------------------
-- Copyright 2012 Microsoft Corporation.
--
-- This is free software; you can redistribute it and/or modify it under the
-- terms of the Apache License, Version 2.0. A copy of the License can be
-- found in the file "license.txt" at the root of this distribution.
-----------------------------------------------------------------------------

module Core.CoreVar ( HasExprVar, (|~>)
                    , HasExpVar, fv, bv
                    , isTopLevel
                    ) where


import qualified Data.Set as S
import Data.Maybe
import Common.Name
import Common.Range
import Common.Failure
import Type.Type
import Type.Pretty ()
import Type.TypeVar


import Core.Core
import Core.Pretty


isTopLevel :: Def -> Bool
isTopLevel (Def name tp expr vis isVal nameRng doc)
  = let freeVar = filter (\(nm) -> not (isQualified nm) && nm /= unqualify name) (map getName (tnamesList (fv expr)))
        freeTVar = ftv expr
        yes = (null freeVar && tvsIsEmpty freeTVar)
    in -- trace ("isTopLevel " ++ show name ++ ": " ++ show (yes,freeVar,tvsList freeTVar)) $
        yes


{--------------------------------------------------------------------------
  Term-substitutions
--------------------------------------------------------------------------}

class HasExpVar a where
  -- Return free variables
  fv :: a -> TNames

  -- Top level bound variables
  bv :: a -> TNames


instance HasExpVar a => HasExpVar [a] where
  fv xs
    = S.unions (map fv xs)

  bv xs
    = S.unions (map bv xs)


instance HasExpVar DefGroup where
  fv defGroup
   = case defGroup of
      DefRec defs   -> fv defs `S.difference` bv defs
      DefNonRec def -> fv def

  bv defGroup
   = case defGroup of
      DefRec defs   -> bv defs
      DefNonRec def -> bv def

instance HasExpVar Def where
  fv (Def name tp expr vis isVal nameRng doc) = fv expr
  bv (Def name tp expr vis isVal nameRng doc) = S.singleton (TName name tp)

fvDefGroups defGroups expr
  = case defGroups of
      [] -> fv expr
      (dg:dgs) -> fv dg `S.union` (fvDefGroups dgs expr `S.difference` bv dg)

instance HasExpVar Expr where
  -- extract free variables from an expression
  fv (Lam tnames eff expr)= foldr S.delete (fv expr) tnames
  fv (Var tname info)     = S.singleton tname
  fv (App e1 e2)          = fv e1 `S.union` fv e2
  fv (TypeLam tyvar expr) = fv expr
  fv (TypeApp expr ty)    = fv expr
  fv (Con tname repr)     = S.empty
  fv (Lit i)              = S.empty
  fv (Let dfgrps expr)    = fvDefGroups dfgrps expr
  fv (Case exprs bs)      = fv exprs `S.union` fv bs

  bv exp                  = failure "Backend.CSharp.FromCore.bv on expr"

instance HasExpVar Branch where
  fv (Branch patterns guards) = fv guards `S.difference` bv patterns
  bv (Branch patterns guards) = bv patterns `S.union` bv guards

instance HasExpVar Guard where
  fv (Guard test expr) = fv test `S.union` fv expr
  bv (Guard test expr) = bv test `S.union` bv expr

instance HasExpVar Pattern where
  fv pat
    = S.empty
  bv pat
    = case pat of
        PatCon tname args _ _ _ _ _ -> bv args
        PatVar tname pat         -> S.union (S.singleton tname) (bv pat)
        PatWild                  -> S.empty
        PatLit lit               -> S.empty


{--------------------------------------------------------------------------
  Term-substitutions
--------------------------------------------------------------------------}

class HasExprVar a where
  (|~>) :: [(TName, Expr)] -> a -> a

instance HasExprVar a => HasExprVar [a] where
  sub |~> xs
    = map (sub |~>) xs

instance (HasExprVar a, HasExprVar b, HasExprVar c) => HasExprVar (a,b,c) where
  sub |~> (x,y,z)
    = (sub |~> x, sub |~> y, sub |~> z)

instance HasExprVar DefGroup where
  sub |~> defGroup
    = case defGroup of
        DefRec    defs -> DefRec (sub |~> defs)
        DefNonRec def  -> DefNonRec (sub |~> def)


instance HasExprVar Def where
  sub |~> (Def dname scheme expr vis isVal nameRng doc)
    = -- assertion "Core.HasExprVar.Def.|~>" (TName name scheme `notIn` sub) $
      let sub' = [(name,e) | (name,e) <- sub, getName name /= dname]
      in Def dname scheme (sub' |~> expr) vis isVal nameRng doc

instance HasExprVar Expr where
  sub |~> expr =
    case expr of
      Lam tnames eff expr  -> -- assertion "Core.HasExprVar.Expr.|~>" (all (\tname -> tname `notIn` sub) tnames) $
                              let sub' = [(name,e) | (name,e) <- sub, all (name /=) tnames]
                              in Lam tnames eff (sub' |~> expr)
      Var tname info       -> fromMaybe expr (lookup tname sub)
      App e1 e2            -> App (sub |~> e1) (sub |~> e2)
      TypeLam typeVars exp -> assertion ("Core.HasExprVar.Expr.|~>.TypeLam: " ++ show typeVars ++ ",\n " ++ show sub ++ "\n " ++ show expr) 
                                        (all (\tv -> not (tvsMember tv (ftv (map snd sub)))) typeVars
                                          || all (\name -> not (S.member name (fv exp) )) (map fst sub)) $
                              TypeLam typeVars (sub |~> exp)
      TypeApp expr tp      -> TypeApp (sub |~> expr) tp
      Con tname repr       -> expr
      Lit lit              -> expr
      Let defGroups expr   -> let defnames = map defName (flattenDefGroups defGroups)
                                  sub' = [(name,e) | (name,e) <- sub, all (getName name /=) defnames]
                              in Let (sub |~> defGroups) (sub' |~> expr)
      Case expr branches   -> Case (sub |~> expr) (sub |~> branches)


instance HasExprVar Branch where
  sub |~> (Branch patterns guards)
    = let bvpat = bv patterns
          sub' = [(name,expr) | (name,expr) <- sub, not (S.member (name ) bvpat)]
      in Branch patterns (map (sub' |~>) guards)

instance HasExprVar Guard where
  sub |~> (Guard test expr)
    = Guard (sub |~> test) (sub |~> expr)


notIn :: TName -> [(TName, Expr)] -> Bool
notIn name subst = not (name `elem` map fst subst)


