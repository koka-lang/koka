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

module Core.Simplify (simplify, simplifyDefs) where

import Lib.Trace
import Lib.PPrint
import Common.Range
import Common.Syntax
import Common.NamePrim( nameEffectOpen, nameToAny, nameEnsureK, nameReturn )
import Common.Unique
import Type.Type
import Type.TypeVar
import Type.Pretty
import Core.Core
import Core.Pretty
import qualified Common.NameMap as M
import qualified Data.Set as S

-- data Env = Env{ inlineMap :: M.NameMap Expr }
-- data Info = Info{ occurrences :: M.NameMap Int }

simplifyDefs :: Int -> DefGroups -> (DefGroups,Int)
simplifyDefs uniq defs
  = runUnique uniq (do defs1 <- simplify defs; simplify defs1)

class Simplify a where
  simplify :: a -> Unique a

{--------------------------------------------------------------------------
  Top-down optimizations 

  These optimizations must be careful to call simplify recursively 
  when necessary.
--------------------------------------------------------------------------}

topDown :: Expr -> Unique Expr

-- Inline simple let-definitions
topDown (Let dgs body)  
  = topDownLet [] [] dgs body
  where
    subst sub expr
      = if null sub then expr else (sub |~> expr)

    topDownLet :: [(TName,Expr)] -> [DefGroup] -> [DefGroup] -> Expr -> Unique Expr
    topDownLet sub acc [] body 
      = case subst sub body of 
          Let sdgs sbody -> topDownLet [] acc sdgs sbody  -- merge nested Let's
          sbody -> if (null acc) 
                    then topDown sbody 
                    else return $ Let (reverse acc) sbody

    topDownLet sub acc (dg:dgs) body
      = let sdg = subst sub dg
        in case sdg of 
          DefRec defs -> topDownLet sub (sdg:acc) dgs body -- don't inline recursive ones
          DefNonRec def@(Def{defName=x,defType=tp,defExpr=se})
            -> if (isTotalAndCheap se) 
                then -- inline very small expressions
                     topDownLet (extend (TName x tp, se) sub) acc dgs body
               else case extractFun se of
                Just (tpars,pars,_,_)  
                  | occursAtMostOnceApplied x (length tpars) (length pars) (Let dgs body) -- todo: exponential revisits of occurs
                  -> -- function that occurs once in the body and is fully applied; inline to expose more optimization
                     -- let f = \x -> x in f(2) ~> 2
                     topDownLet (extend (TName x tp, se) sub) acc dgs body
                _ | isTotal se && isSmall se && occursAtMostOnce x (Let dgs body) -- todo: exponential revisits of occurs
                  -> -- inline small total expressions
                     topDownLet (extend (TName x tp, se) sub) acc dgs body                     
                _ -> -- no inlining
                     topDownLet sub (sdg:acc) dgs body

    extend :: (TName,Expr) -> [(TName,Expr)] -> [(TName,Expr)]
    extend (name,e) sub
      = (name,e):sub                     

    extractFun expr
      = case expr of
          TypeLam tpars (Lam pars eff body) -> Just (tpars,pars,eff,body)
          Lam pars eff body                 -> Just ([],pars,eff,body)
          _ -> Nothing

    isSmall expr
      = isSmallX 3 expr -- at most 3 applications deep

    isSmallX n expr
      = if (n <= 0) then False
        else case expr of
          Var{} -> True
          Con{} -> True
          Lit{} -> True
          TypeLam _ e -> isSmallX n e
          TypeApp e _ -> isSmallX n e
          App (Var v _) _ | getName v == nameReturn -> False  -- and ensureK?
          App f args  -> all (isSmallX (n-1)) (f:args)
          _ -> False

-- Remove effect open applications
topDown (App (TypeApp (Var openName _) _) [arg])  | getName openName == nameEffectOpen
  = topDown arg

-- Direct function applications
topDown (App (Lam pars eff body) args) | length pars == length args 
  = do newNames <- mapM uniqueTName pars
       let sub = [(p,Var np InfoNone) | (p,np) <- zip pars newNames]
       topDown $ Let (zipWith makeDef newNames args) (sub |~> body)       
  where           
    makeDef (TName npar nparTp) arg 
      = DefNonRec (Def npar nparTp arg Private DefVal rangeNull "") 

-- No optimization applies
topDown expr
  = return expr



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

bottomUp (App f args)
  = App f (map bottomUpArg args)

-- No optimization applies
bottomUp expr
  = expr

bottomUpArg :: Expr -> Expr
bottomUpArg arg
  = case arg of
      App (Var v _) [expr] | getName v == nameEnsureK -> expr
      _ -> arg

{--------------------------------------------------------------------------
  Definitions 
--------------------------------------------------------------------------}

instance Simplify DefGroup where
  simplify (DefRec    defs) = fmap DefRec (mapM simplify defs)
  simplify (DefNonRec def ) = fmap DefNonRec (simplify def)

instance Simplify Def where
  simplify (Def name tp expr vis isVal nameRng doc) 
    = do expr' <- simplify expr
         return $ Def name tp expr' vis isVal nameRng doc

instance Simplify a => Simplify [a] where
  simplify  = mapM simplify

{--------------------------------------------------------------------------
  Expressions 
--------------------------------------------------------------------------}

instance Simplify Expr where
  simplify e 
    = do td <- topDown e
         e' <- case td of
                Lam tnames eff expr
                  -> do x <- simplify expr; return $ Lam tnames eff x
                Var tname info     
                  -> return td
                App e1 e2          
                  -> do x1 <- simplify e1
                        x2 <- simplify e2
                        return $ App x1 x2
                TypeLam tv expr    
                  -> fmap (TypeLam tv) (simplify expr)
                TypeApp expr tp    
                  -> do x <- simplify expr; return $ TypeApp x tp
                Con tname repr     
                  -> return td
                Lit lit            
                  -> return td
                Let defGroups expr 
                  -> do dgs <- simplify defGroups
                        x   <- simplify expr
                        return $ Let dgs x
                Case exprs branches
                  -> do xs <- simplify exprs
                        bs <- simplify branches
                        return $ Case xs bs
         return (bottomUp e')

instance Simplify Branch where
  simplify (Branch patterns guards) 
    = fmap (Branch patterns) (mapM simplify guards)

instance Simplify Guard where
  simplify (Guard test expr) 
    = do xt <- simplify test
         xe <- simplify expr
         return $ Guard xt xe



{--------------------------------------------------------------------------
  Occurrences 
--------------------------------------------------------------------------}


isTotalAndCheap :: Expr -> Bool
isTotalAndCheap expr
  = case expr of
      Var{} -> True
      Con{} -> True
      Lit{} -> True
      -- toany(x)
      App (TypeApp (Var v _) [_]) [arg] | getName v == nameToAny -> isTotalAndCheap arg      
      -- functions that are immediately applied to something cheap (cps generates this for resumes)
      -- Lam pars eff (App e args) 
      --  -> isTotalAndCheap e && all isTotalAndCheap args -- matchParArg (zip pars args)
      -- type application / abstraction
      TypeLam _ body -> isTotalAndCheap body
      TypeApp body _ -> isTotalAndCheap body

      _     -> False
  where
    matchParArg (par,Var v _) = par == v
    matchParArg (par,App (TypeApp (Var v _) [_]) [Var arg _]) = par == arg && getName v == nameToAny
    matchParArg _ = False

    matchTParTArg (tv1,TVar tv2) = tv1 == tv2
    matchTParTArg _ = False

occursAtMostOnce :: Name -> Expr -> Bool
occursAtMostOnce name expr
  = case M.lookup name (occurrences expr) of
      Nothing -> True
      Just oc -> case oc of 
                   Many -> False
                   _    -> True


-- occurs at most once; and if so, it was fully applied to `tn` type arguments and `n` arguments.
occursAtMostOnceApplied :: Name -> Int -> Int -> Expr -> Bool
occursAtMostOnceApplied name tn n expr
  = case M.lookup name (occurrences expr) of
      Nothing -> True
      Just oc -> case oc of 
                   Many      -> False
                   Once tm m -> (tn==tm && n==m)
                   _         -> True



data Occur = None | Once Int Int | Many

add oc1 oc2
  = case oc1 of  
      None -> oc2
      Many -> Many
      Once _ _ -> case oc2 of 
                    None -> oc1
                    _    -> Many

occurrences :: Expr -> M.NameMap Occur
occurrences expr
  = case expr of
      App (TypeApp (Var v _) targs) args
        -> ounions (M.singleton (getName v) (Once (length targs) (length args)) : map occurrences args)
      App (Var v _) args 
        -> ounions (M.singleton (getName v) (Once 0 (length args)) : map occurrences args)
      Var v _ -> M.singleton (getName v) (Once 0 0)

      Con{} -> M.empty
      Lit{} -> M.empty
      App f args        -> ounions (occurrences f : map occurrences args)
      Lam pars eff body -> foldr M.delete (occurrences body) (map getName pars)
      TypeLam _ body    -> occurrences body
      TypeApp body _    -> occurrences body
      Let dgs body      -> foldr occurrencesDefGroup (occurrences body) dgs
      Case scruts bs    -> ounions (map occurrences scruts ++ map occurrencesBranch bs)

occurrencesBranch :: Branch -> M.NameMap Occur
occurrencesBranch (Branch pat guards)
  = foldr M.delete (ounions (map occurrencesGuard guards)) (map getName (S.elems (bv pat)))

occurrencesGuard (Guard g e)
  = ounion (occurrences g) (occurrences e) 

ounion :: M.NameMap Occur -> M.NameMap Occur -> M.NameMap Occur
ounion oc1 oc2
  = M.unionWith add oc1 oc2

ounions :: [M.NameMap Occur] -> M.NameMap Occur
ounions ocs
  = M.unionsWith add ocs

occurrencesDefGroup :: DefGroup -> M.NameMap Occur -> M.NameMap Occur
occurrencesDefGroup dg oc
  = case dg of
      DefNonRec def -> ounion (M.delete (defName def) oc) (occurrences (defExpr def))
      DefRec defs   -> foldr M.delete (ounions (oc : map (occurrences . defExpr) defs)) 
                                      (map defName defs)


uniqueTName (TName name tp)
  = do i <- unique
       return (TName (postpend ("." ++ show i) name) tp)