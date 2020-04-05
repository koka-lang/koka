-----------------------------------------------------------------------------
-- Copyright 2020 Microsoft Corporation, Daan Leijen
--
-- This is free software; you can redistribute it and/or modify it under the
-- terms of the Apache License, Version 2.0. A copy of the License can be
-- found in the file "license.txt" at the root of this distribution.
-----------------------------------------------------------------------------

-----------------------------------------------------------------------------
-- Transform .open to specific open calls
-----------------------------------------------------------------------------

module Core.OpenResolve(  openResolve ) where


import qualified Lib.Trace
import Control.Monad
import Control.Applicative

import Lib.PPrint
import Common.Failure
import Common.Name
import Common.Range
import Common.Unique
import Common.NamePrim
import Common.Error
import Common.Syntax

import Kind.Kind
import Type.Type
import Type.Kind
import Type.TypeVar
import Type.Pretty hiding (Env)
import qualified Type.Pretty as Pretty
import Type.Assumption
import Type.Operations( freshTVar )
import Core.Core
import qualified Core.Core as Core
import Core.Pretty
import Core.CoreVar
import Type.Unify( unify, runUnifyEx )

trace s x =
   Lib.Trace.trace s
    x

data Env = Env{ penv :: Pretty.Env, gamma :: Gamma }

openResolve :: Pretty.Env -> Gamma -> DefGroups -> DefGroups
openResolve penv gamma defs
  = resDefGroups (Env penv gamma) defs

{--------------------------------------------------------------------------
  transform definition groups
--------------------------------------------------------------------------}
resDefGroups :: Env -> DefGroups -> DefGroups
resDefGroups env defGroups
  = map (resDefGroup env) defGroups

resDefGroup env (DefRec defs)
  = DefRec (map (resDef env) defs)

resDefGroup env (DefNonRec def)
  = DefNonRec (resDef env def)


{--------------------------------------------------------------------------
  transform a definition
--------------------------------------------------------------------------}
resDef :: Env -> Def -> Def
resDef env def
  = def{ defExpr = resExpr env (defExpr def) }

resExpr :: Env -> Expr -> Expr
resExpr env expr
  = let recurse = resExpr env
    in case expr of
      App eopen@(TypeApp (Var open _) [effFrom,effTo,tpFrom,tpTo]) [f] | getName open == nameEffectOpen
          -> resOpen env eopen effFrom effTo tpFrom tpTo (recurse f)
      -- regular cases
      Lam args eff body
        -> Lam args eff (recurse body)
      App f args
        -> App (recurse f) (map recurse args)
      Let defgs body
        -> Let (resDefGroups env defgs) (recurse body)
      Case exprs bs
        -> Case (map recurse exprs) (map (resBranch env) bs)
      TypeLam tvars body
        -> TypeLam tvars (recurse body)
      TypeApp body tps
        -> TypeApp (recurse body) tps
      _ -> expr  -- var,lit


resBranch :: Env -> Branch -> Branch
resBranch env (Branch pat guards)
  = Branch pat (map (resGuard env) guards)

resGuard :: Env -> Guard -> Guard
resGuard env (Guard guard body)
  = Guard (resExpr env guard) (resExpr env body)


{--------------------------------------------------------------------------
  Insert open call
--------------------------------------------------------------------------}

resOpen :: Env -> Expr -> Effect -> Effect -> Type -> Type -> Expr -> Expr
resOpen (Env penv gamma) eopen effFrom effTo tpFrom tpTo@(TFun targs _ tres) expr
  = trace ("resolve open: " ++ show (ppType penv effFrom) ++ ", to " ++ show (ppType penv effTo)) $
    let n       = length targs
        (ls1,tl1) = extractHandledEffects effFrom
        (ls2,tl2) = extractHandledEffects effTo
    in -- assertion ("Core.OpenResolve.resOpen: opening from non-closed effect? " ++ show (ppType penv effFrom)) (isEffectFixed effFrom) $
       -- if effFrom is not closed (fixed) it comes from a mask (inject) in the type inferencer
       if (matchLabels ls1 ls2)
        then -- same effect (except for builtins?)
             case (runUnifyEx 0 (unify tpFrom tpTo)) of
               (Left _,_,_)  -> -- not exactly equal, leave the .open as a cast
                                trace " use cast" $ App eopen [expr]
               (Right _,_,_) -> -- exact match, just use expr
                                trace " identity" $ expr
        else -- not equal, insert open
             let resolve name = case gammaLookup name gamma of
                                  [(qname,info)] -> coreExprFromNameInfo qname info
                                  _ -> failure $ "Core.openResolve.resOpen: unknown name: " ++ show name
                 actionPar = TName (newHiddenName "action") (TFun targs effFrom tres)
                 params = [actionPar] ++ [TName (newHiddenName ("x" ++ show i)) (snd targ) | (i,targ) <- zip [1..] targs]
                 wrapper openExpr evExprs
                   = Lam params effFrom $
                       App (makeTypeApp openExpr (map snd targs ++ [tres,effFrom,effTo]))
                           (evExprs ++ [Var p InfoNone | p <- params])
             in case ls1 of
                 []  -> -- no handled effect, use cast
                        trace (" no handled effect; use cast") $
                        -- wrapper (resolve (nameOpenNone n)) []
                        App eopen [expr]
                 [l] -> -- just one: used open-atN for efficiency
                        trace (" one handled effect; use at: " ++ show (ppType penv l)) $
                        let (htagTp,hndTp)
                                = let (name,_,tpArgs) = labelNameEx l
                                      hndCon = TCon (TypeCon (toHandlerName name)
                                                             (kindFunN (map getKind tpArgs ++ [kindEffect,kindStar]) kindStar))
                                  in (makeTypeApp (resolve (toEffectTagName name)) tpArgs, typeApp hndCon tpArgs)
                        in wrapper (resolve (nameOpenAt n)) [App (makeTypeApp (resolve nameEvvIndex) [effTo,hndTp]) [htagTp]]

                 _ -> failure $ "Core.OpenResolve.resOpen: todo: from: " ++ show (ppType penv effFrom) ++ ", to " ++ show (ppType penv effTo)


makeTypeApp expr []     = expr
makeTypeApp (TypeApp expr targs0) targs1 = makeTypeApp expr (targs0 ++ targs1)
makeTypeApp expr targs  = TypeApp expr targs

matchLabels (l1:ls1) (l2:ls2) = (labelName l1 == labelName l2) && matchLabels ls1 ls2
matchLabels [] []             = True
matchLabels _ _               = False

extractHandledEffects eff
  = let (ls,tl) = extractOrderedEffect eff
    in (filter isHandledEffect ls, tl)
