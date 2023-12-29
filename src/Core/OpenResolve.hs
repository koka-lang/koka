-----------------------------------------------------------------------------
-- Copyright 2020-2021, Microsoft Research, Daan Leijen
--
-- This is free software; you can redistribute it and/or modify it under the
-- terms of the Apache License, Version 2.0. A copy of the License can be
-- found in the LICENSE file at the root of this distribution.
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
  -- Lib.Trace.trace s
    x

data Env = Env{ penv :: Pretty.Env, gamma :: Gamma }

openResolve :: Pretty.Env -> Gamma -> CorePhase b ()
openResolve penv gamma 
  = liftCorePhase $ \defs -> resDefGroups (Env penv gamma) defs

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
  = trace ("\n*** enter def: " ++ show (defName def)) $
    def{ defExpr = resExpr env (defExpr def) }

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
  = trace (" resolve open: " ++ show (ppType penv effFrom) ++ ", to " ++ show (ppType penv effTo)) $
    let n               = length targs
        (lsFrom,tlFrom) = extractHandledEffect effFrom
        (lsTo,tlTo)     = extractHandledEffect effTo
    in -- assertion ("Core.OpenResolve.resOpen: opening from non-closed effect? " ++ show (ppType penv effFrom)) (isEffectFixed effFrom) $
       -- if effFrom is not closed (fixed) it comes from a mask (inject) in the type inferencer
       {-
       if (matchLabels lsFrom lsTo)
        then -- same effect (except for builtins and/or polymorphic tail?)
             case (runUnifyEx 0 (unify tpFrom tpTo)) of
               (Left _,_,_)  -> -- not exactly equal, leave the .open as a cast
                                trace " use cast" $ App eopen [expr]
               (Right _,_,_) -> -- exact match, just use expr
                                trace " identity" $ expr
        else
       -}
       if (matchType effFrom effTo)
        then -- exact match, just use expr
             trace "  identity" $ expr
       else if (not (isEffectFixed effFrom))
        then {- if (and [matchType t1 t2 | (t1,t2) <- zip lsFrom lsTo])
              then -- all handled effect match, just use expr
                   trace "masking? " $ expr
              else -} failure $ ("Core.openResolve.resOpen: todo: masking handled effect: " ++ show (ppType penv effFrom))
       else if (matchType tlFrom tlTo && length lsFrom == length lsTo && and [matchType t1 t2 | (t1,t2) <- zip lsFrom lsTo])
        then -- same handled effects, just use expr
             trace "  same handled effects, leave as is" $
             expr
        else -- not equal in handled effects, insert open
             let resolve name = case gammaLookup name gamma of
                                  [(qname,info)] -> coreExprFromNameInfo qname info
                                  ress -> failure $ "Core.OpenResolve.resOpen: unknown name: " ++ show name -- ++ ", " ++ show gamma
                 -- actionPar = TName (newHiddenName "action") (TFun targs effFrom tres)
                 params = [TName (newHiddenName ("x" ++ show i)) (snd targ) | (i,targ) <- zip [1..] targs]
                 exprName = TName (newHiddenName "x0") (tpFrom)
                 exprVar  = Var exprName InfoNone
                 exprApp lam  = App (Lam [exprName] typeTotal lam) [expr]
                 
                 wrapperThunk openExpr evExprs
                   = exprApp $
                       Lam params effTo $
                           App (makeTypeApp openExpr [tres,effFrom,effTo])
                               (evExprs ++ [Lam [] effTo (App exprVar [Var p InfoNone | p <- params])])
                    
                           
                 wrapper openExpr evExprs
                   = exprApp $
                       Lam params effTo $
                         App (makeTypeApp openExpr (map snd targs ++ [tres,effFrom,effTo]))
                             (evExprs ++ [exprVar] ++ [Var p InfoNone | p <- params])
                           
                   
                 evIndexOf l
                   = let (htagTp,hndTp)
                             = let (name,_,tpArgs) = labelNameEx l
                                   hndCon = TCon (TypeCon (toHandlerName name)
                                                          (kindFunN (map getKind tpArgs ++ [kindEffect,kindStar]) kindStar))
                               in (makeTypeApp (resolve (toEffectTagName name)) tpArgs, typeApp hndCon tpArgs)
                     in App (makeTypeApp (resolve nameEvvIndex) [effTo,hndTp]) [htagTp]
             in case lsFrom of
                 []  -> -- no handled effect, use cast
                        case lsTo of
                          [] -> trace ("  no handled effect, in no handled effect context: use cast")
                                expr
                          _  -> trace ("  no handled effect; use none: " ++ show expr) $
                                if (isHandlerFree expr) 
                                 then trace ("***  remove open-none") $  -- fully total with using any operations that need evidence; just leave it as is
                                      expr
                                else if (n <= 4) 
                                 then wrapper (resolve (nameOpenNone n)) []  -- fails in perf1c with exceeded stack size if --optmaxdup < 500 (since it prevents a tailcall)
                                      -- expr  -- fails in nim as it evidence is not cleared
                                 else wrapperThunk (resolve (nameOpenNone 0)) []
                                      
                 [l] -> -- just one: used open-atN for efficiency
                        trace ("  one handled effect; use at: " ++ show (ppType penv l)) $
                        if (n <= 4) 
                         then wrapper (resolve (nameOpenAt n)) [evIndexOf l]
                         else wrapperThunk (resolve (nameOpenAt 0)) [evIndexOf l]

                 _ -> --failure $ "Core.OpenResolve.resOpen: todo: from: " ++ show (ppType penv effFrom) ++ ", to " ++ show (ppType penv effTo)
                      --           ++ " with handled: " ++ show (map (ppType penv) lsFrom, map (ppType penv) lsTo)
                      let indices = makeVector typeEvIndex (map evIndexOf lsFrom)
                      in if (n <= 3) 
                          then wrapper (resolve (nameOpen n)) [indices]
                          else wrapperThunk (resolve (nameOpen 0)) [indices]

resOpen (Env penv gamma) eopen effFrom effTo tpFrom tpTo expr
  = failure $ "Core.OpenResolve.resOpen: open applied to a non-function? " ++ show (ppType penv effTo)



matchLabels (l1:ls1) (l2:ls2) = (labelName l1 == labelName l2) && matchLabels ls1 ls2
matchLabels [] []             = True
matchLabels _ _               = False

-- is a function expression handler free? : meaning if invoked, 
-- it will never need evidence (invoke an operation) or change the evidence (use a handler).
isHandlerFree :: Expr -> Bool   
isHandlerFree expr  
  = case expr of
      TypeLam tpars body -> isHandlerFree body
      TypeApp body targs -> isHandlerFree body
      Lam pars eff body  -> not (containsHandledEffect [] eff) && isHandlerFree body
      App f args         -> all isHandlerFree (f:args) 
      Var vname (Core.InfoExternal{})  
                  -> case handlerFreeFunType (typeOf vname) of
                       Nothing  -> True
                       Just ok  -> ok
      Var vname _ -> case handlerFreeFunType (typeOf vname) of 
                       Nothing   -> True
                       Just ok   -> ok && (isSystemCoreName (getName vname))
      Con{} -> True
      Lit{} -> True
      _     -> False
      
handlerFreeFunType :: Type -> Maybe Bool
handlerFreeFunType tp
  = case splitFunScheme tp of
      Just (_,_,_,eff,_) -> Just (not (containsHandledEffect [] eff))
      _ -> Nothing
