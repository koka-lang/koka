-----------------------------------------------------------------------------
-- Copyright 2019 Microsoft Corporation, Daan Leijen, Daniel Hillerström
--
-- This is free software; you can redistribute it and/or modify it under the
-- terms of the Apache License, Version 2.0. A copy of the License can be
-- found in the file "license.txt" at the root of this distribution.
-----------------------------------------------------------------------------

-----------------------------------------------------------------------------
-- Evidence translation for effects
-----------------------------------------------------------------------------

module Core.Evidence( evidenceTransform
                    ) where


import qualified Lib.Trace
import Control.Monad
import Control.Applicative

import Lib.PPrint

import Common.Failure
import Common.Name
import Common.Range
import Common.Unique
import Common.NamePrim( nameTpEv, nameConEv, nameEffectOpen, nameReturn )
import Common.Error
import Common.Syntax

import Kind.Kind( kindStar, isKindEffect, kindFun, kindEffect, kindHandled )

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

trace s x =
  -- Lib.Trace.trace s
    x

evidenceTransform :: Pretty.Env -> DefGroups -> Error DefGroups
evidenceTransform penv defGroups
  = return defGroups

{--------------------------------------------------------------------------
  transform definition groups
--------------------------------------------------------------------------}
evDefGroups :: P -> DefGroups -> EvMon (P, DefGroups)
evDefGroups evCtx evDefGroups
  = do defGroups' <- mapM (evDefGroup evCtx) evDefGroups
       let evCtx' = foldr (<>) pnil (map fst defGroups')
       return $ (evCtx', map snd defGroups')

evDefGroup evCtx (DefRec defs)
  = do defs' <- mapM (evDef evCtx) defs
       let evCtx' = foldr (<>) pnil (map fst defs')
       return $ (evCtx, DefRec (map snd defs'))

evDefGroup evCtx (DefNonRec def)
  = do (evCtx', def') <- evDef evCtx def
       return $ (evCtx', DefNonRec def')


{--------------------------------------------------------------------------
  transform a definition
--------------------------------------------------------------------------}
evDef :: P -> Def -> EvMon (P, Def)
evDef evCtx def
  = do (evCtx', expr') <- evExpr evCtx (defExpr def)
       let ty' = evType (defType def)
       return $ (evCtx', def { defExpr = expr', defType = ty' })

evExpr :: P -> Expr -> EvMon (P, Expr)
evExpr evCtx expr
  = case expr of
      --  lift _open_ applications
      App eopen@(TypeApp (Var open _) [effFrom,effTo,_,_]) [f]
        | getName open == nameEffectOpen
        -> undefined

      -- regular cases
      Lam params eff body
        -> let params' = map param params
           in do (evCtx', body') <- evExpr pnil body
                 let evs = [] -- FIXME TODO: add evidence parameters.
                 return $ (pnil, Lam (evs ++ params') eff body')
                   where param :: TName -> TName
                         param (TName name ty) = TName name (evType ty)

      App f args
        -> do (evCtx', f') <- evExpr evCtx f
              args' <- mapM (evExpr evCtx) args
              let args'' = map snd args'
              -- FIXME TODO: join evidence environments, check type
              -- equality, introduce evidence abstraction.
              return $ (undefined, App f' args'')

      Let defgs body
        -> do (_, defgs') <- evDefGroups evCtx defgs
              (evCtx', body') <- evExpr evCtx body
              return $ (evCtx', Let defgs' body')

      Case exprs bs
        -> do exprs' <- mapM (evExpr evCtx) exprs
              bs' <- mapM branch bs
              let evCtx' = foldr (<>) pnil (map fst exprs')
              let evCtx'' = foldr (<>) evCtx' (map fst bs')
              let exprs'' = map snd exprs'
              let bs'' = map snd bs'
              return $ (evCtx'', Case exprs'' bs'')
                where branch :: Branch -> EvMon (P, Branch)
                      branch (Branch pats guards)
                        = do guards' <- mapM guard guards
                             let evCtx' = foldr (<>) pnil (map fst guards')
                             let guards'' = map snd guards'
                             return $ (evCtx', Branch { branchPatterns = pats
                                                      , branchGuards   = guards'' })
                      guard :: Guard -> EvMon (P, Guard)
                      guard (Guard test expr)
                        = do (evCtx', test') <- evExpr evCtx test
                             (evCtx'', expr') <- evExpr evCtx expr
                             return $ (evCtx' <> evCtx'',
                                       Guard { guardTest = test'
                                             , guardExpr = expr' })

      Var (TName name tp) info -- FIXME TODO: potentially update arity information.
        -> let tp' = evType tp
           in return $ (pnil, Var (TName name tp') info)

      -- type application and abstraction
      TypeLam tvars body
        -> do (evCtx', body') <- evExpr evCtx body
              return $ (evCtx', TypeLam tvars body')

      TypeApp body tps
        -> let tps' = map evType tps
           in do (evCtx', body') <- evExpr evCtx body
                 return $ (evCtx', TypeApp body' tps')

      _ -> return (pnil, expr) -- leave unchanged


{--------------------------------------------------------------------------
  transform a type
--------------------------------------------------------------------------}
evType :: Type -> Type
evType typ
  = case typ of
     TCon tycon              -> TCon tycon
     TVar tyvar              -> TVar tyvar
     TFun params eff cod     ->
       let params' = map (\(name, t) -> (name, evType t)) params
           evs     = undefined -- FIXME TODO.
           cod'    = evType cod
       in TFun (evs ++ params') eff cod'
     TForall tyvars pred rho ->
       let pred' = map evPred pred
           rho'  = evType rho
       in TForall tyvars pred' rho'
     TApp t ts               ->
       let t'  = evType t
           ts' = map evType ts
       in TApp t' ts'
     TSyn tysyn ts t         ->
       let ts' = map evType ts
           t'  = evType t
       in TSyn tysyn ts t

evPred :: Pred -> Pred
evPred (PredSub t t') = PredSub (evType t) (evType t')
evPred (PredIFace name ts) = PredIFace name (map evType ts)

-----------------------------------------------------------------------------
-- Evidence constructor
-----------------------------------------------------------------------------
type Label = String
data Ev = Ev Label

-----------------------------------------------------------------------------
-- Evidence Monad
-----------------------------------------------------------------------------

newtype EvMon a = EvMon (Env -> State -> Result a)

type Q = [Label]
type P = [(Label, Ev)]

pnil :: P
pnil = []

(<>) :: P -> P -> P
p0 <> p1 = undefined -- FIXME TODO: implement evidence composition

-- This cannot be entirely right...
evEnact :: P -> Q -> Expr -> (P, Expr)
evEnact _ [] e = (pnil, e)
evEnact p (l : q) e
  = case lookup l p of
     Nothing ->
       let (p', e') = evEnact p q e
           ql       = undefined -- TODO create term.
           l'       = undefined -- TODO create runtime name
       in ((l, Ev l') : p', App e' ql)
     Just (Ev l') ->
       let (p', e') = evEnact p q e
           ql       = undefined -- TODO create term.
       in (p', App e' ql)

data Env = Env { penv_ :: P }

getEnv :: EvMon Env
getEnv = EvMon (\env st -> Ok env st)

withEnv :: (Env -> Env) -> EvMon a -> EvMon a
withEnv f (EvMon ctxt)
  = EvMon (\env st -> ctxt (f env) st)

evLookup :: Label -> EvMon (Maybe Ev)
evLookup label
  = do env <- getEnv
       return $ lookup label (penv_ env)

evInsert :: Label -> Ev -> EvMon a -> EvMon a
evInsert label ev evm
  = withEnv (\env -> Env { penv_ = (label, ev) : (penv_ env) }) evm

data State = State { uniq :: Int }

data Result a = Ok a State

instance Functor EvMon where
  fmap f (EvMon ctxt) = EvMon (\env st -> case ctxt env st of
                                            Ok x st' -> Ok (f x) st')

instance Applicative EvMon where
  pure = return
  (<*>) = ap

instance Monad EvMon where
  return x           = EvMon (\_ st -> Ok x st)
  (EvMon ctxt) >>= k = EvMon (\env st -> case ctxt env st of
                                           Ok x st' -> case k x of
                                                         EvMon ctxt' -> ctxt' env st')
