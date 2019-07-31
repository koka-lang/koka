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
evDefGroups :: DefGroups -> EvMon DefGroups
evDefGroups evDefGroups
  = do defGroupss <- mapM evDefGroup evDefGroups
       return (concat defGroupss)

evDefGroup (DefRec defs)
  = do defss <- mapM (evDef True) defs
       return [DefRec (concat defss)]

evDefGroup (DefNonRec def)
  = do defs <- evDef False def
       return (map DefNonRec defs)


{--------------------------------------------------------------------------
  transform a definition
--------------------------------------------------------------------------}
evDef :: Bool -> Def -> EvMon [Def]
evDef recursive def
  = undefined

evExpr :: Expr -> EvMon Expr
evExpr expr
  = case expr of
      --  lift _open_ applications
      App eopen@(TypeApp (Var open _) [effFrom,effTo,_,_]) [f]
        | getName open == nameEffectOpen
        -> undefined

      -- return
      App ret@(Var v _) [arg] | getName v == nameReturn
        -> undefined

      -- regular cases
      Lam args eff body
        -> undefined

      App f args
        -> undefined

      Let defgs body
        -> undefined

      Case exprs bs
        -> undefined

      Var (TName name tp) info -- FIXME TODO: potentially update arity information.
        -> let tp' = evType tp
           in return $ Var (TName name tp') info

      -- type application and abstraction
      TypeApp (TypeLam tvars body) tps  | length tvars == length tps
        -> undefined

      TypeLam tvars body
        -> undefined

      TypeApp body tps
        -> undefined

      _ -> return expr -- leave unchanged


{--------------------------------------------------------------------------
  transform a types
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
evPred (PredSub t t') = undefined
evPred (PredIFace name ts) = undefined

-----------------------------------------------------------------------------
-- Evidence Monad
-----------------------------------------------------------------------------

newtype EvMon a = EvMon (Env -> State -> Result a)

data Env = Env { foobar :: [()] } -- FIXME TODO.

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
