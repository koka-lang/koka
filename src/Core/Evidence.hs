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
import Common.NamePrim( nameTpEv, nameConEv, nameEffectOpen, nameReturn, nameTpHandled, nameTpHandled1 )
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

      -- Interesting cases.
      Lam params eff body
        -> let params' = map param params
           in do (evCtx', body') <- evExpr pnil body
                 return $ (pnil, abstract evCtx' $ Lam params' eff body')
                   where param :: TName -> TName
                         param (TName name ty) = TName name (evType ty)
                         toParam :: (Name, Type) -> TName
                         toParam (name, ty) = TName name ty

      App f args
        -> do (evCtx', f') <- evExpr evCtx f
              args' <- mapM (evExpr evCtx) args
              let args'' = map snd args'
              let (q3, s1'') = decomposeEvType (typeOf f')
              -- FIXME TODO: join evidence environments, check type
              -- equality, introduce evidence abstraction.
              return $ (undefined, App f' args'')

      -- Regular cases
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

      TypeLam tvars body
        -> do (evCtx', body') <- evExpr evCtx body
              return $ (evCtx', TypeLam tvars body')

      TypeApp body tps
        -> let tps' = map evType tps
           in do (evCtx', body') <- evExpr evCtx body
                 return $ (evCtx', TypeApp body' tps')

      _ -> return (pnil, expr) -- leave unchanged


abstract :: P -> Expr -> Expr
abstract p expr
  = foldr (\(_, (tname, _)) expr -> Lam [tname] typeTotal expr) expr p

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
           evs     = evFromEffectType eff
           cod'    = evType cod
       in foldr
            (\ev cod -> TFun [ev] typeTotal cod)
            (TFun params' eff cod')
            evs
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

evFromEffectType :: Tau -> [(Name, Type)]
evFromEffectType eff
  = let (constants, tail) = extractOrderedEffect eff
        labels            = map (toRuntimeLabelName . labelName) constants
        evTypes           = map (makeEvType . extractTypeConstant) constants
    in zip labels evTypes

extractTypeConstant :: Tau -> Tau
extractTypeConstant t
  = case expandSyn t of
      TApp (TCon (TypeCon name _)) [htp] | (name == nameTpHandled || name == nameTpHandled1)
        -> extractTypeConstant htp -- reach under the handled<htp> name to extract htp.
      t' -> t'

toRuntimeLabelName :: Name -> Name
toRuntimeLabelName lbl = makeHiddenName "ev" lbl -- FIXME TODO: match whatever names are generated by the parser.

evsFromType :: Type -> Q
evsFromType (TFun [(name, ty)] _ cod)
   | isEvType ty = (recoverLabel name, (name, ty)) : evsFromType cod
   | otherwise   = []
evsFromType _ = []

evTypeErase :: Type -> Type
evTypeErase (ty @ TFun [(_, ty)] _ cod)
   | isEvType ty = evTypeErase cod
   | otherwise = ty
evTypeErase ty = ty

decomposeEvType :: Type -> (Q, Type)
decomposeEvType ty = (evsFromType ty, evTypeErase ty)

recoverLabel :: Name -> Label
recoverLabel name
  = case nameId name of
     '.' : 'e' : 'v' : '-' : label -> label
     _ -> error "Expected an evidence name."

-----------------------------------------------------------------------------
-- Evidence constructor
-----------------------------------------------------------------------------
type Label = String
data Ev = Ev Label

evClosureR :: P -> Q -> Expr -> (P, Expr)
evClosureR _ [] expr = (pnil, expr)
evClosureR p ((l, nt) : q) expr
  = case lookup l p of
      Nothing
        -> let (p', expr') = evClosureR p q expr
               w           = makeWitness nt
               x           = toVar w
               p''         = (l, (varName x, varInfo x)) : p'
               expr''      = App expr' [x]
           in (p'', expr'')
      Just (name, info)
        -> let (p', expr') = evClosureR p q expr
               expr''      = App expr' [Var { varName = name, varInfo = info }]
           in (p', expr'')

toVar :: Witness -> Expr
toVar (name, info) = Var { varName = name, varInfo = info }

type Witness = (TName, VarInfo)

makeWitness :: (Name, Type) -> Witness
makeWitness (lbl, t)
  = (TName lbl t, InfoNone) -- FIXME TODO: include correct var info.

-----------------------------------------------------------------------------
-- Evidence Monad
-----------------------------------------------------------------------------

newtype EvMon a = EvMon (Env -> State -> Result a)

type Q = [(Label, (Name, Type))]
type P = [(Label, (TName, VarInfo))]

pnil :: P
pnil = []

(<>) :: P -> P -> P
p0 <> p1 = mappend p0 p1

data Env = Env { penv_ :: P }

getEnv :: EvMon Env
getEnv = EvMon (\env st -> Ok env st)

withEnv :: (Env -> Env) -> EvMon a -> EvMon a
withEnv f (EvMon ctxt)
  = EvMon (\env st -> ctxt (f env) st)

-- evLookup :: Label -> EvMon (Maybe Ev)
-- evLookup label
--   = do env <- getEnv
--        return $ lookup label (penv_ env)

-- evInsert :: Label -> Ev -> EvMon a -> EvMon a
-- evInsert label ev evm
--   = withEnv (\env -> Env { penv_ = (label, ev) : (penv_ env) }) evm

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
