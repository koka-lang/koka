-----------------------------------------------------------------------------
-- Copyright 2020 Microsoft Corporation, Daan Leijen, Anton Lorenzen
--
-- This is free software; you can redistribute it and/or modify it under the
-- terms of the Apache License, Version 2.0. A copy of the License can be
-- found in the file "license.txt" at the root of this distribution.
-----------------------------------------------------------------------------
{-# LANGUAGE NamedFieldPuns, GeneralizedNewtypeDeriving  #-}

-----------------------------------------------------------------------------
-- Borrow inference for precise automatic reference counting
-----------------------------------------------------------------------------

module Backend.C.ParcInfer ( parcInfer ) where

import Lib.Trace (trace)
import Control.Monad
import Control.Monad.Reader
import Control.Monad.State.Strict
import Data.Maybe ( catMaybes, fromMaybe, isJust )
import Data.Char
import qualified Data.List as L
import qualified Data.Map.Strict as M
import Data.Set ( (\\) )
import qualified Data.Set as S

import Kind.Newtypes
import Type.Type
import qualified Type.Pretty as Pretty

import Lib.PPrint
import Common.NamePrim
import Common.Failure
import Common.Unique
import Common.Syntax hiding (scanFields)

import Core.Core
import Core.CoreVar
import Core.Pretty
import Core.Borrowed

-- We borrow every parameter, except:
--  a) Those that are drop-reused anywhere in the function
--     (because drop-reuse optimization is more important)
--  b) Those that are returned or passed as owned to another function
--     (because we don't save anything there)
--  c) Recursive types in recursive functions
--     (this is a crude heuristic so that we don't free too late)
--
-- a) and b) are tracked by marking variables as "consumed"
-- similar to the live set in Parc.
-- Unlike in Parc, we don't have to traverse the tree backwards.
-- c) is tracked by looking at the newtypes.
--
-- If a parameter was marked as borrowing by the user,
-- we borrow it even if any of the above holds.


parcInfer :: Pretty.Env -> Platform -> Newtypes -> Borrowed -> Core -> Unique Core
parcInfer penv platform newtypes borrowed core
  = do defs <- runInfer penv platform newtypes borrowed (inferDefGroups True (coreProgDefs core))
       return core{coreProgDefs=defs}
  where penv' = penv{Pretty.coreShowDef=True,Pretty.coreShowTypes=False,Pretty.fullNames=False}
        tr d = trace (show (vcat (map (prettyDefGroup penv') d)))

--------------------------------------------------------------------------
-- Definition groups
--------------------------------------------------------------------------

inferDefGroups :: Bool -> DefGroups -> Infer DefGroups
inferDefGroups topLevel = mapM (inferDefGroup topLevel)

inferDefGroup :: Bool -> DefGroup -> Infer DefGroup
inferDefGroup topLevel dg
  = case dg of
      DefRec    defs -> DefRec    <$> mapM (inferDef topLevel) defs
      DefNonRec def  -> DefNonRec <$> inferDef topLevel def

inferDef :: Bool -> Def -> Infer Def
inferDef topLevel def
  = (if topLevel then isolated_ else id) $ withCurrentDef def $
      do -- inferTrace "enter def"
        (def', expr) <- inferDefExpr (defSort def) (defExpr def)
        return def{defExpr=expr, defSort=def'}

--------------------------------------------------------------------------
-- Main Infer algorithm
--------------------------------------------------------------------------

inferDefExpr :: DefSort -> Expr -> Infer (DefSort, Expr)
inferDefExpr (DefFun bs) expr
  = case expr of
      TypeLam tpars body
        -> fmap (TypeLam tpars) <$> inferDefExpr (DefFun bs) body
      Lam pars eff body
        -> do let parsBs = zip pars $ bs ++ repeat Own
              (body', cons) <- isolateWith S.empty
                            $ inferExpr body

              -- todo: criterion c)
              let bs' = flip map parsBs $ \(p, b) ->
                    if p `S.notMember` cons then Borrow else b
              return $ (DefFun bs', Lam pars eff body')
      _ -> (\x -> (DefFun bs, x)) <$> inferExpr expr
inferDefExpr def expr = (\x -> (def, x)) <$> inferExpr expr

inferExpr :: Expr -> Infer Expr
inferExpr expr
  = case expr of
      TypeLam tpars body
        -> TypeLam tpars <$> inferExpr body
      TypeApp body targs
        -> (`TypeApp` targs) <$> inferExpr body
      Lam pars eff body
        -> do (body', live) <- isolateWith S.empty
                             $ inferExpr body
              return $ Lam pars eff body'
      Var tname info | infoIsRefCounted info
        -> do -- inferTrace ("refcounted: " ++ show tname ++ ": " ++ show info)
              markConsumed tname
              return expr
      Var tname info -- InfoArity/External/Field are not reference-counted
        -> do -- inferTrace ("not refcounted: " ++ show tname ++ ": " ++ show info)
             return expr
      App inner@(TypeApp (Var tname info) targs) args
        -> inferBorrowApp tname inner args
      App inner@(Var tname info) args
        -> inferBorrowApp tname inner args
      App fn args
        -> do args' <- mapM inferExpr args
              fn'   <- inferExpr fn
              return $ App fn' args'
      Lit _
        -> return expr
      Con ctor repr
        -> return expr
      Let [] body
        -> inferExpr body
      Let (DefNonRec def:dgs) body
        -> do body' <- scoped (bv def) $ inferExpr (Let dgs body)
              def'  <- inferDef False def
              return $ makeLet [DefNonRec def'] body'
      Let (DefRec _ : _) _
        -> failure "Backend.C.Infer.inferExpr: Recursive definition in let"
      Case vars brs
        -> Case vars <$> inferBranches vars brs

-- | Find out which variables were passed as borrowed in the application.
-- todo: this only works for user-supplied borrow annotations at the moment!
inferBorrowApp :: TName -> Expr -> [Expr] -> Infer Expr
inferBorrowApp tname inner args
  = do bs <- getParamInfos (getName tname)
       let argsBs = zip args $ bs ++ repeat Own
       args' <- forM argsBs $ \(a, b) -> do
         case (a, b) of
           (Var argName argInfo, Borrow) -> return $ Var argName argInfo
           _ -> inferExpr a
       return $ App inner args'

inferBranches :: [Expr] -> [Branch] -> Infer [Branch]
inferBranches vars brs
  = mapM (inferBranch vars) brs

inferBranch :: [Expr] -> Branch -> Infer Branch
inferBranch vars (Branch pats guards)
  = scoped (bv pats) $ do
      guards' <- mapM inferGuard guards
      transferConsumed vars pats
      return $ Branch pats guards'

-- | We do not check for consumed variables in a test
-- since parc will use borrowing there anyway.
inferGuard :: Guard -> Infer Guard
inferGuard (Guard test expr)
  = do test' <- isolated_ $ inferExpr test
       expr' <- inferExpr expr
       return (Guard test' expr')

-- | Mark a variable as consumed if any of its components
-- is consumed; that is, one of the variables in the pattern
-- match is consumed.
transferConsumed :: [Expr] -> [Pattern] -> Infer ()
transferConsumed vars pats
  = do forM_ (zip vars pats) $ \(v, p) -> do
         case v of
           Var tname _ -> case p of
             PatCon _ ps _ _ _ _ _ _ ->
               mapM_ (transferConsumed [v] . (:[])) ps
             PatVar pname pat' -> do
               consumed <- isConsumed pname
               if consumed
                 then markConsumed tname
                 else transferConsumed [v] [pat']
             _ -> return ()
           _ -> return ()

-- | Decide whether a type is not self-referencing.
-- If a type is flat, it cannot be recursively destructured
-- by a recursive function and is safe to borrow without late frees.
-- However, mutually recursive datatypes are not safe to borrow either and
-- won't be found by this function (todo).
isFlatType :: Type -> Infer Bool
isFlatType tp = undefined

--------------------------------------------------------------------------
-- Infer monad
--------------------------------------------------------------------------

-----------------
-- definitions --

data Env = Env { currentDef :: [Def],
                 prettyEnv :: Pretty.Env,
                 platform  :: Platform,
                 newtypes  :: Newtypes,
                 borrowed  :: Borrowed
               }

type Consumed = TNames
data InferState = InferState
  { uniq :: Int
  , consumed :: Consumed
  }

type InferM a = ReaderT Env (State InferState) a
newtype Infer a = Infer (InferM a)
  deriving (Functor, Applicative, Monad, MonadReader Env, MonadState InferState)

instance HasUnique Infer where
  updateUnique f = do { old <- getUniq; modifyUniq f; return old }
  setUnique = setUniq

withEnv :: (Env -> Env) -> Infer a -> Infer a
withEnv = local

getEnv :: Infer Env
getEnv = ask

updateSt :: (InferState -> InferState) -> Infer ()
updateSt = modify

getSt :: Infer InferState
getSt = get

runInfer :: Pretty.Env -> Platform -> Newtypes -> Borrowed -> Infer a -> Unique a
runInfer penv platform newtypes borrowed (Infer action)
  = withUnique $ \u ->
      let env = Env [] penv platform newtypes borrowed
          st = InferState u S.empty
          (val, st') = runState (runReaderT action env) st
       in trace (show (ppBorrowed penv borrowed)) $
          (val, uniq st')

-------------------
-- env accessors --

getCurrentDef :: Infer [Def]
getCurrentDef = currentDef <$> getEnv

withCurrentDef :: Def -> Infer a -> Infer a
withCurrentDef def = withEnv (\e -> e { currentDef = def : currentDef e })

--

getPrettyEnv :: Infer Pretty.Env
getPrettyEnv = prettyEnv <$> getEnv

withPrettyEnv :: (Pretty.Env -> Pretty.Env) -> Infer a -> Infer a
withPrettyEnv f = withEnv (\e -> e { prettyEnv = f (prettyEnv e) })

--

getNewtypes :: Infer Newtypes
getNewtypes = newtypes <$> getEnv

withNewtypes :: (Newtypes -> Newtypes) -> Infer a -> Infer a
withNewtypes f = withEnv (\e -> e { newtypes = f (newtypes e) })

getPlatform :: Infer Platform
getPlatform = platform <$> getEnv

-- | Return borrowing infos for a name. May return the empty list
-- if no borrowing takes place.
getParamInfos :: Name -> Infer [ParamInfo]
getParamInfos name
  = do b <- borrowed <$> getEnv
       case borrowedLookup name b of
         Nothing -> return []
         Just pinfos -> return pinfos

---------------------
-- state accessors --

getUniq :: Infer Int
getUniq = uniq <$> getSt

modifyUniq :: (Int -> Int) -> Infer ()
modifyUniq f = updateSt (\s -> s { uniq = f (uniq s) })

setUniq :: Int -> Infer ()
setUniq = modifyUniq . const

--

getConsumed :: Infer Consumed
getConsumed = consumed <$> getSt

modifyConsumed :: (Consumed -> Consumed) -> Infer ()
modifyConsumed f = updateSt (\s -> s { consumed = f (consumed s) })

setConsumed :: Consumed -> Infer ()
setConsumed = modifyConsumed . const

-------------------------------
-- live set abstractions --

-- | Mark local variables as live.
markConsumed :: TName -> Infer ()
markConsumed tname
  = unless (isQualified (getName tname))
  $ modifyConsumed (S.insert tname)

markConsumedAll :: Foldable t => t TName -> Infer ()
markConsumedAll = mapM_ markConsumed

forget :: TNames -> Infer ()
forget tns = modifyConsumed (\\ tns)

isConsumed :: TName -> Infer Bool
isConsumed name = S.member name <$> getConsumed

isolated :: Infer a -> Infer (a, Consumed)
isolated action
  = do live <- getConsumed
       x <- action
       live' <- getConsumed
       setConsumed live
       return (x, live')

isolated_ :: Infer a -> Infer a
isolated_ action = fst <$> isolated action

isolateWith :: Consumed -> Infer a -> Infer (a, Consumed)
isolateWith live action
  = isolated $
    do setConsumed live
       action

------------------------
-- scope abstractions --

scoped :: TNames -> Infer a -> Infer a
scoped vars action
  = do expr <- action
       forget vars
       return expr

--------------------------------------------------------------------------
-- Tracing
--------------------------------------------------------------------------

inferTraceDoc :: (Pretty.Env -> Doc) -> Infer ()
inferTraceDoc f
 = do pretty <- getPrettyEnv
      inferTrace (show (f pretty))

inferTrace :: String -> Infer ()
inferTrace msg
 = do defs <- getCurrentDef
      trace ("Backend.C.ParcInfer: " ++ show (map defName defs) ++ ": " ++ msg) $
       return ()
