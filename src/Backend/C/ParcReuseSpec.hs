-----------------------------------------------------------------------------
-- Copyright 2020 Microsoft Corporation, Daan Leijen, Alex Reinking
--
-- This is free software; you can redistribute it and/or modify it under the
-- terms of the Apache License, Version 2.0. A copy of the License can be
-- found in the file "license.txt" at the root of this distribution.
-----------------------------------------------------------------------------
{-# LANGUAGE NamedFieldPuns, GeneralizedNewtypeDeriving  #-}

-----------------------------------------------------------------------------
-- constructor reuse analysis
-----------------------------------------------------------------------------

module Backend.C.ParcReuseSpec( parcReuseSpecialize ) where

import Lib.Trace (trace)
import Control.Monad
import Control.Monad.Reader
import Control.Monad.State

import Type.Type
import qualified Type.Pretty as Pretty

import Lib.PPrint
import Common.NamePrim( nameConFieldsAssign, nameAllocAt, nameReuseIsValid )
import Common.Failure
import Common.Unique
import Common.Syntax

import Core.Core
import Core.Pretty

--------------------------------------------------------------------------
-- Specialize reuse applications
--------------------------------------------------------------------------

parcReuseSpecialize :: Pretty.Env -> Core -> Unique Core
parcReuseSpecialize penv core
  = do defs <- runReuse penv (ruDefGroups (coreProgDefs core))
       return core{coreProgDefs=defs}
  
--------------------------------------------------------------------------
-- Definition groups
--------------------------------------------------------------------------

ruDefGroups :: DefGroups -> Reuse DefGroups
ruDefGroups dgs = mapM ruDefGroup dgs

ruDefGroup :: DefGroup -> Reuse DefGroup
ruDefGroup dg
  = case dg of
      DefRec    defs -> DefRec    <$> mapM ruDef defs
      DefNonRec def  -> DefNonRec <$> ruDef def

ruDef :: Def -> Reuse Def
ruDef def
  = withCurrentDef def $
    do expr <- ruExpr (defExpr def)
       return def{defExpr=expr}

--------------------------------------------------------------------------
-- 
--------------------------------------------------------------------------

ruExpr :: Expr -> Reuse Expr
ruExpr expr
  = case expr of
      App alloc@(Var allocAt _) [Var reuseName (InfoReuse pat), conApp]  | nameAllocAt == getName allocAt
        -> ruSpecialize alloc reuseName pat conApp
      TypeLam tpars body
        -> TypeLam tpars <$> ruExpr body
      TypeApp body targs
        -> (`TypeApp` targs) <$> ruExpr body
      Lam pars eff body
        -> Lam pars eff <$> ruExpr body
      App fn args
        -> liftM2 App (ruExpr fn) (mapM ruExpr args)

      Let [] body
        -> ruExpr body
      Let (DefNonRec def:dgs) body
        -> liftM2 makeLet' (ruDef def) (ruExpr (Let dgs body))
           where makeLet' def' = makeLet [DefNonRec def']
      Let _ _
        -> failure "Backend.C.ParcReuseSpec.ruExpr"

      Case scrutinees branches
        -> liftM2 Case (mapM ruExpr scrutinees) (ruBranches branches)

      -- Var, Lit, Con
      _ -> return expr


ruBranches :: [Branch] -> Reuse [Branch]
ruBranches branches
  = mapM ruBranch branches
    
ruBranch :: Branch -> Reuse Branch
ruBranch (Branch pats guards)
  = do guards' <- mapM ruGuard guards      
       return (Branch pats guards')
  
ruGuard :: Guard -> Reuse Guard
ruGuard (Guard test expr)  
  = do test' <- ruExpr test
       expr' <- ruExpr expr
       return (Guard test' expr')

--------------------------------------------------------------------------
-- Specialization
--------------------------------------------------------------------------

ruSpecialize :: Expr -> TName -> Pattern -> Expr -> Reuse Expr
ruSpecialize allocAt reuseName reusePat conApp
  = -- TODO: generate reuse specialized code by matching reusePat with the conApp
    -- conApp will be (App (Con _ _) [args]) or (App (TApp (Con _ _) targs) args))
    return (App allocAt [Var reuseName (InfoReuse reusePat), conApp])
  

-- generates: if (reuseName != NULL) then onValid else onInvalid  
genReuseIfValid :: TName -> Expr -> Expr -> Expr
genReuseIfValid reuseName onValid onInvalid
  = makeIfExpr (genReuseIsValid reuseName) onValid onInvalid
    
genReuseIsValid :: TName -> Expr
genReuseIsValid reuseName 
  = App (Var (TName nameReuseIsValid typeReuseIsValid) (InfoExternal [(C,"#1!=NULL")])) [Var reuseName InfoNone]
  where
    typeReuseIsValid = TFun [(nameNil,typeReuse)] typeTotal typeBool

-- genConFieldsAssign tp conName reuseName [(field1,expr1)...(fieldN,exprN)]    
-- generates:  c = (conName*)reuseName; c->field1 := expr1; ... ; c->fieldN := exprN; (tp*)(c) 
genConFieldsAssign :: Type -> TName -> TName -> [(Name,Expr)] -> Expr
genConFieldsAssign resultType conName reuseName fieldExprs
  = App (Var (TName nameConFieldsAssign typeConFieldsAssign) (InfoArity 0 (length fieldExprs + 1))) 
        ([Var reuseName (InfoConField conName nameNil)] ++ map snd fieldExprs)
  where
    fieldTypes = [(name,typeOf expr) | (name,expr) <- fieldExprs]
    typeConFieldsAssign = TFun ([(nameNil,typeOf reuseName)] ++ fieldTypes) typeTotal resultType


--------------------------------------------------------------------------
-- Utilities for readability
--------------------------------------------------------------------------

-- create a unique name specific to this module
uniqueTName :: Type -> Reuse TName
uniqueTName tp = (`TName` tp) <$> uniqueName "ru"


--------------------------------------------------------------------------
-- Reuse monad
--------------------------------------------------------------------------

-----------------
-- definitions --

data Env = Env { currentDef :: [Def],
                 prettyEnv :: Pretty.Env                 
               }

data ReuseState = ReuseState { uniq :: Int }

type ReuseM a = ReaderT Env (State ReuseState) a

newtype Reuse a = Reuse (ReuseM a)
  deriving (Functor, Applicative, Monad, MonadReader Env, MonadState ReuseState)

instance HasUnique Reuse where
  updateUnique f = do { old <- getUniq; modifyUniq f; return old }
  setUnique = setUniq

withEnv :: (Env -> Env) -> Reuse a -> Reuse a
withEnv = local

getEnv :: Reuse Env
getEnv = ask

updateSt :: (ReuseState -> ReuseState) -> Reuse ()
updateSt = modify

getSt :: Reuse ReuseState
getSt = get

runReuse :: Pretty.Env -> Reuse a -> Unique a
runReuse penv (Reuse action)
  = withUnique $ \u ->
      let env = Env [] penv
          st = ReuseState u
          (val, st') = runState (runReaderT action env) st
       in (val, uniq st')


-------------------
-- env accessors --

getCurrentDef :: Reuse [Def]
getCurrentDef = currentDef <$> getEnv

withCurrentDef :: Def -> Reuse a -> Reuse a
withCurrentDef def = withEnv (\e -> e { currentDef = def : currentDef e })

--

getPrettyEnv :: Reuse Pretty.Env
getPrettyEnv = prettyEnv <$> getEnv

withPrettyEnv :: (Pretty.Env -> Pretty.Env) -> Reuse a -> Reuse a
withPrettyEnv f = withEnv (\e -> e { prettyEnv = f (prettyEnv e) })


---------------------
-- state accessors --

getUniq :: Reuse Int
getUniq = uniq <$> getSt

modifyUniq :: (Int -> Int) -> Reuse ()
modifyUniq f = updateSt (\s -> s { uniq = f (uniq s) })

setUniq :: Int -> Reuse ()
setUniq = modifyUniq . const


--------------------------------------------------------------------------
-- Tracing
--------------------------------------------------------------------------

ruTraceDoc :: (Pretty.Env -> Doc) -> Reuse ()
ruTraceDoc f
 = do pretty <- getPrettyEnv
      ruTrace (show (f pretty))

ruTrace :: String -> Reuse ()
ruTrace msg
 = do defs <- getCurrentDef
      trace ("Backend.C.ParcReuseSpec: " ++ show (map defName defs) ++ ": " ++ msg) $
        return ()
