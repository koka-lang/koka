-----------------------------------------------------------------------------
-- Copyright 2020-2021, Microsoft Research, Daan Leijen, Alex Reinking, Anton Lorenzen
--
-- This is free software; you can redistribute it and/or modify it under the
-- terms of the Apache License, Version 2.0. A copy of the License can be
-- found in the LICENSE file at the root of this distribution.
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
import Common.NamePrim( nameConFieldsAssign, nameAllocAt, nameReuseIsValid, nameDup )
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
      App alloc@(Var allocAt _) [reuse@(Var reuseName (InfoReuse pat)), conApp]  | nameAllocAt == getName allocAt
        -> do conApp' <- ruExpr conApp
              mbExpr  <- ruSpecialize reuseName pat conApp'
              case mbExpr of
                Just newExpr -> return newExpr
                Nothing      -> return (App alloc [reuse, conApp'])
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

ruSpecialize :: TName -> Pattern -> Expr -> Reuse (Maybe Expr)
ruSpecialize reuseName reusePat conApp
  = -- TODO: generate reuse specialized code by matching reusePat with the conApp
    -- conApp will be (App (Con _ _) [args]) or (App (TApp (Con _ _) targs) args))
    --  Cons@r(2,dup(xs)) ~> dup(xs); Cons@r(2,xs)
    --                    ~> dup(xs);
    --                       if (r!=NULL) then { r->tail = xs; r->head = 2; r }
    --                                    else { Cons(2,xs)  ===  p = alloc(n); p->tail = xs; p->head=2; p }
    {-
    r@ C' p1 ... pN
    C a1 ... aN

    *is C == C' ?
    *match each p_i with a_i: if p_i is a variable x  and a_i == x or dup(x)  -> good
    *profitable? if at least one field is reused? other measures?
    *bind each argument that is not a "value"
    -}
    case (conApp,reusePat) of
      (App con@(Con cname repr) args, PatCon{patConName,patConPatterns,patConInfo})
        | cname == patConName -> ruSpecCon reuseName cname patConInfo (typeOf conApp) (App con) args patConPatterns
      (App con@(TypeApp (Con cname repr) targs) args, PatCon{patConName,patConPatterns,patConInfo})
        | cname == patConName -> ruSpecCon reuseName cname patConInfo (typeOf conApp) (App con) args patConPatterns
      _ -> return Nothing

ruSpecCon :: TName -> TName -> ConInfo -> Type -> ([Expr] -> Expr) -> [Expr] -> [Pattern] -> Reuse (Maybe Expr)
ruSpecCon reuseName conName conInfo resultType makeConApp args pats
  = let matches = map tryMatch (zip args pats)
        specialize = (length (filter isMatch matches) >= 1 + (length args `div` 4))   -- more than 1/4th matches
    in if (not specialize) then return Nothing else
        do xs <- mapM ruToAssign matches
           let fields     = map fst (conInfoParams conInfo)
               (defss,assigns) = unzip xs
               reuseExpr = genConFieldsAssignEx resultType conName reuseName (zip fields assigns)
               specExpr  = genReuseIfValid reuseName reuseExpr (makeConApp (map fst assigns))
           return (Just (makeLet (concat defss) specExpr))

data Match = Match { pre :: [Expr], arg :: Expr }
           | NoMatch { arg :: Expr }

isMatch (Match{}) = True
isMatch _         = False

tryMatch :: (Expr,Pattern) -> Match
tryMatch (expr,pat)
  = case (expr,pat) of
      (Var vname _, PatVar pname _)  -- direct match (x == x)
         | vname == pname -> Match [] expr
      (App (Var dname _) [v@(Var vname _)], PatVar pname _)  -- match dup (x == dup(x))
         | getName dname == nameDup && vname == pname -> Match [expr] v
      (Con cname _, PatCon{patConName,patConPatterns = []}) 
         | cname == patConName -> Match [] expr
      (Con cname _, PatVar pname (PatCon{patConName,patConPatterns = []})) 
         | cname == patConName -> Match [] (Var pname InfoNone)
      -- (App con@(Con cname repr) args, PatCon{patConName,patConPatterns,patConInfo})
      _ -> NoMatch expr

ruToAssign :: Match -> Reuse ([DefGroup],(Expr,Bool {-is match?-}))
ruToAssign (Match pres arg)  = return ([DefNonRec (makeDef nameNil pre) | pre <- pres],(arg,True))
ruToAssign (NoMatch expr)
  = if (isTotal expr)
     then return ([],(expr,False))
     else do name <- uniqueName "ru"
             let def = DefNonRec (makeDef name expr)
             let var = Var (TName name (typeOf expr)) InfoNone
             return ([def],(var,False))

-- generates: if (reuseName != NULL) then onValid else onInvalid
genReuseIfValid :: TName -> Expr -> Expr -> Expr
genReuseIfValid reuseName onValid onInvalid
  = makeIfExpr (genReuseIsValid reuseName) onValid onInvalid

genReuseIsValid :: TName -> Expr
genReuseIsValid reuseName
  = App (Var (TName nameReuseIsValid typeReuseIsValid) (InfoExternal [(C,"kk_likely(#1!=NULL)")])) [Var reuseName InfoNone]
  where
    typeReuseIsValid = TFun [(nameNil,typeReuse)] typeTotal typeBool

genConFieldsAssignEx :: Type -> TName -> TName -> [(Name,(Expr,Bool))] -> Expr
genConFieldsAssignEx resultType conName reuseName fieldExprs
  = genConFieldsAssign resultType conName reuseName $
    [(name,expr) | (name,(expr,isMatch)) <- fieldExprs, not isMatch]  -- only assign not matching

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
