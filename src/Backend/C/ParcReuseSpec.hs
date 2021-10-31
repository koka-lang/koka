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
import Common.NamePrim( nameConFieldsAssign, nameAllocAt, nameReuseIsValid, nameDup, nameSetTag, nameConTagFieldsAssign )
import Common.Failure
import Common.Unique
import Common.Syntax

import Core.Core
import Core.Pretty
import Kind.Newtypes (Newtypes, newtypesLookupAny)

--------------------------------------------------------------------------
-- Specialize reuse applications
--------------------------------------------------------------------------

parcReuseSpecialize :: Pretty.Env -> Newtypes -> Core -> Unique Core
parcReuseSpecialize penv newtypes core
  = do defs <- runReuse penv newtypes (ruDefGroups (coreProgDefs core))
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
      App alloc@(Var allocAt _) [reuse@(Var reuseName info), conApp]  | nameAllocAt == getName allocAt
        -> do conApp' <- ruExpr conApp
              mbExpr  <- ruSpecialize reuseName info conApp'
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

-- | Try to specialize an allocation at a reuse.
-- We will only do this if we can get the necessary ConInfo 
-- and save at least a fourth of the necessary assignments.
ruSpecialize :: TName -> VarInfo -> Expr -> Reuse (Maybe Expr)
ruSpecialize reuseName info conApp
  = case (conApp,info) of
      (App con args, InfoReuse (PatCon{patConName, patConPatterns}))
        -> case extractCon con of
             Just (cname, repr)
               -> do mci <- getConInfo (typeOf con) (getName cname)
                     let matches = zipWith tryMatch args patConPatterns
                         needsTag = patConName /= cname
                         oldTagBenefit = if needsTag then 0 else 1
                         specialize = oldTagBenefit + length (filter isMatch matches) >= 1 + ((1 + length args) `div` 4)
                     case (mci, specialize) of
                       (Just ci, True)
                         -> Just <$> ruSpecCon reuseName cname ci needsTag (conTag repr) (typeOf conApp) (App con) matches
                       _ -> return Nothing
             Nothing -> return Nothing
      _ -> return Nothing
  where
    extractCon (Con cname repr) = Just (cname, repr)
    extractCon (TypeApp (Con cname repr) _) = Just (cname, repr)
    extractCon _ = Nothing

-- | Move dups before the allocation and emit:
-- if(reuseName != NULL) { set tag and fields }
-- else { allocate constructor without reuse }
ruSpecCon :: TName -> TName -> ConInfo -> Bool -> Int -> Type -> ([Expr] -> Expr) -> [Match] -> Reuse Expr
ruSpecCon reuseName conName conInfo needsTag tag resultType makeConApp matches
  = do (defss, assigns) <- unzip <$> mapM ruToAssign matches
       let fields = map fst (conInfoParams conInfo)
           nonMatching = [(name,expr) | (name,(expr,isMatch)) <- zip fields assigns, not isMatch]
           reuseExpr = if needsTag then genConTagFieldsAssign resultType conName reuseName tag nonMatching
                                   else genConFieldsAssign resultType conName reuseName nonMatching
           specExpr = makeIfExpr (genReuseIsValid reuseName) reuseExpr (makeConApp (map fst assigns))
       return (makeLet (concat defss) specExpr)

data Match
  = Match { pre :: [Expr], arg :: Expr }
  | NoMatch { arg :: Expr }

isMatch :: Match -> Bool
isMatch Match{} = True
isMatch _       = False

-- | Determine if we write the same value back that is currently in this place.
-- This is the case if the expression is:
--   - the same variable as in the pattern
--   - a dup on the same variable as in the pattern
--   - a unit constructor that was also in the pattern
-- We do not check for non-unit constructors because these may be reused
-- (and specialized) as well.
tryMatch :: Expr -> Pattern -> Match
tryMatch expr pat
  = case (expr,pat) of
      (Var vname _, PatVar pname _)  -- direct match (x == x)
         | vname == pname -> Match [] expr
      (App (Var dname _) [v@(Var vname _)], PatVar pname _)  -- match dup (x == dup(x))
         | getName dname == nameDup && vname == pname -> Match [expr] v
      (Con cname _, PatCon{patConName,patConPatterns = []})
         | cname == patConName -> Match [] expr
      (Con cname _, PatVar pname (PatCon{patConName,patConPatterns = []}))
         | cname == patConName -> Match [] expr
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

genReuseIsValid :: TName -> Expr
genReuseIsValid reuseName
  = App (Var (TName nameReuseIsValid typeReuseIsValid) (InfoExternal [(C CDefault,"kk_likely(#1!=NULL)")])) [Var reuseName InfoNone]
  where
    typeReuseIsValid = TFun [(nameNil,typeReuse)] typeTotal typeBool

-- genConFieldsAssign tp conName reuseName [(field1,expr1)...(fieldN,exprN)]
-- generates:  c = (conName*)reuseName; c->field1 := expr1; ... ; c->fieldN := exprN; (tp*)(c)
genConTagFieldsAssign :: Type -> TName -> TName -> Int -> [(Name,Expr)] -> Expr
genConTagFieldsAssign resultType conName reuseName tag fieldExprs
  = App (Var (TName nameConTagFieldsAssign typeConFieldsAssign) (InfoArity 0 (length fieldExprs + 1)))
        ([Var reuseName (InfoConField conName nameNil), Var (TName (newName (show tag)) typeUnit) InfoNone] ++ map snd fieldExprs)
  where
    fieldTypes = [(name,typeOf expr) | (name,expr) <- fieldExprs]
    typeConFieldsAssign = TFun ([(nameNil,typeOf reuseName), (nameNil, typeUnit)] ++ fieldTypes) typeTotal resultType

-- genConTagFieldsAssign tp conName reuseName [(field1,expr1)...(fieldN,exprN)]
-- generates:  c = (conName*)reuseName; c->field1 := expr1; ... ; c->fieldN := exprN; (tp*)(c)
genConFieldsAssign :: Type -> TName -> TName -> [(Name,Expr)] -> Expr
genConFieldsAssign resultType conName reuseName fieldExprs
  = App (Var (TName nameConFieldsAssign typeConFieldsAssign) (InfoArity 0 (length fieldExprs + 1)))
        (Var reuseName (InfoConField conName nameNil) : map snd fieldExprs)
  where
    fieldTypes = [(name,typeOf expr) | (name,expr) <- fieldExprs]
    typeConFieldsAssign = TFun ((nameNil,typeOf reuseName) : fieldTypes) typeTotal resultType

getConInfo :: Type -> Name -> Reuse (Maybe ConInfo)
getConInfo dataType conName
  = do newtypes <- getNewtypes
       let mdataName = extractDataName dataType
       let mdataInfo = (`newtypesLookupAny` newtypes) =<< mdataName
       case filter (\ci -> conInfoName ci == conName) . dataInfoConstrs <$> mdataInfo of
         Just (ci:_) -> pure $ Just ci
         _ -> pure Nothing
  where
    extractDataName :: Type -> Maybe Name
    extractDataName tp
      = case expandSyn tp of
          TFun _ _ t -> extractDataName t
          TCon tc    -> Just (typeConName tc)
          _          -> Nothing

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
                 newtypes :: Newtypes,
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

runReuse :: Pretty.Env -> Newtypes -> Reuse a -> Unique a
runReuse penv newtypes (Reuse action)
  = withUnique $ \u ->
      let env = Env [] newtypes penv
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

getNewtypes :: Reuse Newtypes
getNewtypes = newtypes <$> getEnv

withNewtypes :: (Newtypes -> Newtypes) -> Reuse a -> Reuse a
withNewtypes f = withEnv (\e -> e { newtypes = f (newtypes e) })

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
