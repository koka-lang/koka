-----------------------------------------------------------------------------
-- Copyright 2020 Microsoft Corporation, Daan Leijen
--
-- This is free software; you can redistribute it and/or modify it under the
-- terms of the Apache License, Version 2.0. A copy of the License can be
-- found in the file "license.txt" at the root of this distribution.
-----------------------------------------------------------------------------
{-# LANGUAGE NamedFieldPuns, GeneralizedNewtypeDeriving  #-}

module Backend.C.Parc ( parcCore ) where

import Lib.Trace (trace)
import Control.Applicative hiding (empty)
import Control.Monad
import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.State
import Data.List ( intersperse, partition )
import Data.Maybe ( catMaybes, fromMaybe )
import Data.Char
import Data.Foldable (foldMap)
import Data.Set ( (\\) )
import qualified Data.Set as S

import Kind.Kind
import Kind.Newtypes
import Type.Type
import Type.TypeVar
import Type.Kind( getKind )
import qualified Type.Pretty as Pretty

import Lib.PPrint
import Common.Name
import Common.Range
import Common.NamePrim
import Common.Failure
import Common.Unique
import Common.Syntax

import Core.Core
import Core.Pretty
import Core.CoreVar

import Platform.Runtime (unsafePerformIO)
import qualified System.Environment as Sys

{-# NOINLINE enabled #-}
enabled :: Bool
enabled = unsafePerformIO $ do
  e <- Sys.lookupEnv "KK_PARC"
  case e of
    Nothing -> return False
    Just val -> return $ map toLower val `elem` ["1", "on", "yes", "true", "y", "t"]

--------------------------------------------------------------------------
-- Reference count transformation
--------------------------------------------------------------------------

parcCore :: Pretty.Env -> Newtypes -> Core -> Unique Core
parcCore penv newtypes core
  | not enabled = return core
  | otherwise   = do defs <- runParc penv newtypes (parcDefGroups True (coreProgDefs core))
                     trace (show (vcat (map (prettyDefGroup penv{Pretty.coreShowDef=True,Pretty.coreShowTypes=False,Pretty.fullNames=False})
                                            defs))) $
                      return core{ coreProgDefs  = defs }

--------------------------------------------------------------------------
-- Rule for ensuring a binding is live in its scope.
--------------------------------------------------------------------------

parcOwnedBindings :: TNames -> Expr -> Parc Expr
parcOwnedBindings tns expr
  = inScope tns $ do
      expr'  <- parcExpr expr
      live <- getLive
      drops  <- foldMapM genDrop (tns \\ live)
      return $ maybeStats drops expr'

--------------------------------------------------------------------------
-- Definition groups
--------------------------------------------------------------------------

parcDefGroups :: Bool -> DefGroups -> Parc DefGroups
parcDefGroups topLevel = reverseMapM (parcDefGroup topLevel)

parcDefGroup :: Bool -> DefGroup -> Parc DefGroup
parcDefGroup topLevel dg
  = case dg of
      DefRec    defs -> DefRec <$> reverseMapM (parcDef topLevel) defs
      DefNonRec def  -> DefNonRec <$> parcDef topLevel def

parcDef :: Bool -> Def -> Parc Def
parcDef topLevel def
  = (if topLevel then isolated_ else id) $
    do expr <- parcExpr (defExpr def)
       return def{defExpr=expr}

--------------------------------------------------------------------------
-- Main PARC algorithm
--------------------------------------------------------------------------

parcExpr :: Expr -> Parc Expr
parcExpr expr
  = case expr of
      TypeLam tpars body
        -> TypeLam tpars <$> parcExpr body
      TypeApp body targs
        -> (`TypeApp` targs) <$> parcExpr body
      Lam pars eff body
        -> do let free = freeLocals expr
              dups <- foldMapM useTName free
              (body', live) <- isolateWith S.empty $
                               setOwned free $        -- captured variables are owned
                               parcOwnedBindings (S.fromList pars) body
              -- dups <- foldMapM useTName live
              -- assertion "parcExpr: free==live" (free == live) $
              return (maybeStats dups $ Lam pars eff body')
      Var tname InfoNone
        -> fromMaybe expr <$> useTName tname
      Var _ _ -- InfoArity/External are not reference-counted
        -> return expr
      App fn args
        -> do args' <- reverseMapM parcExpr args
              fn'   <- parcExpr fn
              return $ App fn' args'
      Lit _
        -> return expr
      Con ctor repr
        -> return expr
      Let dgs body
        -> do body' <- inScope (bv dgs) $ parcExpr body
              dgs' <- parcDefGroups False dgs
              -- TODO: drop potential bindings
              return $ Let dgs' body'
      Case vars brs | caseIsNormalized vars brs
        -> Case vars <$> parcBranches (fv vars) brs
      Case _ _
        -> do nexpr <- normalizeCase expr
              parcExpr nexpr

parcBranches :: TNames -> [Branch] -> Parc [Branch]
parcBranches scrutinees brs
  = do live <- getLive
       branchFns <- reverseMapM (parcBranch scrutinees live) brs
       markLives scrutinees
       live' <- getLive
       mapM ($ live') branchFns

parcBranch :: TNames -> Live -> Branch -> Parc (Live -> Parc Branch)
parcBranch scrutinees live (Branch pats guards)
  = do let pvs = bv pats
       guardFns <- reverseMapM (parcGuard scrutinees pvs live) guards
       forget pvs
       return $ \c -> Branch pats <$> mapM ($c) guardFns

parcGuard :: TNames -> TNames -> Live -> Guard -> Parc (Live -> Parc Guard)
parcGuard scrutinees pvs live (Guard test expr)
  = do (expr', live') <- isolateWith live $ extendOwned pvs $ parcExpr expr
       dups <- foldMapM genDup (S.intersection pvs live')
       markLives live'
       test' <- setOwned S.empty $ parcExpr test
       return $ \matchLive -> do
         drops <- foldMapM genDrop (matchLive \\ live')  -- drop anything owned that is not alive anymore
         return $ Guard test' (maybeStats (dups ++ drops) expr')


--------------------------------------------------------------------------
-- Case normalization
--------------------------------------------------------------------------

caseIsNormalized :: [Expr] -> [Branch] -> Bool
caseIsNormalized exprs branches
  = all isExprVar exprs && all (not . mightAlias) branches
  where isExprVar Var{}   = True
        isExprVar _       = False
        isPatVar PatVar{} = True
        isPatVar _        = False
        mightAlias Branch{branchPatterns}
          = any isPatVar branchPatterns

normalizeCase :: Expr -> Parc Expr
normalizeCase Case{caseExprs,caseBranches}
  = do (vexprs,dgs) <- unzip <$> reverseMapM caseExpandExpr caseExprs
       let brs' = map (normalizeBranch vexprs) caseBranches
       return $ makeLet (catMaybes dgs) (Case vexprs brs')
normalizeCase _
  = failure "Backend.C.Parc.normalizeCase" 

-- Generate variable names for scrutinee expressions
caseExpandExpr :: Expr -> Parc (Expr, Maybe DefGroup)
caseExpandExpr x@Var{} = return (x, Nothing)
caseExpandExpr x = do name <- uniqueName "match"
                      let def = DefNonRec (makeDef name x)
                      let var = Var (TName name (typeOf x)) InfoNone
                      return (var, Just def)

normalizeBranch :: [Expr] -> Branch -> Branch
normalizeBranch vexprs br@Branch{branchPatterns, branchGuards}
  = let {-renameGuard (old, new) (Guard test expr)
          = Guard (rename old new test) (rename old new expr)
        -}
        nameMapping (PatVar old pat') var@(Var _ _) = (Just (old, var), pat')
        nameMapping pat var = (Nothing,pat)
        (newNames, pats') = unzip $ zipWith nameMapping branchPatterns vexprs
        -- newNames = [(name,v) | (name,v) <- newNames0] --catMaybes newNames
        guards' = catMaybes newNames |~> branchGuards  -- map (\g -> foldr renameGuard g newNames') branchGuards
     in Branch pats' guards'
{-
-- only safe once variable names have been uniquified
rename :: Name -> Name -> Expr -> Expr
rename old new expr
  = let renameExpr
          = rename old new
        renameName name
          = if name == old then new else name
        renameTName (TName name ty)
          = TName (renameName name) ty
        renameDef def@Def{defName,defExpr}
          = def{defName = renameName defName, defExpr = renameExpr defExpr}
        renameDefGroup dg
          = case dg of
              DefRec defs -> DefRec (map renameDef defs)
              DefNonRec def -> DefNonRec (renameDef def)
        renameGuard (Guard test expr)
          = Guard (renameExpr test) (renameExpr expr)
        renameBranch br@Branch{branchGuards}
          = br{branchGuards=map renameGuard branchGuards}
    in case expr of
         Var tn info -> Var (renameTName tn) info
         Lam tns eff expr -> Lam (map renameTName tns) eff (renameExpr expr)
         App body args -> App (renameExpr body) (map renameExpr args)
         TypeLam tvs expr -> TypeLam tvs (renameExpr expr)
         TypeApp expr tys -> TypeApp (renameExpr expr) tys
         Con tn repr -> Con (renameTName tn) repr
         Lit _ -> expr
         Let dgs expr -> Let (map renameDefGroup dgs) (renameExpr expr)
         Case exprs brs -> Case (map renameExpr exprs) (map renameBranch brs)
-}

--------------------------------------------------------------------------
-- Convenience methods for inserting PARC ops
--------------------------------------------------------------------------

useTName :: TName -> Parc (Maybe Expr)
useTName tname
  = do live <- isLive tname
       owned <- isOwned tname
       markLive tname
       if live || not owned
         then genDup tname
         else return Nothing

borrowTName :: TName -> Parc (Maybe Expr)
borrowTName tname = return Nothing

-- Generate a "drop match"
genDropMatch :: TName -> [TName] -> [TName] -> Parc Expr
genDropMatch con dups drops
  = do xdrops <- mapM genDrop drops
       xdups  <- mapM genDup dups
       cdrop  <- genDrop con
       return $ makeIfExpr (genIsUnique con)
                  (makeStats (catMaybes xdrops ++ [genFree con]))
                  (makeStats (catMaybes (xdups ++ [cdrop])))

genKeepMatch :: TName -> [TName] -> [TName] -> Parc Expr
genKeepMatch con dups drops
  = do xdups  <- mapM genDup dups
       cdrop  <- genDrop con
       return $ makeStats (catMaybes (xdups ++ [cdrop]))

-- Generate a "reuse match"
genReuseMatch :: TName -> [TName] -> [TName] -> Parc Expr
genReuseMatch con dups drops
 = do xdrops <- mapM genDrop drops
      xdups  <- mapM genDup dups
      cdrop  <- genDrop con
      return $ makeIfExpr (genIsUnique con)
                 (makeStats (catMaybes xdrops ++ [genReuse con]))
                 (makeStats (catMaybes (xdups ++ [cdrop]) ++ [genNoReuse]))

-- Generate a test if a (locally bound) name is unique
genIsUnique :: TName -> Expr
genIsUnique tname
  = App (Var (TName nameIsUnique funTp) (InfoExternal [(C, "constructor_is_unique(#1)")]))
        [Var tname InfoNone]
  where
    tp    = typeOf tname
    funTp = TFun [(nameNil,tp)] typeTotal typeBool


-- Generate a free of a constructor
genFree :: TName -> Expr
genFree tname
  = App (Var (TName nameFree funTp) (InfoExternal [(C, "constructor_free(#1)")]))
        [Var tname InfoNone]
  where
    tp    = typeOf tname
    funTp = TFun [(nameNil,tp)] typeTotal typeUnit

-- Generate a reuse of a constructor
genReuse :: TName -> Expr
genReuse tname
  = App (Var (TName nameReuse funTp) (InfoExternal [(C, "constructor_reuse(#1)")]))
        [Var tname InfoNone]
  where
    tp    = typeOf tname
    funTp = TFun [(nameNil,tp)] typeTotal typeReuse


-- Generate a reuse of a constructor
genNoReuse :: Expr
genNoReuse
  = App (Var (TName nameNoReuse funTp) (InfoArity 0 0)) []
  where
    funTp = TFun [] typeTotal typeReuse

-- Generate a dup/drop over a given (locally bound) name
-- May return Nothing if the type never needs a dup/drop (like an `int` or `bool`)
genDupDrop :: Bool -> TName -> Parc (Maybe Expr)
genDupDrop isDup tname
  = do let tp = typeOf tname
       mbRepr <- getDataDefRepr tp
       case mbRepr of
         Just (dataDef,dataRepr)
           -> case dataDef of
                 DataDefValue _ 0 -> return Nothing    -- no need to dup/drop a value type with no pointer fields (like int)
                 _ -> return (Just (App (dupDropFun isDup tp) [Var tname InfoNone]))
         _ -> return Nothing

genDup  = genDupDrop True
genDrop = genDupDrop False

-- get the dup/drop function
dupDropFun :: Bool -> Type -> Expr
dupDropFun isDup tp
  = Var (TName name coerceTp) (InfoExternal [(C, (if isDup then "dup" else "drop") ++ "(#1)")])
  where
    name = if isDup then nameDup else nameDrop
    coerceTp = TFun [(nameNil,tp)] typeTotal (if isDup then tp else typeUnit)

dupFun  = dupDropFun True
dropFun = dupDropFun False

--------------------------------------------------------------------------
-- Utilities for readability
--------------------------------------------------------------------------

reverseMapM :: Monad m => (a -> m b) -> [a] -> m [b]
reverseMapM action args =
  do args' <- mapM action (reverse args)
     return $ reverse args'

-- for mapping over a set and collecting the results into a list.
foldMapM :: (Monad m, Foldable t) => (a -> m b) -> t a -> m [b]
foldMapM f = foldr merge (return [])
  where merge x r = do y <- f x 
                       (y:) <$!> r

maybeStats :: [Maybe Expr] -> Expr -> Expr
maybeStats xs expr 
  = makeStats (catMaybes xs ++ [expr])

--------------------------------------------------------------------------
-- Parc monad
--------------------------------------------------------------------------

-----------------
-- definitions --

type Owned = TNames
data Env = Env { currentDef :: [Def],
                 prettyEnv :: Pretty.Env,
                 newtypes  :: Newtypes,
                 owned     :: Owned
               }

type Live = TNames
data ParcState = ParcState { uniq :: Int,
                             live :: Live
                           }

type ParcM a = ReaderT Env (State ParcState) a
newtype Parc a = Parc (ParcM a)
  deriving (Functor, Applicative, Monad, MonadReader Env, MonadState ParcState)

instance HasUnique Parc where
  updateUnique f = do { old <- getUniq; modifyUniq f; return old }
  setUnique = setUniq

withEnv :: (Env -> Env) -> Parc a -> Parc a
withEnv = local

getEnv :: Parc Env
getEnv = ask

updateSt :: (ParcState -> ParcState) -> Parc ()
updateSt = modify

getSt :: Parc ParcState
getSt = get

runParc :: Pretty.Env -> Newtypes -> Parc a -> Unique a
runParc penv newtypes (Parc action)
  = withUnique $ \u ->
      let env = Env [] penv newtypes S.empty
          st = ParcState u S.empty
          (val, st') = runState (runReaderT action env) st
       in (val, uniq st')

-------------------
-- env accessors --

getCurrentDef :: Parc [Def]
getCurrentDef = currentDef <$> getEnv

withCurrentDef :: ([Def] -> [Def]) -> Parc a -> Parc a
withCurrentDef f = withEnv (\e -> e { currentDef = f (currentDef e) })

--

getPrettyEnv :: Parc Pretty.Env
getPrettyEnv = prettyEnv <$> getEnv

withPrettyEnv :: (Pretty.Env -> Pretty.Env) -> Parc a -> Parc a
withPrettyEnv f = withEnv (\e -> e { prettyEnv = f (prettyEnv e) })

--

getNewtypes :: Parc Newtypes
getNewtypes = newtypes <$> getEnv

withNewtypes :: (Newtypes -> Newtypes) -> Parc a -> Parc a
withNewtypes f = withEnv (\e -> e { newtypes = f (newtypes e) })

--

getOwned :: Parc Owned
getOwned = owned <$> getEnv

withOwned :: (Owned -> Owned) -> Parc a -> Parc a
withOwned f = withEnv (\e -> e { owned = f (owned e) })

---------------------
-- state accessors --

getUniq :: Parc Int
getUniq = uniq <$> getSt

modifyUniq :: (Int -> Int) -> Parc ()
modifyUniq f = updateSt (\s -> s { uniq = f (uniq s) })

setUniq :: Int -> Parc ()
setUniq = modifyUniq . const

--

getLive :: Parc Live
getLive = live <$> getSt

modifyLive :: (Live -> Live) -> Parc ()
modifyLive f = updateSt (\s -> s { live = f (live s) })

setLive :: Live -> Parc ()
setLive = modifyLive . const

-----------------------------
-- owned name abstractions --

isOwned :: TName -> Parc Bool
isOwned tn = S.member tn <$> getOwned

setOwned :: TNames -> Parc a -> Parc a
setOwned = withOwned . const

extendOwned :: Owned -> Parc a -> Parc a
extendOwned = withOwned . S.union

-------------------------------
-- live set abstractions --

markLive :: TName -> Parc ()
markLive = modifyLive . S.insert

markLives :: TNames -> Parc ()
markLives = modifyLive . S.union

forget :: TNames -> Parc ()
forget tns = modifyLive (\\ tns)

isLive :: TName -> Parc Bool
isLive name = S.member name <$> getLive

isolated :: Parc a -> Parc (a, Live)
isolated action
  = do live <- getLive
       x <- action
       live' <- getLive
       setLive live
       return (x, live')

isolated_ :: Parc a -> Parc a
isolated_ action = fst <$> isolated action

isolateWith :: Live -> Parc a -> Parc (a, Live)
isolateWith live action 
  = isolated $ 
    do setLive live 
       action

------------------------
-- scope abstractions --

inScope :: TNames -> Parc a -> Parc a
inScope tns action
  = do r <- extendOwned tns action
       forget tns
       return r

--------------------------------------------------------------------------
-- Tracing
--------------------------------------------------------------------------

parcTraceDoc :: (Pretty.Env -> Doc) -> Parc ()
parcTraceDoc f
 = do pretty <- getPrettyEnv
      parcTrace (show (f pretty))

parcTrace :: String -> Parc ()
parcTrace msg
 = do defs <- getCurrentDef
      trace ("Core.Parc: " ++ show (map defName defs) ++ ": " ++ msg) $ return ()

----------------

getDataDefRepr :: Type -> Parc (Maybe (DataDef,DataRepr))
getDataDefRepr tp
  = case extractDataDefType tp of
      Nothing -> return (Just (DataDefNormal,DataNormal))
      Just name -> do newtypes <- getNewtypes
                      case newtypesLookupAny name newtypes of
                        Nothing -> failure $ "Core.Parc.getDataInfo: cannot find type: " ++ show name
                        Just di -> return (Just (dataInfoDef di, fst (getDataRepr di)))

extractDataDefType :: Type -> Maybe Name
extractDataDefType tp
  = case expandSyn tp of
      TApp t _      -> extractDataDefType t
      TForall _ _ t -> extractDataDefType t
      TCon tc       -> Just (typeConName tc)
      _             -> Nothing
