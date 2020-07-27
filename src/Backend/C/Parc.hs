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
-- Rule for ensuring a binding is consumed in its scope.
--------------------------------------------------------------------------

parcOwnedBindings :: [TName] -> Expr -> Parc Expr
parcOwnedBindings tns expr
  = withOwned (S.fromList tns) $ do
      expr'  <- parcExpr expr
      unused <- filterM (fmap not . isConsumed) tns
      drops  <- mapM genDrop unused
      return $ maybeStats drops expr'

--------------------------------------------------------------------------
-- Definition groups
--------------------------------------------------------------------------

parcDefGroups :: Bool -> DefGroups -> Parc DefGroups
parcDefGroups topLevel defGroups
  = reverseMapM (parcDefGroup topLevel) defGroups

parcDefGroup :: Bool -> DefGroup -> Parc DefGroup
parcDefGroup topLevel dg
  = case dg of
      DefRec    defs -> DefRec <$> reverseMapM (parcDef topLevel) defs
      DefNonRec def  -> DefNonRec <$> parcDef topLevel def

parcDef :: Bool -> Def -> Parc Def
parcDef topLevel def
  = (if topLevel then isolated else id) $
    do expr <- parcExpr (defExpr def)
       return def{defExpr=expr}
  where
    isolated action
      = do (x,inuse) <- isolateConsumed action
           return x

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
        -> do (body', consumed) <- isolateBody $ parcOwnedBindings pars body
              let captures = S.toList $ consumed \\ S.fromList pars
              dups <- dupTNames (zip captures (repeat InfoNone))
              mapM_ consume captures
              return $ Lam pars eff (maybeStats dups body')
      Var tname info
        -> fromMaybe expr <$> dupTName (tname, info)
      App fn args
        -> do args' <- reverseMapM parcExpr args
              fn'   <- parcExpr fn
              return $ App fn' args'
      Lit _
        -> return expr
      Con ctor repr
        -> return expr
      Let dgs body
        -> do body' <- parcExpr body
              dgs' <- parcDefGroups False dgs
              return $ Let dgs' body'
      Case exprs brs | caseIsNormalized exprs brs
        ->  do let scrutinees = map (\(Var tn _) -> tn) exprs
               -- dup the scrutinees if they're used after the match
               (exprs', _) <- isolateConsumed $ reverseMapM parcExpr exprs
               brs' <- parcBranches scrutinees brs
               -- mark the scrutinees as consumed
               mapM_ consume scrutinees
               return $ Case exprs' brs'
      Case _ _
        -> parcExpr =<< normalizeCase expr

parcBranches :: [TName] -> [Branch] -> Parc [Branch]
parcBranches scrutinees brs
  = do let scrutinees' = S.fromList scrutinees
       brs' <- mapM (withOwned scrutinees' . parcBranch) brs
       return brs'

parcBranch :: Branch -> Parc Branch
parcBranch b@(Branch pats guards)
  = do let pvs = bv pats
       guards' <- mapM (parcGuard pvs) guards
       return b

parcGuard :: TNames -> Guard -> Parc Guard
parcGuard pvs g@(Guard test expr)
  = do (test', _) <- isolateConsumed $ noneOwned $ parcExpr test
       (expr', cij) <- isolateConsumed $ withOwned pvs $ parcExpr expr
       let needsDups = S.intersection pvs cij

       return g

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

-- Generate variable names for scrutinee expressions
caseExpandExpr :: Expr -> Parc (Expr, Maybe DefGroup)
caseExpandExpr x@Var{} = return (x, Nothing)
caseExpandExpr x = do name <- uniqueName "match"
                      let def = DefNonRec (makeDef name x)
                      let var = Var (TName name (typeOf x)) InfoNone
                      return (var, Just def)

normalizeBranch :: [Expr] -> Branch -> Branch
normalizeBranch vexprs br@Branch{branchPatterns, branchGuards}
  = let renameGuard (old, new) (Guard test expr)
          = Guard (rename old new test) (rename old new expr)
        nameMapping pat (Var (TName new _) _)
          = case pat of
              PatVar (TName old _) pat'
                -> (Just (old, new), pat')
              _ -> (Nothing, pat)
        (newNames, pats') = unzip $ zipWith nameMapping branchPatterns vexprs
        newNames' = catMaybes newNames
        guards' = map (\g -> foldr renameGuard g newNames') branchGuards
     in Branch pats' guards'

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

--------------------------------------------------------------------------
-- Convenience methods for inserting PARC ops
--------------------------------------------------------------------------

dupTNames :: [(TName,VarInfo)] -> Parc [Maybe Expr]
dupTNames = mapM dupTName

dupTName :: (TName,VarInfo) -> Parc (Maybe Expr)
dupTName (tname,InfoNone)
  = do needsDup <- isConsumed tname
       if needsDup
        then genDup tname
        else do consume tname
                return Nothing
dupTName _
  = return Nothing

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

genDup  tname = genDupDrop True tname
genDrop tname = genDupDrop False tname

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


dupFun tp  = dupDropFun True tp
dropFun tp = dupDropFun False tp

dupDropFun isDup tp
  = Var (TName name coerceTp) (InfoExternal [(C, (if isDup then "dup" else "drop") ++ "(#1)")])
  where
    name = if isDup then nameDup else nameDrop
    coerceTp = TFun [(nameNil,tp)] typeTotal (if isDup then tp else typeUnit)

--------------------------------------------------------------------------
-- Utilities for readability
--------------------------------------------------------------------------

reverseMapM :: Monad m => (a -> m b) -> [a] -> m [b]
reverseMapM action args =
  do args' <- mapM action (reverse args)
     return $ reverse args'

maybeStats :: [Maybe Expr] -> Expr -> Expr
maybeStats xs expr = makeStats (catMaybes xs ++ [expr])

--------------------------------------------------------------------------
-- Parc monad
--------------------------------------------------------------------------

type ParcM a = ReaderT Env (State ParcState) a
newtype Parc a = Parc (ParcM a)
  deriving (Functor, Applicative, Monad, MonadReader Env, MonadState ParcState)

type Owned = S.Set TName
data Env = Env { currentDef :: [Def],
                 prettyEnv :: Pretty.Env,
                 newtypes  :: Newtypes,
                 owned     :: Owned
               }

type Consumed = S.Set TName
data ParcState = ParcState { uniq :: Int,
                             consumed :: Consumed
                           }

runParc :: Pretty.Env -> Newtypes -> Parc a -> Unique a
runParc penv newtypes (Parc action)
  = withUnique $ \u ->
      let env = Env [] penv newtypes S.empty
          st = ParcState u S.empty
          (val, st') = runState (runReaderT action env) st
       in (val, uniq st')

instance HasUnique Parc where
  updateUnique f = modify (\s -> s { uniq = f (uniq s) }) >> gets uniq
  setUnique i = modify (\s -> s { uniq = i })

withEnv :: (Env -> Env) -> Parc a -> Parc a
withEnv = local

getEnv :: Parc Env
getEnv = ask

updateSt :: (ParcState -> ParcState) -> Parc ParcState
updateSt f = modify f >> get

getSt :: Parc ParcState
getSt = get


-----------------------
-- owned names

noneOwned :: Parc a -> Parc a
noneOwned = withEnv (\env -> env { owned = S.empty })

withOwned :: TNames -> Parc a -> Parc a
withOwned tnames
  = withEnv (\env -> env{ owned = S.union (owned env) tnames })

getOwned :: Parc Owned
getOwned = owned <$> getEnv

isOwned :: TName -> Parc Bool
isOwned tn = S.member tn <$> getOwned

-----------------------
-- in-use sets

getConsumed :: Parc Consumed
getConsumed = consumed <$> getSt

setConsumed :: Consumed -> Parc ()
setConsumed consumed'
 = do updateSt (\st -> st{ consumed = consumed' })
      return ()

consume :: TName -> Parc ()
consume name
  = do updateSt (\st -> st{ consumed = S.insert name (consumed st) })
       return ()

isConsumed :: TName -> Parc Bool
isConsumed name = S.member name <$> getConsumed

isolateConsumed :: Parc a -> Parc (a, Consumed)
isolateConsumed action
  = do consumed <- getConsumed
       x <- action
       consumed' <- getConsumed
       setConsumed consumed
       return (x, consumed')

-- for a lambda
isolateBody :: Parc a -> Parc (a, Consumed)
isolateBody action
  = isolateConsumed $ noneOwned action

--------------------------------------------------------------------------
-- Tracing
--------------------------------------------------------------------------

withCurrentDef :: Def -> Parc a -> Parc a
withCurrentDef def = withEnv (\env -> env{currentDef = def:currentDef env})

parcTraceDoc :: (Pretty.Env -> Doc) -> Parc ()
parcTraceDoc f
 = do env <- getEnv
      parcTrace (show (f (prettyEnv env)))

parcTrace :: String -> Parc ()
parcTrace msg
 = do env <- getEnv
      trace ("Core.Parc: " ++ show (map defName (currentDef env)) ++ ": " ++ msg) $ return ()


----------------
getNewtypes :: Parc Newtypes
getNewtypes = newtypes <$> getEnv

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
