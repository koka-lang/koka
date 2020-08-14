-----------------------------------------------------------------------------
-- Copyright 2020 Microsoft Corporation, Daan Leijen, Alex Reinking
--
-- This is free software; you can redistribute it and/or modify it under the
-- terms of the Apache License, Version 2.0. A copy of the License can be
-- found in the file "license.txt" at the root of this distribution.
-----------------------------------------------------------------------------
{-# LANGUAGE NamedFieldPuns, GeneralizedNewtypeDeriving  #-}

-----------------------------------------------------------------------------
-- precise automatic reference counting
-----------------------------------------------------------------------------

module Backend.C.Parc ( parcCore ) where

import Lib.Trace (trace)
import Control.Monad
import Control.Monad.Reader
import Control.Monad.State
import Data.Maybe ( catMaybes, fromMaybe )
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
import Common.Syntax

import Core.Core
import Core.CoreVar
import Core.Pretty

import Platform.Runtime (unsafePerformIO)
import qualified System.Environment as Sys

import Backend.C.ParcReuse( genDropReuse )

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

parcCore :: Pretty.Env -> Platform -> Newtypes -> Core -> Unique Core
parcCore penv platform newtypes core
  | not enabled = return core
  | otherwise   = do defs <- runParc penv platform newtypes (parcDefGroups True (coreProgDefs core))
                     -- tr defs $
                     return core{coreProgDefs=defs}
  where penv' = penv{Pretty.coreShowDef=True,Pretty.coreShowTypes=False,Pretty.fullNames=False}
        tr d = trace (show (vcat (map (prettyDefGroup penv') d)))

--------------------------------------------------------------------------
-- Definition groups
--------------------------------------------------------------------------

parcDefGroups :: Bool -> DefGroups -> Parc DefGroups
parcDefGroups topLevel = reverseMapM (parcDefGroup topLevel)

parcDefGroup :: Bool -> DefGroup -> Parc DefGroup
parcDefGroup topLevel dg
  = case dg of
      DefRec    defs -> DefRec    <$> reverseMapM (parcDef topLevel) defs
      DefNonRec def  -> DefNonRec <$> parcDef topLevel def

parcDef :: Bool -> Def -> Parc Def
parcDef topLevel def
  = (if topLevel then isolated_ else id) $
    withCurrentDef def $
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
        -> do -- todo: this is a whole second pass. is there any way around this?
              -- maybe we should track a borrowed set and presume owned, instead
              -- of the other way around?
              let caps = freeLocals expr
              let parsSet = S.fromList pars
              (body', live) <- isolateWith S.empty
                             $ withOwned caps  -- captured variables are owned
                             $ ownedInScope parsSet
                             $ parcExpr body
              dups <- foldMapM useTName caps
              assertion ("parcExpr: caps==live" ++ show caps ++ show live ++ show body) (caps == live) $
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
      Let [] body
        -> parcExpr body
      Let (DefNonRec def:dgs) body
        -> do body' <- ownedInScope (bv def) $ parcExpr (Let dgs body)
              def'  <- parcDef False def
              return $ makeLet [DefNonRec def'] body'
      Let _ _
        -> failure "Backend.C.Parc.parcExpr"
      Case vars brs | caseIsNormalized vars brs
        -> Case vars <$> parcBranches (varNames vars) brs
      Case _ _
        -> do nexpr <- normalizeCase expr
              parcExpr nexpr

varNames :: [Expr] -> [TName]
varNames (Var tn _:exprs) = tn:varNames exprs
varNames (_:exprs) = varNames exprs
varNames _ = []

parcBranches :: [TName] -> [Branch] -> Parc [Branch]
parcBranches scrutinees brs
  = do live <- getLive
       branchFns <- reverseMapM (parcBranch scrutinees live) brs
       markLives scrutinees
       live' <- getLive
       mapM ($ live') branchFns

parcBranch :: [TName] -> Live -> Branch -> Parc (Live -> Parc Branch)
parcBranch scrutinees live (Branch pats guards)
  = do guardFns <- reverseMapM (parcGuard scrutinees pats live) guards
       return $ \c -> Branch pats <$> mapM ($ c) guardFns

parcGuard :: [TName] -> [Pattern] -> Live -> Guard -> Parc (Live -> Parc Guard)
parcGuard scrutinees pats live (Guard test expr)
  = scoped pvs $
    do (expr', live') <- isolateWith live $ parcExpr expr
       markLives live'
       test' <- withOwned S.empty $ parcExpr test
       return $ \matchLive -> scoped pvs $ do
         let dups = S.intersection pvs live'
         let drops = matchLive \\ live'
         Guard test' <$> parcGuardRC scrutinees pats dups drops expr'
  where pvs = bv pats

type Dups   = TNames
type Drops  = TNames

data DropRec = Drop TName | Reuse TName
type DropRecs = [DropRec]

getDropVar :: DropRec -> TName
getDropVar (Drop tn) = tn
getDropVar (Reuse tn) = tn

-- maps reused name to its token variable and scan count
type ReuseInfo = M.Map TName (TName, Expr)

parcGuardRC :: [TName] -> [Pattern] -> Dups -> Drops -> Expr -> Parc Expr
parcGuardRC scrutinees pats dups drops body
  = do let aliases = aliasMap scrutinees pats
       let (reuses, body') = extractReuse body
       let reuseInfo = M.fromList reuses
       let drops' = map Drop (S.toList drops) ++ map (Reuse . fst) reuses
       let reuseBindings = [DefNonRec (makeTDef tname genReuseNull) | (_,(tname,_)) <- reuses]
       rcStats <- optimizeGuard aliases reuseInfo dups drops'
       return $ makeLet reuseBindings (maybeStats rcStats body')

extractReuse :: Expr -> ([(TName, (TName, Expr))], Expr)
extractReuse expr
  = let (dgs, body) = collectLets [] expr
        (reuses, dgs') = extractReuses dgs
     in (reuses, makeLet dgs' body)
  where
    extractReuses (DefNonRec (Def x tp (App (Var reuse _) [Var y InfoNone, scanCount]) Private DefVal _ _ _):dgs)
      | getName reuse == nameDropReuse
          = let (rs,dgs') = extractReuses dgs
                r = (y, (TName x tp, scanCount))
             in (r:rs, dgs')
    extractReuses defs = ([], defs)

    collectLets acc (Let dgs body) = collectLets (dgs:acc) body
    collectLets acc expr = (concat (reverse acc), expr)

-- maps a name to its children
type AliasMap = M.Map TName TNames

-- TODO:
-- 1. what about non-heap allocated data? (always unique)
-- 2. what about value types with references? (need type-specific dup/drop but have no refcount)
-- 3. interaction with borrowed names
-- order invariant:
--  all dups before drops, and drops before drop-reuses, and,
--  within drops and dropr-reuses, each are ordered by pattern tree depth:
--   parents must appear before children.
-- note: all drops are "tree" disjoint, none is a parent of another.
optimizeGuard :: AliasMap -> ReuseInfo -> Dups -> DropRecs -> Parc [Maybe Expr]
optimizeGuard aliases ri = opt
  where
    children x
      = M.findWithDefault S.empty x aliases
    isChildOf parent x
      = S.member x (children parent)
    isDescendentOf parent x
      = let ys = children parent
         in not (S.null ys) && (S.member x ys || any (`isDescendentOf` x) ys)

    opt :: Dups -> DropRecs -> Parc [Maybe Expr]
    opt dups []
      | S.null dups = return []
      | otherwise   = foldMapM genDup dups
    opt dups dropRecs
      | S.null dups = foldMapM (genDropRec ri) dropRecs
    opt dups (yRec:ys)  -- due to order, parents are first; if this would not be the case specialization opportunities may be lost
      = do newtypes <- getNewtypes
           platform <- getPlatform
           let y = getDropVar yRec
           case yRec of
             -- due to ordering, there are no further drops
             Reuse _ | S.member y dups
               -- can never reuse as it is dup'd: remove dup/drop-reuse pair
               -> do rest <- opt (S.delete y dups) ys
                     set <- genSetNull ri y
                     return $ rest ++ [set]
             Drop _ | S.member y dups
               -> opt (S.delete y dups) ys -- dup/drop cancels
             Drop _ | Just x <- forwardingChild newtypes platform dups y
               -- cancel with boxes, value types that simply
               -- forward their drops to their children
               -> opt (S.delete x dups) ys
             _ -> do let (yDups, dups') = S.partition (isDescendentOf y) dups
                     let (yRecs, ys')   = L.partition (isDescendentOf y . getDropVar) ys
                     rest    <- opt dups' ys'             -- optimize outside the y tree
                     inlined <- inline yDups yRec yRecs   -- specialize the y tree
                     return $ rest ++ [inlined]

    forwardingChild :: Newtypes -> Platform -> Dups -> TName -> Maybe TName
    forwardingChild newtypes platform dups y
      | [x] <- S.toList (S.filter (isChildOf y) dups)
      = let ty = typeOf y
         in case getValueForm' newtypes ty of
              Just ValueOneScan -> Just x
              Just _            -> Nothing
              Nothing -> case getBoxForm' platform newtypes (typeOf x) of
                           BoxIdentity | isBoxType ty -> Just x
                           _ -> Nothing
    forwardingChild _ _ _ _
      = Nothing

    inline dups v drops     -- dups and drops are descendents of v
      = Just <$>
        do xShared <- opt dups drops   -- for the non-unique branch
           xUnique <- opt dups $
                      map Drop (S.toList . children $ getDropVar v) ++ drops -- drop direct children in unique branch (note: `v \notin drops`)
           xDecRef <- genDecRef (getDropVar v)
           case v of
             Reuse y
               -> do xReuse   <- genReuseAssign ri y
                     xSetNull <- genSetNull ri y
                     return $ makeIfExpr (genIsUnique y)
                                (maybeStatsUnit (xUnique ++ [xReuse]))
                                (maybeStatsUnit (xShared ++ [xDecRef, xSetNull]))
             Drop y | isFun (typeOf y) || isTypeInt (typeOf y)
               -- don't specialize certain primitives
               -> do xDrop <- genDrop y
                     return (maybeStatsUnit (xShared ++ [xDrop]))
             Drop y
               -> do xFree <- genFree y
                     return $ makeIfExpr (genIsUnique y)
                                (maybeStatsUnit (xUnique ++ [xFree]))
                                (maybeStatsUnit (xShared ++ [xDecRef]))


genDropRec :: ReuseInfo -> DropRec -> Parc (Maybe Expr)
genDropRec _ (Drop tn) = genDrop tn
genDropRec ri (Reuse tn)
  = case M.lookup tn ri of
      Just (r,scan) -> return (Just (genDropReuse tn scan))
      _ -> failure $ "Backend.C.Parc.genDropRec: cannot find: " ++ show tn


genSetNull :: ReuseInfo -> TName -> Parc (Maybe Expr)
genSetNull ri x = genReuseAssignEx ri x True

genReuseAssign :: ReuseInfo -> TName -> Parc (Maybe Expr)
genReuseAssign ri x = genReuseAssignEx ri x False

genReuseAssignEx :: ReuseInfo -> TName -> Bool -> Parc (Maybe Expr)
genReuseAssignEx ri x setNull =
  return $ do
    (r, scan) <- M.lookup x ri  -- TODO: failure if not found
    let assign = TName nameAssignReuse (TFun [(nameNil,typeReuse),(nameNil,typeOf x)] typeTotal typeUnit)
        arg    = if setNull then genReuseNull else genReuseAddress x
    return (App (Var assign (InfoExternal [(C, "#1 = #2")])) [Var r InfoNone, arg])

aliasMap :: [TName] -> [Pattern] -> AliasMap
aliasMap scrutineeNames pats = M.unions $ zipWith aliases scrutineeNames pats
  where aliases parent PatVar{patName,patPattern}
          = M.insertWith S.union parent (S.singleton patName) $
              aliases patName patPattern
        aliases parent PatCon{patConPatterns}
          = M.unionsWith S.union (map (aliases parent) patConPatterns)
        aliases _ _ = M.empty


-- Generate a reuse a block
genReuseAddress :: TName -> Expr
genReuseAddress tname
  = App (Var (TName nameReuse funTp) (InfoExternal [(C, "reuse_datatype(#1,current_context())")])) [Var tname InfoNone]
  where
    tp    = typeOf tname
    funTp = TFun [(nameNil,tp)] typeTotal typeReuse

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
normalizeCase Case{caseExprs, caseBranches}
  = do (vexprs, dgs) <- unzip <$> reverseMapM caseExpandExpr caseExprs
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
  = let nameMapping pat new
           = case pat of
               PatVar old pat' -> (Just (old, new), pat')
               _ -> (Nothing, pat)
        (newNames, pats') = unzip $ zipWith nameMapping branchPatterns vexprs
        guards' = catMaybes newNames |~> branchGuards
     in Branch pats' guards'

--------------------------------------------------------------------------
-- Convenience methods for inserting PARC ops
--------------------------------------------------------------------------

useTName :: TName -> Parc (Maybe Expr)
useTName tname
  = do live <- isLive tname
       borrowed <- isBorrowed tname
       markLive tname
       if live || borrowed
         then genDup tname
         else return Nothing

-----------------------------------------------------------
-- Optimize boxed reference counting
-----------------------------------------------------------

data BoxForm = BoxIdentity   -- directly in the box itself (`int` or any regular datatype)
             | BoxRaw        -- (possibly) heap allocated raw bits (`int64`)
             | BoxValue      -- (possibly) heap allocated value with scan fields (`maybe<int>`)
             deriving(Eq,Ord,Enum,Show)

getBoxForm' :: Platform -> Newtypes -> Type -> BoxForm
getBoxForm' platform newtypes tp
  = case getDataDefRepr' newtypes tp of
      (DataDefValue _ 0, _) -- 0 scan fields
        -> case extractDataDefType tp of
             Just name
               | name `elem` [nameTpInt, nameTpChar, nameTpInt8, nameTpInt16, nameTpByte] ||
                 ((name `elem` [nameTpInt32, nameTpFloat32]) && sizePtr platform > 4)
                   -> BoxIdentity
             _ -> BoxRaw
      (DataDefValue _ _, _)
        -> BoxValue
      _ -> BoxIdentity

getBoxForm :: Type -> Parc BoxForm
getBoxForm tp
 = do platform <- getPlatform
      newtypes <- getNewtypes
      return $ getBoxForm' platform newtypes tp

-----------------------------------------------------------
-- Optimize boxed reference counting
-----------------------------------------------------------

-- value types with reference fields still need a drop
needsDupDrop :: Type -> Parc Bool
needsDupDrop tp
  = do repr <- getDataDefRepr tp
       return $ case repr of
         (DataDefValue _ 0, _) -> False
         _                     -> True

isValueType :: Type -> Parc Bool
isValueType tp
  = do repr <- getDataDefRepr tp
       return $ case repr of
         (DataDefValue _ _, _) -> True
         _                     -> False

data ValueForm
  = ValueAllRaw   -- just bits
  | ValueOneScan  -- one heap allocated member
  | ValueOther

getValueForm' :: Newtypes -> Type -> Maybe ValueForm
getValueForm' newtypes tp
  = case getDataDefRepr' newtypes tp of
      (DataDefValue _ 0, _) -> Just ValueAllRaw
      (DataDefValue 0 1, _) -> Just ValueOneScan
      (DataDefValue _ _, _) -> Just ValueOther
      _                     -> Nothing

getValueForm :: Type -> Parc (Maybe ValueForm)
getValueForm tp = (`getValueForm'` tp) <$> getNewtypes

-- Generate a dup/drop over a given (locally bound) name
-- May return Nothing if the type never needs a dup/drop (like an `int32` or `bool`)
genDupDrop :: Bool -> TName -> Parc (Maybe Expr)
genDupDrop isDup tname
  = do let tp = typeOf tname
       repr <- getDataDefRepr tp
       borrowed <- isBorrowed tname
       return $
         if borrowed && not isDup
           then Nothing
           else case repr of
                 (DataDefValue _ 0, _) -> Nothing
                 _ -> Just (App (dupDropFun isDup tp) [Var tname InfoNone])

genDup  = genDupDrop True
genDrop = genDupDrop False

-- get the dup/drop function
dupDropFun :: Bool -> Type -> Expr
dupDropFun isDup tp
  = Var (TName name coerceTp) (InfoExternal [(C, (if isDup then "dup" else "drop") ++ "(#1)")])
  where
    name = if isDup then nameDup else nameDrop
    coerceTp = TFun [(nameNil,tp)] typeTotal (if isDup then tp else typeUnit)

-- Generate a test if a (locally bound) name is unique
genIsUnique :: TName -> Expr
genIsUnique tname
  = App (Var (TName nameIsUnique funTp) (InfoExternal [(C, "is_unique(#1)")]))
        [Var tname InfoNone]
  where funTp = TFun [(nameNil, typeOf tname)] typeTotal typeBool

-- Generate a free of a constructor
genFree :: TName -> Parc (Maybe Expr)
genFree tname
  = return $ Just $
      App (Var (TName nameFree funTp) (InfoExternal [(C, "runtime_free(#1)")]))
        [Var tname InfoNone]
  where funTp = TFun [(nameNil, typeOf tname)] typeTotal typeUnit

-- Generate a ref-count drop of a constructor
genDecRef :: TName -> Parc (Maybe Expr)
genDecRef tname
  = do needs <- needsDupDrop (typeOf tname)
       if not needs
         then return Nothing
         else return $ Just $
                        App (Var (TName nameDecRef funTp) (InfoExternal [(C, "decref(#1,current_context())")]))
                            [Var tname InfoNone]
  where
    funTp = TFun [(nameNil, typeOf tname)] typeTotal typeUnit


-- Generate a reuse free of a constructor
genFreeReuse :: TName -> Expr
genFreeReuse tname
  = App (Var (TName nameFreeReuse funTp) (InfoExternal [(C, "free_reuse(#1)")]))
        [Var tname InfoNone]
  where funTp = TFun [(nameNil, typeOf tname)] typeTotal typeReuse

-- Get a null token for reuse inlining
genReuseNull :: Expr
genReuseNull
  = App (Var (TName nameReuseNull funTp) (InfoExternal [(C, "reuse_null")])) []
  where funTp = TFun [] typeTotal typeReuse


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

maybeStatsUnit :: [Maybe Expr] -> Expr
maybeStatsUnit xs
  = case catMaybes xs of
      []    -> exprUnit
      stats -> makeStats stats


--------------------------------------------------------------------------
-- Parc monad
--------------------------------------------------------------------------

-----------------
-- definitions --

type Owned = TNames
data Env = Env { currentDef :: [Def],
                 prettyEnv :: Pretty.Env,
                 platform  :: Platform,
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

runParc :: Pretty.Env -> Platform -> Newtypes -> Parc a -> Unique a
runParc penv platform newtypes (Parc action)
  = withUnique $ \u ->
      let env = Env [] penv platform newtypes S.empty
          st = ParcState u S.empty
          (val, st') = runState (runReaderT action env) st
       in (val, uniq st')

-------------------
-- env accessors --

getCurrentDef :: Parc [Def]
getCurrentDef = currentDef <$> getEnv

withCurrentDef :: Def -> Parc a -> Parc a
withCurrentDef def = withEnv (\e -> e { currentDef = def : currentDef e })

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

getPlatform :: Parc Platform
getPlatform = platform <$> getEnv

--

getOwned :: Parc Owned
getOwned = owned <$> getEnv

updateOwned :: (Owned -> Owned) -> Parc a -> Parc a
updateOwned f = withEnv (\e -> e { owned = f (owned e) })

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

isBorrowed :: TName -> Parc Bool
isBorrowed tn = not <$> isOwned tn

withOwned :: Owned -> Parc a -> Parc a
withOwned = updateOwned . const

extendOwned :: Owned -> Parc a -> Parc a
extendOwned = updateOwned . S.union

-------------------------------
-- live set abstractions --

-- only add locals (can happen with scrutinees)
markLive :: TName -> Parc ()
markLive tname
  = unless (isQualified (getName tname))
  $ modifyLive (S.insert tname)

markLives :: Foldable t => t TName -> Parc ()
markLives = mapM_ markLive

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

scoped :: TNames -> Parc a -> Parc a
scoped vars action
  = do expr <- extendOwned vars action
       forget vars
       return expr

ownedInScope :: TNames -> Parc Expr -> Parc Expr
ownedInScope vars action
  = scoped vars $
      do expr <- action
         live <- getLive
         drops <- foldMapM genDrop (vars \\ live)
         return $ maybeStats drops expr

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

getDataDefRepr' :: Newtypes -> Type -> (DataDef, DataRepr)
getDataDefRepr' newtypes tp
  = case extractDataDefType tp of
      Nothing -> (DataDefNormal, DataNormal True)
      Just name | name == nameBoxCon -> (DataDefNormal, DataNormal False)
      Just name -> case newtypesLookupAny name newtypes of
                      Nothing -> failure $ "Core.Parc.getDataDefRepr: cannot find type: " ++ show name
                      Just di -> (dataInfoDef di, fst (getDataRepr di))

getDataDefRepr :: Type -> Parc (DataDef, DataRepr)
getDataDefRepr tp
  = do newtypes <- getNewtypes
       return $ getDataDefRepr' newtypes tp

extractDataDefType :: Type -> Maybe Name
extractDataDefType tp
  = case expandSyn tp of
      TApp t _      -> extractDataDefType t
      TForall _ _ t -> extractDataDefType t
      TCon tc       -> Just (typeConName tc)
      _             -> Nothing


isBoxType :: Type -> Bool
isBoxType (TCon (TypeCon name _))  = name == nameTpBox
isBoxType (TVar _)                 = True
isBoxType (TSyn _ _ tp)            = isBoxType tp
isBoxType (TApp tp _)              = isBoxType tp
isBoxType _                        = False -- trace ("not a box: " ++ show (pretty tp))  False
