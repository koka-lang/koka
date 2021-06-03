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

import Backend.C.ParcReuse( genDropReuse, constructorSizeOf )

--------------------------------------------------------------------------
-- Reference count transformation
--------------------------------------------------------------------------

parcCore :: Pretty.Env -> Platform -> Newtypes -> Borrowed -> Bool -> Core -> Unique Core
parcCore penv platform newtypes borrowed enableSpecialize core
  = do defs <- runParc penv platform newtypes borrowed enableSpecialize (parcDefGroups True (coreProgDefs core))
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

-- todo: should inline definitions use borrowing as well?
parcDef :: Bool -> Def -> Parc Def
parcDef topLevel def
  = (if topLevel then isolated_ else id) $
    withCurrentDef def $
    do -- parcTrace "enter def"
       expr <- (if topLevel then parcTopLevelExpr (defSort def) else parcExpr) (defExpr def)
       return def{defExpr=expr}


--------------------------------------------------------------------------
-- Main PARC algorithm
--------------------------------------------------------------------------

parcTopLevelExpr :: DefSort -> Expr -> Parc Expr
parcTopLevelExpr (DefFun bs) expr
  = case expr of
      TypeLam tpars body
        -> TypeLam tpars <$> parcTopLevelExpr (DefFun bs) body
      Lam pars eff body
        -> do let parsBs = zip pars $ bs ++ repeat Own
              let parsSet = S.fromList $ map fst $ filter (\x -> snd x == Own) parsBs

              -- todo: this is a whole second pass. is there any way around this?
              -- maybe we should track a borrowed set and presume owned, instead
              -- of the other way around?
              let caps = freeLocals expr
              (body', _) <- isolateWith S.empty
                             $ withOwned S.empty
                             $ ownedInScope (S.union caps parsSet)
                             $ parcExpr body
              dups <- foldMapM useTName caps
              return (maybeStats dups $ Lam pars eff body')
      _ -> parcExpr expr
parcTopLevelExpr _ expr = parcExpr expr

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
                             $ withOwned S.empty
                             $ ownedInScope (S.union caps parsSet)
                             $ parcExpr body
              dups <- foldMapM useTName caps
              assertion("Backend.C.Parc.parcExpr.Lam: live==[]: " ++ show live ++ "\n in: " ++ show expr) (S.null live) $
                return (maybeStats dups $ Lam pars eff body')
      Var tname info | infoIsRefCounted info
        -> do -- parcTrace ("refcounted: " ++ show tname ++ ": " ++ show info)
              fromMaybe expr <$> useTName tname

      -- Functions/Externals are not reference-counted,
      -- but they need to be wrapped if they appear outside of an application.
      -- todo: reduce type app application?
      Var tname info -- InfoArity/External/Field are not reference-counted
        -> do -- parcTrace ("not refcounted: " ++ show tname ++ ": " ++ show info)
              bs <- getParamInfos (getName tname)
              if Borrow `notElem` bs
                then return expr
                else do
                  -- parcTrace $ "Wrapping: " ++ show tname
                  case splitFunScheme $ typeOf expr of
                    Just (ts, [], as, eff, _)
                      -> do parcExpr $ addTypeLambdas ts $ addLambdas as eff
                              $ addApps (map (flip Var InfoNone . uncurry TName) as) $ addTypeApps ts
                              $ Var tname info
                    Just _
                      -> do parcTrace $ "Preds not empty: " ++ show (typeOf expr)
                            return expr
                    Nothing
                      -> return expr

      -- If the function is refcounted, it may need to be dupped.
      -- On the other hand, we want to avoid an infinite recursion with
      -- the wrapping if it is not ref-counted.
      App inner@(Var tname info) args
        -> do inner' <- if infoIsRefCounted info then parcExpr inner else return inner
              parcBorrowApp tname args inner'
      App inner@(TypeApp (Var tname info) targs) args
        -> do inner' <- if infoIsRefCounted info then parcExpr inner else return inner
              parcBorrowApp tname args inner'
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
      Let (DefRec _ : _) _
        -> failure "Backend.C.Parc.parcExpr: Recursive definition in let"
      Case vars brs | caseIsNormalized vars brs
        -> Case vars <$> parcBranches (varNames vars) brs
      Case _ _
        -> do nexpr <- normalizeCase expr
              parcExpr nexpr

parcBorrowApp :: TName -> [Expr] -> Expr -> Parc Expr
parcBorrowApp tname args expr
  = do bs <- getParamInfos (getName tname)
       let argsBs = zip args $ bs ++ repeat Own
       (lets, drops, args') <- unzip3 <$> reverseMapM (uncurry parcBorrowArg) argsBs
       -- parcTrace $ "On function " ++ show (getName tname) ++ " with args " ++ show args ++ " we have: " ++ show (lets, drops, args')
       expr' <- case catMaybes drops of
         []
           -> return $ App expr args'
         _
           -> do appName <- uniqueName "borrowApp"
                 let def = makeDef appName $ App expr args'
                 return $ makeLet [DefNonRec def] $ maybeStats drops $ Var (defTName def) InfoNone
       return $ makeLet (concat lets) expr'

parcBorrowArg :: Expr -> ParamInfo -> Parc ([DefGroup], Maybe Expr, Expr)
parcBorrowArg a b
  = do case (a, b) of
         (_, Own)
           -> (\x -> ([], Nothing, x)) <$> parcExpr a
         -- Optimize: Direct variable
         (Var tname info, Borrow)
           -> if infoIsRefCounted info
              then (\d -> ([], d, Var tname info)) <$> useTNameBorrowed tname
              else return ([], Nothing, Var tname info)
         (_, Borrow)
           -> do argName <- uniqueName "borrowArg"
                 a' <- parcExpr a
                 let def = makeDef argName a'
                 drop <- genDrop (defTName def)
                 case drop of
                   Nothing
                     -> return ([], Nothing, a') -- if no drop necessary, make output prettier
                   Just _
                     -> return ([DefNonRec def], drop, Var (defTName def) InfoNone)

varNames :: [Expr] -> [TName]
varNames (Var tn _:exprs) = tn:varNames exprs
varNames (_:exprs) = varNames exprs
varNames _ = []

parcBranches :: [TName] -> [Branch] -> Parc [Branch]
parcBranches scrutinees brs
  = do live <- getLive
       branchFns <- reverseMapM (parcBranch scrutinees live) brs
       ownedScrutinees <- filterM isOwned scrutinees
       markLives ownedScrutinees
       live' <- getLive
       mapM ($ live') branchFns

parcBranch :: [TName] -> Live -> Branch -> Parc (Live -> Parc Branch)
parcBranch scrutinees live (Branch pats guards)
  = do guardFns <- reverseMapM (parcGuard scrutinees pats live) guards
       return $ \c -> Branch pats <$> mapM ($ c) guardFns

parcGuard :: [TName] -> [Pattern] -> Live -> Guard -> Parc (Live -> Parc Guard)
parcGuard scrutinees pats live (Guard test expr)
  = do ownedPats <- map snd <$> filterM (\(s, _) -> isOwned s) (zip scrutinees pats)
       let ownedPvs = bv ownedPats
       let pvs = bv pats
       scoped pvs $ extendOwned ownedPvs $
         do shapes <- inferShapes scrutinees pats  -- create alias map for the pattern
            extendShapes shapes $ -- merge with current alias map
              do (expr', liveInThisBranch) <- isolateWith live $ parcExpr expr
                 markLives liveInThisBranch
                 test' <- withOwned S.empty $ parcExpr test
                 return $ \liveInSomeBranch -> scoped pvs $ extendOwned ownedPvs $ extendShapes shapes $ do
                  let dups = S.intersection ownedPvs liveInThisBranch
                  let drops = liveInSomeBranch \\ liveInThisBranch
                  Guard test' <$> parcGuardRC dups drops expr'

type Dups     = TNames
type Drops    = TNames

data DropInfo = Drop TName | Reuse TName

instance Show DropInfo where
  show (Drop tname) = "Drop:" ++ show tname
  show (Reuse tname) = "Reuse:"++ show tname

dropInfoVar :: DropInfo -> TName
dropInfoVar (Drop tn) = tn
dropInfoVar (Reuse tn) = tn

-- maps reused name to its reuse token binding and scan count
type ReuseInfo = M.Map TName (TName, Expr)

-- maps a name to its children
type ShapeMap = M.Map TName ShapeInfo
data ShapeInfo = ShapeInfo{
                   mchildren  :: Maybe TNames,  -- if known, Just of children
                   mconRepr   :: Maybe (ConRepr,Name), -- if known, conRepr; may be Just while mchildren is Nothing in case there is just one constructor (like a tuple)
                   scanFields :: Maybe Int
                 }

parcGuardRC :: Dups -> Drops -> Expr -> Parc Expr
parcGuardRC dups drops body
  = do let (reuses, body') = extractReuse body
       let reuseInfo = M.fromList reuses
       let drops' = map Drop (S.toList drops) ++ map (Reuse . fst) reuses
       let reuseBindings = [DefNonRec (makeTDef tname genReuseNull) | (_,(tname,_)) <- reuses]
       enable <- allowSpecialize
       rcStats <- optimizeGuard enable reuseInfo dups drops'
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



-- TODO:interaction with borrowed names
-- order invariant:
--  - all dups before drops, and drops before drop-reuses, and,
--  - within drops and dropr-reuses, each are ordered by pattern tree depth: parents must appear before children.
-- note: all drops are "tree" disjoint, none is a parent of another.
optimizeGuard :: Bool {-specialize?-} -> ReuseInfo -> Dups -> [DropInfo] -> Parc [Maybe Expr]
optimizeGuard False ri dups rdrops
  = -- no optimization on dup/drops
    do xdups  <- foldMapM genDup dups
       xdrops <- foldMapM (genDropFromInfo ri) rdrops
       return (xdups ++ xdrops)

optimizeGuard enabled ri dups rdrops
  = do shapes <- getShapeMap
       let mchildrenOf x = case M.lookup x shapes of
                            Just (ShapeInfo mchildren _ _) -> mchildren
                            _    -> Nothing
       let conNameOf x  = case M.lookup x shapes of
                            Just (ShapeInfo _ (Just (_,cname)) _) -> Just cname
                            _    -> Nothing
       optimizeGuardEx mchildrenOf conNameOf ri dups rdrops

optimizeGuardEx :: (TName -> Maybe TNames) -> (TName -> Maybe Name) -> ReuseInfo -> Dups -> [DropInfo] -> Parc [Maybe Expr]
optimizeGuardEx mchildrenOf conNameOf ri dups rdrops
  = do -- parcTrace ("optimizeGuardEx: " ++ show (dups, rdrops))
       optimize dups rdrops
  where
    childrenOf parent
      = case (mchildrenOf parent) of
          Just ch -> ch
          Nothing -> S.empty
    isChildOf parent x
      = S.member x (childrenOf parent)
    isDescendentOf parent x
      = let ys = childrenOf parent
        in S.member x ys || any (`isDescendentOf` x) ys

    optimize :: Dups -> [DropInfo] -> Parc [Maybe Expr]
    optimize dups []
      = foldMapM genDup dups
    optimize dups dropRecs | S.null dups
      = foldMapM (genDropFromInfo ri) dropRecs
    optimize dups (yRec:ys)  -- due to order, parents are first; if this would not be the case specialization opportunities may be lost
      = do newtypes <- getNewtypes
           platform <- getPlatform
           let y = dropInfoVar yRec
           case yRec of
             -- due to ordering, there are no further drops
             Reuse _ | S.member y dups
               -- can never reuse as it is dup'd: remove dup/drop-reuse pair
               -> do rest <- optimize (S.delete y dups) ys
                     set <- genSetNull ri y
                     return $ rest ++ [set]
             Drop _ | S.member y dups
               -> -- trace ("cancel dup/drop of " ++ show y) $
                  optimize (S.delete y dups) ys -- dup/drop cancels
             Drop _ | Just x <- forwardingChild platform newtypes childrenOf dups y
               -- cancel with boxes, value types that simply
               -- forward their drops to their children
               -> -- trace ("fuse forward child: " ++ show y ++ " -> " ++ show x) $
                  optimize (S.delete x dups) ys
             _ -> do let (yDups, dups') = S.partition (isDescendentOf y) dups
                     let (yRecs, ys')   = L.partition (isDescendentOf y . dropInfoVar) ys
                     rest    <- optimize dups' ys'             -- optimize outside the y tree
                     inlined <- specialize yDups yRec yRecs   -- specialize the y tree
                     return $ rest ++ [inlined]

    specialize :: Dups -> DropInfo -> [DropInfo] -> Parc (Maybe Expr)
    specialize dups v drops     -- dups and drops are descendents of v
      = -- trace ("enter specialize: " ++ show (mchildrenOf (dropInfoVar v)) ++ ", " ++ show (dups,v,drops)) $
        do xShared <- optimize dups drops   -- for the non-unique branch
           xUnique <- optimize dups $
                      map Drop (S.toList . childrenOf $ dropInfoVar v) ++ drops -- drop direct children in unique branch (note: `v \notin drops`)
           xDecRef <- genDecRef (dropInfoVar v)
           let tp = typeOf (dropInfoVar v)
           isValue <- isJust <$> getValueForm tp
           let hasKnownChildren = isJust (mchildrenOf (dropInfoVar v))
               dontSpecialize   = not hasKnownChildren ||   -- or otherwise xUnique is wrong!
                                  isValue || isBoxType tp || isFun tp || isTypeInt tp

               noSpecialize y   = do xDrop <- genDrop y
                                     return $ Just (maybeStatsUnit (xShared ++ [xDrop]))
           case v of
             Reuse y
               -> assertion "Backend.C.Parc.specialize: reuse without known children" (hasKnownChildren)  $
                  do xReuse   <- genReuseAssign ri y
                     xSetNull <- genSetNull ri y
                     return $ Just $ makeIfExpr (genIsUnique y)
                                (maybeStatsUnit (xUnique ++ [xReuse]))
                                (maybeStatsUnit (xShared ++ [xDecRef, xSetNull]))
 
             Drop y | isValue && S.size (childrenOf (dropInfoVar v)) == length dups && null drops
               -- Try to optimize a dropped value type where all fields are dup'd and where
               -- the fields are not boxed in a special way (all BoxIdentity).
               -- this optimization is important for TRMC for the `ctail` value type.
               -> -- trace ("drop spec value: " ++ show (mchildrenOf (dropInfoVar v)) ++ ", " ++ show (dups,v,drops) ++ ", " ++ show tp) $
                  do mftps <- getFieldTypes tp (conNameOf (dropInfoVar v))
                     case mftps of
                       Nothing   -> noSpecialize y
                       Just ftps -> do bforms <- mapM getBoxForm ftps
                                       if (all isBoxIdentity bforms)
                                         then -- trace ("  all box identity: " ++ show v) $
                                              return Nothing
                                         else -- trace (" no identity: " ++ show (bforms,map pretty ftps)) $
                                              noSpecialize y

             Drop y | dontSpecialize
               -- don't specialize certain primitives
               -> do -- parcTrace $ "no specialize: " ++ show y
                     noSpecialize y
             Drop y
               -> do -- parcTrace $ "specialize: " ++ show y
                     xFree <- genFree y
                     return $ Just $ makeIfExpr (genIsUnique y)
                                (maybeStatsUnit (xUnique ++ [xFree]))
                                (maybeStatsUnit (xShared ++ [xDecRef]))

-- | Return a dupped name which is a child of the given name
-- if the given name will forward a drop to the child
-- (eg. because the given name is a box or a newtype).
forwardingChild :: Platform -> Newtypes -> (TName -> TNames) -> Dups -> TName -> Maybe TName
forwardingChild platform newtypes childrenOf dups y
  = case tnamesList (childrenOf y) of
      [x] -> -- trace ("forwarding child?: " ++ show y ++ " -> " ++ show x) $
             case getValueForm' newtypes (typeOf y) of
               Just ValueOneScan
                 -> case findChild x dups of
                      Just x  -> -- trace (" is forwarding: " ++ show y ++ " -> " ++ show x) $
                                 Just x -- Just(x) as y
                      Nothing -> -- trace (" check box type child: " ++ show (y,x)) $
                                 case getBoxForm' platform newtypes (typeOf x) of
                                   BoxIdentity
                                     -> -- trace (" check child's children: " ++ show x) $
                                        case tnamesList (childrenOf x) of
                                          [x'] -> findChild x' dups  -- (Just(Box x' as x)) as y
                                          _    -> Nothing
                                   _ -> Nothing
               Just _  -> Nothing
               Nothing | isBoxType (typeOf y)
                       -> case getBoxForm' platform newtypes (typeOf x) of
                            BoxIdentity -> --trace (" box identity: " ++ show y) $
                                           findChild x dups  -- Box(x) as y
                            _ -> Nothing
               _       -> Nothing
      _ -> Nothing
 where
   findChild x dups
     = if (S.member x dups) then Just x else Nothing


genDropFromInfo :: ReuseInfo -> DropInfo -> Parc (Maybe Expr)
genDropFromInfo ri (Drop tn) = genDrop tn
genDropFromInfo ri (Reuse tn)
  = case M.lookup tn ri of
      Just (reuseName,scan)
        -> -- assertion "wrong scan fields in reuse" (snd (maybe (False,0) (scanFieldsOf tn)) == scan)  $
           -- return (Just (makeTDef reuseName (genDropReuse tn scan)))
           do expr <- genReuseAssignWith reuseName (genDropReuse tn scan)
              return (Just expr)
      _ -> failure $ "Backend.C.Parc.genDropFromInfo: cannot find: " ++ show tn



genSetNull :: ReuseInfo -> TName -> Parc (Maybe Expr)
genSetNull ri x
  = Just <$> genReuseAssignEx ri x True

genReuseAssign :: ReuseInfo -> TName -> Parc (Maybe Expr)
genReuseAssign ri x
  = Just <$> genReuseAssignEx ri x False

genReuseAssignEx :: ReuseInfo -> TName -> Bool -> Parc Expr
genReuseAssignEx ri x setNull
  = case M.lookup x ri of
      Nothing -> failure $ "Backend.C.Parc.genReuseAssignEx: cannot find: " ++ show x
      Just (r, scan)
        -> let arg = if setNull then genReuseNull else genReuseAddress x
           in genReuseAssignWith r arg


genReuseAssignWith :: TName -> Expr -> Parc Expr
genReuseAssignWith reuseName arg
  = let assign = TName nameAssignReuse (TFun [(nameNil,typeReuse),(nameNil,typeReuse)] typeTotal typeUnit)
    in return (App (Var assign (InfoExternal [(C, "#1 = #2")])) [Var reuseName InfoNone, arg])


inferShapes :: [TName] -> [Pattern] -> Parc ShapeMap
inferShapes scrutineeNames pats
  = do ms <- zipWithM shapesOf scrutineeNames pats
       return (M.unionsWith noDup ms)
  where shapesOf :: TName -> Pattern -> Parc ShapeMap
        shapesOf parent pat
          = case pat of
              PatCon{patConPatterns,patConName,patConRepr}
                -> do ms <- mapM shapesChild patConPatterns
                      scan <- getConstructorScanFields patConName patConRepr
                      let m  = M.unionsWith noDup ms
                          shape = ShapeInfo (Just (tnamesFromList (map patName patConPatterns)))
                                            (Just (patConRepr,getName patConName)) (Just scan)
                      return (M.insert parent shape m)
              PatVar{patName,patPattern}
                -> do m <- shapesOf patName patPattern
                      case M.lookup patName m of
                        Just shape -> return (M.insert parent shape m)   -- same children as parent == patName
                        Nothing -> failure $ ("Backend.C.Parc.aliasMap: unbound alias: " ++ show patName)
              PatLit _
                -> return (M.singleton parent (ShapeInfo Nothing Nothing (Just 0)))
              PatWild
                -> return (M.singleton parent (ShapeInfo Nothing Nothing Nothing))

        shapesChild :: Pattern -> Parc ShapeMap
        shapesChild pat
          = case pat of
              PatVar{patName,patPattern}
                -> shapesOf patName patPattern
              PatCon{patConPatterns}
                -> failure $ ("Backend.C.Parc.aliasMap: unnamed nested pattern")
                   -- do ms <- mapM shapesChild patConPatterns
                   --    return (M.unionsWith noDup ms)
              _ -> return M.empty

        noDup :: ShapeInfo -> ShapeInfo -> ShapeInfo
        noDup shape1 shape2 = failure $ "Backend.C.Parc.aliasMap.noDup: duplicate pattern names"


mergeShapeInfo :: ShapeInfo -> ShapeInfo -> ShapeInfo   -- prefer most info
mergeShapeInfo (ShapeInfo mchildren1 mci1 mscan1) (ShapeInfo mchildren2 mci2 mscan2)
  = ShapeInfo (mergeMaybe mchildren1 mchildren2) (mergeMaybe mci1 mci2) (mergeMaybe mscan1 mscan2)
  where
    mergeMaybe Nothing y = y
    mergeMaybe x _       = x

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
       -- parcTrace ("marked live: " ++ show tname)
       if live || borrowed
         then -- trace ("use dup " ++ show tname) $
              genDup tname
         else -- trace ("use nodup " ++ show tname) $
              return Nothing

useTNameBorrowed :: TName -> Parc (Maybe Expr)
useTNameBorrowed tname
  = do live <- isLive tname
       borrowed <- isBorrowed tname
       markLive tname
       -- parcTrace ("marked live: " ++ show tname)
       if not (live || borrowed)
         then -- trace ("use drop " ++ show tname) $
              genDrop tname
         else -- trace ("use nodrop " ++ show tname) $
              return Nothing

-----------------------------------------------------------
-- Optimize boxed reference counting
-----------------------------------------------------------

data BoxForm = BoxIdentity   -- directly in the box itself (`int` or any regular datatype)
             | BoxRaw        -- (possibly) heap allocated raw bits (`int64`)
             | BoxValue      -- (possibly) heap allocated value with scan fields (`maybe<int>`)
             deriving(Eq,Ord,Enum,Show)

isBoxIdentity BoxIdentity = True
isBoxIdentity _           = False

getBoxForm' :: Platform -> Newtypes -> Type -> BoxForm
getBoxForm' platform newtypes tp
  = case getDataDef' newtypes tp of
      Just (DataDefValue _ 0) -- 0 scan fields
        -> case extractDataDefType tp of
             Just name
               | name `elem` [nameTpInt, nameTpChar, nameTpInt8, nameTpInt16, nameTpByte, nameTpCField] ||
                 ((name `elem` [nameTpInt32, nameTpFloat32]) && sizePtr platform > 4)
                   -> BoxIdentity
             _ -> BoxRaw
      Just (DataDefValue _ _)
        -> BoxValue
      Just _
        -> BoxIdentity
      Nothing 
        -> BoxValue

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
  = do dd <- getDataDef tp
       return $ case dd of
         (DataDefValue _ 0) -> False
         _                  -> True

isValueType :: Type -> Parc Bool
isValueType tp
  = do dd <- getDataDef tp
       return $ case dd of
         (DataDefValue _ _) -> True
         _                  -> False

data ValueForm
  = ValueAllRaw   -- just bits
  | ValueOneScan  -- one heap allocated member
  | ValueOther

getValueForm' :: Newtypes -> Type -> Maybe ValueForm
getValueForm' newtypes tp
  = case getDataDef' newtypes tp of
      Just (DataDefValue _ 0) -> Just ValueAllRaw
      Just (DataDefValue 0 1) -> Just ValueOneScan
      Just (DataDefValue _ _) -> Just ValueOther
      _                       -> Nothing

getValueForm :: Type -> Parc (Maybe ValueForm)
getValueForm tp = (`getValueForm'` tp) <$> getNewtypes

getFieldTypes :: Type -> Maybe Name -> Parc (Maybe [Type])
getFieldTypes tp Nothing
  = return Nothing
getFieldTypes tp (Just conName)
  = do mdi <- getDataInfo tp
       case mdi of
         Just di -> case filter (\ci -> conName == conInfoName ci) (dataInfoConstrs di) of
                      [con] -> return (Just (map snd (conInfoParams con)))
                      _     -> return Nothing
         Nothing -> return Nothing

-- Generate a dup/drop over a given (locally bound) name
-- May return Nothing if the type never needs a dup/drop (like an `int32` or `bool`)
genDupDrop :: Bool -> TName -> Maybe (ConRepr,Name) -> Maybe Int -> Parc (Maybe Expr)
genDupDrop isDup tname (Just (ConSingleton{},_)) (Just 0)
  = do -- parcTrace $ "drop singleton: " ++ show tname
       return Nothing
genDupDrop isDup tname mbConRepr mbScanCount
  = do let tp = typeOf tname
       mbDi     <- getDataInfo tp
       borrowed <- isBorrowed tname
       -- parcTrace $ "gen dup/drop: " ++ (if (isDup) then "dup" else "drop") ++ " " ++ show tname ++ ": " ++ 
       --            show (mbDi,mbConRepr,mbScanCount,borrowed)
       if borrowed && not isDup
         then do -- parcTrace $ "  borrowed and drop, " ++ show tname
                 return Nothing
         else let normal = (Just (dupDropFun isDup tp mbConRepr mbScanCount (Var tname InfoNone)))
              in case mbDi of
                Just di -> case (dataInfoDef di, dataInfoConstrs di, snd (getDataRepr di)) of
                             (DataDefNormal, [conInfo], [conRepr])  -- data with just one constructor
                               -> do scan <- getConstructorScanFields (TName (conInfoName conInfo) (conInfoType conInfo)) conRepr
                                     -- parcTrace $ "  add scan fields: " ++ show scan ++ ", " ++ show tname
                                     return (Just (dupDropFun isDup tp (Just (conRepr,conInfoName conInfo)) (Just scan) (Var tname InfoNone)))
                             (DataDefValue _ 0, _, _)
                               -> do -- parcTrace $ ("  value with no scan fields: " ++ show di ++  ", " ++ show tname)
                                     return Nothing  -- value with no scan fields
                             _ -> do -- parcTrace $ "  dup/drop(1), " ++ show tname
                                     return normal
                _ -> do -- parcTrace $ "  dup/drop(2), " ++ show tname
                        return normal

genDup name  = do genDupDrop True name Nothing Nothing
genDrop name = do shape <- getShapeInfo name
                  genDupDrop False name (mconRepr shape) (scanFields shape)

-- get the dup/drop function
dupDropFun :: Bool -> Type -> Maybe (ConRepr,Name) -> Maybe Int -> Expr -> Expr
dupDropFun False {-drop-} tp (Just (conRepr,_)) (Just scanFields) arg  | not (conReprIsValue conRepr) && not (isBoxType tp)-- drop with known number of scan fields
  = App (Var (TName name coerceTp) (InfoExternal [(C, "dropn(#1,#2)")])) [arg,makeInt32 (toInteger scanFields)]
  where
    name = nameDrop
    coerceTp = TFun [(nameNil,tp),(nameNil,typeInt32)] typeTotal typeUnit
dupDropFun isDup tp mbConRepr mbScanCount arg
  = App (Var (TName name coerceTp) (InfoExternal [(C, (if isDup then "dup" else "drop") ++ "(#1)")])) [arg]
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
      App (Var (TName nameFree funTp) (InfoExternal [(C, "kk_constructor_free(#1)")]))
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
  = App (Var (TName nameFreeReuse funTp) (InfoExternal [(C, "kk_reuse_free(#1)")]))
        [Var tname InfoNone]
  where funTp = TFun [(nameNil, typeOf tname)] typeTotal typeReuse

-- Get a null token for reuse inlining
genReuseNull :: Expr
genReuseNull
  = App (Var (TName nameReuseNull funTp) (InfoExternal [(C, "kk_reuse_null")])) []
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
                 enableSpec:: Bool,
                 owned     :: Owned,
                 shapeMap  :: ShapeMap,
                 borrowed  :: Borrowed
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

runParc :: Pretty.Env -> Platform -> Newtypes -> Borrowed -> Bool -> Parc a -> Unique a
runParc penv platform newtypes borrowed enableSpecialize (Parc action)
  = withUnique $ \u ->
      let env = Env [] penv platform newtypes enableSpecialize S.empty M.empty borrowed
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

allowSpecialize :: Parc Bool
allowSpecialize = enableSpec <$> getEnv

getNewtypes :: Parc Newtypes
getNewtypes = newtypes <$> getEnv

withNewtypes :: (Newtypes -> Newtypes) -> Parc a -> Parc a
withNewtypes f = withEnv (\e -> e { newtypes = f (newtypes e) })

getPlatform :: Parc Platform
getPlatform = platform <$> getEnv


getConstructorScanFields :: TName -> ConRepr -> Parc Int
getConstructorScanFields conName conRepr
  = do platform <- getPlatform
       newtypes <- getNewtypes
       let (size,scan) = (constructorSizeOf platform newtypes conName conRepr)
       -- parcTrace $ "get size " ++ show conName ++ ": " ++ show (size,scan) ++ ", " ++ show conRepr
       return scan

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

-----
extendShapes :: ShapeMap -> Parc a -> Parc a
extendShapes extend
  = withEnv (\e -> e{ shapeMap = M.unionWith mergeShapeInfo extend (shapeMap e) })  -- note: left-biased, prefer extend

getShapeMap :: Parc ShapeMap
getShapeMap = shapeMap <$> getEnv

getShapeInfo :: TName -> Parc ShapeInfo
getShapeInfo tname
  = do m <- getShapeMap
       return (getShapeInfoOf tname m)

getShapeInfoOf tname m
  = case M.lookup tname m of
      Just shape -> shape
      Nothing    -> (ShapeInfo Nothing Nothing Nothing)

-- | Return borrowing infos for a name. May return the empty list
-- if no borrowing takes place.
getParamInfos :: Name -> Parc [ParamInfo]
getParamInfos name
  = do b <- borrowed <$> getEnv
       case borrowedLookup name b of
         Nothing -> return []
         Just pinfos -> return pinfos


-------------------------------
-- live set abstractions --

-- | Mark local variables as live.
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

-- | Remove the variables from the live set after the action.
scoped :: TNames -> Parc a -> Parc a
scoped vars action
  = do expr <- action
       forget vars
       return expr

-- | Assume the variables as owned in the scope
-- and insert the necessary drops
-- and remove them from the live set afterwards
ownedInScope :: TNames -> Parc Expr -> Parc Expr
ownedInScope vars action
  = scoped vars $ extendOwned vars $
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
      trace ("Backend.C.Parc: " ++ show (map defName defs) ++ ": " ++ msg) $
       return ()

----------------

getDataInfo' :: Newtypes -> Type -> Maybe DataInfo
getDataInfo' newtypes tp
  = case extractDataDefType tp of
      Nothing   -> Nothing
      Just name | name == nameBoxCon -> Nothing
      Just name -> case newtypesLookupAny name newtypes of
                      Nothing -> failure $ "Core.Parc.getDataDefInfo: cannot find type: " ++ show name
                      Just di -> -- trace ("datainfo of " ++ show (pretty tp) ++ " = " ++ show di) $
                                 Just di

getDataDef' :: Newtypes -> Type -> Maybe DataDef
getDataDef' newtypes tp
  = case getDataInfo' newtypes tp of
      Just di -> Just (dataInfoDef di)
      _       -> Nothing -- DataDefNormal


getDataInfo :: Type -> Parc (Maybe DataInfo)
getDataInfo tp
  = do newtypes <- getNewtypes
       return (getDataInfo' newtypes tp)

getDataDef :: Type -> Parc DataDef
getDataDef tp
  = do newtypes <- getNewtypes
       return (case getDataDef' newtypes tp of
                 Just dd -> dd
                 Nothing -> DataDefNormal)


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
