-----------------------------------------------------------------------------
-- Copyright 2020-2021, Microsoft Research, Daan Leijen, Alex Reinking, Anton Lorenzen
--
-- This is free software; you can redistribute it and/or modify it under the
-- terms of the Apache License, Version 2.0. A copy of the License can be
-- found in the LICENSE file at the root of this distribution.
-----------------------------------------------------------------------------
{-# LANGUAGE NamedFieldPuns, GeneralizedNewtypeDeriving  #-}

{----------------------------------------------------------------------------
-- precise automatic reference counting (now called "Perceus")
-- See: https://www.microsoft.com/en-us/research/uploads/prod/2020/11/perceus-tr-v4.pdf

Notes:
- The monad has a borrowed and owned (multi-set) environment just like the paper
- The live variable set is a state
- To calculate the live variables we visit the expression tree _in reverse_
  (see the parcDefGroup, and parcExpr for let-bindings and applications for example)
- That still works with the borrowed and owned environments as those stay
  the same in a scope.
----------------------------------------------------------------------------}

module Backend.C.Parc ( parcCore, getDataDef' ) where

import Lib.Trace (trace)
import Control.Monad
import Control.Monad.Reader
import Control.Monad.State.Strict
import Data.Maybe ( catMaybes, fromMaybe, isJust, mapMaybe )
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
parcTopLevelExpr ds@(DefFun bs _) expr
  = case expr of
      TypeLam tpars body
        -> TypeLam tpars <$> parcTopLevelExpr ds body
      Lam pars eff body
        -> do let parsBs = zip pars $ bs ++ repeat Own
              let parsSet = S.fromList $ map fst $ filter (\x -> snd x == Own) parsBs
              (dups, body') <- parcLam expr parsSet body
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
        -> do let parsSet = S.fromList pars
              (dups, body') <- parcLam expr parsSet body
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
        -> do def1  <- -- check if we need to name a result in case it will be dropped
                       do mbDrop <- if (nameIsNil (defName def)) then genDrop (defTName def) else return Nothing
                          case mbDrop of
                            Just _ -- | nameIsNil (defName def) 
                              -> do name <- uniqueName "res" -- name the result
                                    return def{defName = name}
                            _ -> return def
              body1 <- ownedInScope (S.singleton $ defTName def1) $ parcExpr (Let dgs body)
              def2  <- parcDef False def1
              return $ makeLet [DefNonRec def2] body1
      Let (DefRec _ : _) _
        -> failure "Backend.C.Parc.parcExpr: Recursive definition in let"
      Case vars brs | caseIsNormalized vars brs
        -> Case vars <$> parcBranches (varNames vars) brs
      Case _ _
        -> do nexpr <- normalizeCase expr
              parcExpr nexpr

-- todo: this is a whole second pass. is there any way around this?
-- maybe we should track a borrowed set and presume owned, instead
-- of the other way around?
parcLam :: Expr -> S.Set TName -> Expr -> Parc ([Maybe Expr], Expr)
parcLam expr parsSet body
  = do let caps = freeLocals expr
       (body', _) <- isolateWith S.empty
                       $ withOwned S.empty
                       $ ownedInScope (S.union caps parsSet)
                       $ parcExpr body
       dups <- foldMapM useTName caps
       pure (dups, body')

parcBorrowApp :: TName -> [Expr] -> Expr -> Parc Expr
parcBorrowApp tname args expr
  = do bs <- getParamInfos (getName tname)
       if Borrow `notElem` bs 
         then App expr <$> reverseMapM parcExpr args
         else do  let argsBs = zip args (bs ++ repeat Own)
                  (lets, drops, args') <- unzip3 <$> reverseMapM (uncurry parcBorrowArg) argsBs
                  -- parcTrace $ "On function " ++ show (getName tname) ++ " with args " ++ show args ++ " we have: " ++ show (lets, drops, args')
                  expr' <- case catMaybes drops of
                             [] -> return $ App expr args'
                             _  -> do appName <- uniqueName "brw"
                                      let def = makeDef appName $ App expr args'
                                      return $ makeLet [DefNonRec def] $ maybeStats drops $ Var (defTName def) InfoNone
                  return $ makeLet (concat lets) expr'

-- | Let-float borrowed arguments to the top so that we can drop them after the function call
-- We also let-float owned arguments so that the order of evaluation is unchanged.
-- We make the result prettier by not floating variables and owned total expressions.
parcBorrowArg :: Expr -> ParamInfo -> Parc ([DefGroup], Maybe Expr, Expr)
parcBorrowArg expr parInfo
  = case (expr, parInfo) of
      (_, Own)
        | isTotal expr -> (\x -> ([], Nothing, x)) <$> parcExpr expr
        | otherwise
          -> do argName <- uniqueName "own"
                expr' <- parcExpr expr
                let def = makeDef argName expr'
                return ([DefNonRec def], Nothing, Var (defTName def) InfoNone)
      (Var tname info, Borrow)
        -> if infoIsRefCounted info
            then (\d -> ([], d, expr)) <$> useTNameBorrowed tname
            else return ([], Nothing, expr)
      (_, Borrow)
        -> do expr' <- parcExpr expr
              notRefCounted <- exprIsNotRefcounted expr   -- for example, small integer literals
              if (notRefCounted) 
                then return ([], Nothing, expr') 
                else do argName <- uniqueName "brw"
                        let def = makeDef argName expr'
                        drop <- extendOwned (S.singleton (defTName def)) $ genDrop (defTName def)
                        return ([DefNonRec def], drop, Var (defTName def) InfoNone)

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
                  drops <- filterM isOwned (S.toList $ liveInSomeBranch \\ liveInThisBranch)
                  Guard test' <$> parcGuardRC dups (S.fromList drops) expr'

type Dups     = TNames
type Drops    = TNames

-- maps a name to its children
type ShapeMap = M.Map TName ShapeInfo
data ShapeInfo = ShapeInfo{
                   mchildren  :: Maybe TNames,  -- if known, Just of children
                   mconRepr   :: Maybe (ConRepr,Name), -- if known, conRepr; may be Just while mchildren is Nothing in case there is just one constructor (like a tuple)
                   scanFields :: Maybe Int
                 }

parcGuardRC :: Dups -> Drops -> Expr -> Parc Expr
parcGuardRC dups drops body
  = do enable <- allowSpecialize
       rcStats <- optimizeGuard enable dups drops 
       return $ maybeStats rcStats body


-- TODO:interaction with borrowed names
-- order invariant:
--  - all dups before drops, and drops before drop-reuses, and,
--  - within drops and dropr-reuses, each are ordered by pattern tree depth: parents must appear before children.
-- note: all drops are "tree" disjoint, none is a parent of another.
optimizeGuard :: Bool {-specialize?-} -> Dups -> Drops -> Parc [Maybe Expr]
optimizeGuard False dups rdrops
  = -- no optimization on dup/drops
    do xdups  <- foldMapM genDup dups
       xdrops <- foldMapM genDrop rdrops
       return (xdups ++ xdrops)

optimizeGuard True {-specialize-} dups rdrops
  = do shapes <- getShapeMap
       let mchildrenOf x = case M.lookup x shapes of
                             Just (ShapeInfo (Just mchildren) _ _) | not (null mchildren) -> Just mchildren                            
                             _    -> Nothing
       let conNameOf x  = case M.lookup x shapes of
                            Just (ShapeInfo _ (Just (_,cname)) _) -> Just cname
                            _    -> Nothing
       optimizeDupDrops mchildrenOf conNameOf dups rdrops


optimizeDupDrops :: (TName -> Maybe TNames) -> (TName -> Maybe Name) -> Dups -> Drops -> Parc [Maybe Expr]
optimizeDupDrops mchildrenOf conNameOf dups0 drops0
  = do (fdups, fdrops) <- fuseDupDrops childrenOf dups0 drops0
       assertion ("Backend.C.Parc.optimizeDupDrops: intersection not empty: " ++ show (fdups,fdrops))
                 (S.null (S.intersection fdups fdrops)) $
         optimizeDisjoint fdups (S.toList fdrops)
  where
    childrenOf x
      = case mchildrenOf x of
          Just children -> children
          Nothing       -> S.empty

    isDescendentOf parent x
      = let ys = childrenOf parent
        in S.member x ys || any (`isDescendentOf` x) ys

    optimizeDisjoint :: Dups -> [TName] -> Parc [Maybe Expr]
    optimizeDisjoint dups []
      = do foldMapM genDup dups     
    -- optimizeDisjoint dups drops | S.null dups  -- todo: do not do this as it will not specialize all drops
    --  = do foldMapM genDrop drops      
    optimizeDisjoint dups (y:drops)
      = do  let (yDups, dups')    = S.partition (isDescendentOf y) dups
            let (yDrops, drops')  = L.partition (isDescendentOf y) drops
            rest   <- optimizeDisjoint dups' drops'             -- optimize outside the y tree
            prefix <- mapM genDrop yDrops                       -- todo: these could be decRef as these can never be unique
            spec   <- specializeDrop mchildrenOf conNameOf yDups y   -- specialize the y tree
            return $ rest ++ prefix ++ spec


specializeDrop :: (TName -> Maybe TNames) -> (TName -> Maybe Name) -> Dups -> TName -> Parc [Maybe Expr]
specializeDrop mchildrenOf conNameOf dups v    -- dups are descendents of v
  = do  -- parcTrace ("enter specialize: " ++ show v ++ ", children: " ++ show (mchildrenOf v) ++ ", dups: " ++ show dups)
        xShared <- foldMapM genDup dups         -- for the non-unique branch
        xUnique <- optimizeDupDrops mchildrenOf conNameOf dups (childrenOf v) -- drop direct children in unique branch (note: `v \notin drops`)
        let tp = typeOf v
        isValue <- isJust <$> getValueForm tp
        isDataAsMaybe <- getIsDataAsMaybe tp
        let hasKnownChildren = isJust (mchildrenOf v)
            dontSpecialize   = not hasKnownChildren ||   -- or otherwise xUnique is wrong!
                               isValue || isBoxType tp || isFun tp || isTypeInt tp || isDataAsMaybe

            noSpecialize y   = do xDrop <- genDrop y
                                  return $ xShared ++ [xDrop]
        if isValue && all (\child -> S.member child dups) (S.toList (childrenOf v))
            -- Try to optimize a dropped value type where all fields are dup'd and where
            -- the fields are not boxed in a special way (all BoxIdentity).
            -- this optimization is important for TRMC for the `ctail` value type.
          then -- trace ("drop spec value: " ++ show v ++ ", children: " ++ show (mchildrenOf v) ++ ", dups: " ++ show (dups) ++ ", tp: " ++ show tp) $
               do mftps <- getFieldTypes tp (conNameOf v)
                  case mftps of
                    Nothing   -> noSpecialize v
                    Just ftps -> do bforms <- mapM getBoxForm ftps
                                    if (all isBoxIdentityOrUnknown bforms)
                                      then -- trace ("** all box identity: " ++ showTName (v) ++ ": " ++ show (bforms,map pretty ftps)) $
                                           return []
                                      else -- trace ("** no identity: " ++ showTName (v) ++ ": " ++ show (bforms,map pretty ftps)) $
                                           noSpecialize v

          {- else if isBoxType tp 
            -- elide drop/dups on boxed types whose elements are not heap allocated
            then case S.toList (childrenOf v) of
                   [x] -> do bx <- getBoxForm (typeOf x)
                             case bx of 
                               BoxIdentity -> do parcTrace $ "boxed value that is never heap allocated"
                                                 return [] 
                               _ -> do parcTrace $ "boxed value with boxform: " ++ show bx
                                       noSpecialize v
                   [] -> do parcTrace $ "no specialize boxed: " ++ show v
                            noSpecialize v
                   _  -> failure $ "Backend.C.Parc: boxed type with more than one child: " ++ show (childrenOf v) -}
          else if dontSpecialize
            -- don't specialize certain primitives
            then do -- parcTrace $ "no specialize: " ++ show v
                    noSpecialize v
            else do -- parcTrace $ "specialize: " ++ show y
                    xDecRef <- genDecRef v
                    let maybeStatsUnit :: [Maybe Expr] -> Expr
                        maybeStatsUnit xs
                          = maybeStats xs exprUnit  -- must be let to match in ParcReuse
                          -- = case catMaybes xs of
                          --    []     -> exprUnit
                          --    exprs  -> makeStats exprs 
                    return $ [Just (makeDropSpecial v (maybeStatsUnit xUnique) (maybeStatsUnit xShared) (maybeStatsUnit [xDecRef]))]           

  where
    childrenOf x
      = case mchildrenOf x of
          Just children -> children
          Nothing       -> S.empty

-- Remove dup/drop pairs
fuseDupDrops :: (TName -> TNames) -> Dups -> Drops -> Parc (Dups, Drops)
fuseDupDrops childrenOf dups drops
  = fuseAliases childrenOf (dups S.\\ drops) (drops S.\\ dups)
      
fuseAliases :: (TName -> TNames) -> Dups -> Drops -> Parc (Dups,Drops)
fuseAliases childrenOf dups drops
  = do newtypes <- getNewtypes
       platform <- getPlatform
       return $ L.foldl' (fuseAlias platform newtypes) (dups,S.empty) (S.toList drops)
  where
    fuseAlias :: Platform -> Newtypes -> (Dups,Drops) -> TName -> (Dups,Drops)
    fuseAlias platform newtypes (dups,drops) y 
      = case forwardingChild platform newtypes childrenOf dups y of
          Just child -> assertion ("Backend.C.Parc.fuseAlias: not a member? " ++ show (child,dups)) 
                                  (S.member child dups) $
                        (S.delete child dups, drops)  
          Nothing    -> (dups,S.insert y drops)        -- not (S.member y dups)

-- | Return a dupped name which is a child of the given name
-- if the given name will always forward a drop directly to the child
-- (e.g. because the given name is a box or a newtype).
forwardingChild :: Platform -> Newtypes -> (TName -> TNames) -> Dups -> TName -> (Maybe TName)
forwardingChild platform newtypes childrenOf dups y
  = case tnamesList (childrenOf y) of
      [x] -> -- trace ("forwarding child?: " ++ show y ++ " -> " ++ show x) $
             case getValueForm' newtypes (typeOf y) of
               Just ValueOneScan  -- for example `value type maybe<a> { Nothing; Just(val:a) }` 
                 -> case findChild x dups of
                      Just x  -> -- trace (" is forwarding: " ++ show y ++ " -> " ++ show x) $
                                 Just x -- y as Just(x)
                      Nothing | isBoxType (typeOf x)
                              -> case tnamesList (childrenOf x) of
                                   [x'] -> case getBoxForm' platform newtypes (typeOf x') of
                                            BoxIdentity
                                              -> findChild x' dups  -- y as Just(x as Box(x'))
                                            _ -> Nothing
                                   _ -> Nothing
                      Nothing -> -- trace (" check box type child: " ++ show (y,x)) $
                                 case getBoxForm' platform newtypes (typeOf x) of
                                   BoxIdentity
                                     -> case tnamesList (childrenOf x) of
                                          [x'] -> findChild x' dups 
                                          _    -> Nothing
                                   _ -> Nothing
               Just _  -> Nothing
               Nothing | isBoxType (typeOf y)
                       -> case getBoxForm' platform newtypes (typeOf x) of
                            BoxIdentity -> --trace (" box identity: " ++ show y) $
                                           findChild x dups  -- y as Box(x) 
                            _ -> Nothing
               _       -> Nothing
      _ -> Nothing
 where
   findChild x dups
     = if (S.member x dups) then Just x else Nothing

inferShapes :: [TName] -> [Pattern] -> Parc ShapeMap
inferShapes scrutineeNames pats
  = do ms <- zipWithM shapesOf scrutineeNames pats
       return (M.unionsWith noDup ms)
  where shapesOf :: TName -> Pattern -> Parc ShapeMap
        shapesOf parent pat
          = case pat of
              PatCon{patConPatterns,patConName,patConRepr,patConInfo}
                -> do ms <- mapM shapesChild patConPatterns
                      let scan = conReprScanCount patConRepr
                          m  = M.unionsWith noDup ms
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
             | BoxUnknown 
             deriving(Eq,Ord,Enum,Show)

isBoxIdentity BoxIdentity = True
isBoxIdentity _           = False

isBoxIdentityOrUnknown BoxIdentity = True
isBoxIdentityOrUnknown BoxUnknown  = True
isBoxIdentityOrUnknown _           = False

getBoxForm' :: Platform -> Newtypes -> Type -> BoxForm
getBoxForm' platform newtypes tp
  = -- trace ("getBoxForm' of " ++ show (pretty tp)) $
    case getDataDef' newtypes tp of
      Just (DataDefValue (ValueRepr m 0 _)) -- 0 scan fields, m is size in bytes of raw fields
        -> -- trace "  0 scan fields" $
           case extractDataDefType tp of
             Just name
               | name `elem` [nameTpInt, nameTpFieldAddr] ||
                 ((name `elem` [nameTpInt8, nameTpInt16, nameTpFloat16]) && sizePtr platform > 2) ||
                 ((name `elem` [nameTpChar, nameTpInt32, nameTpFloat32]) && sizePtr platform > 4)
                   -> BoxIdentity
             _ -> if m < sizePtr platform   -- for example, `bool`, but not `int64`
                   then BoxIdentity 
                   else BoxRaw
      Just (DataDefValue{})
        -> BoxValue
      Just _
        -> BoxIdentity
      Nothing 
        -> BoxUnknown

getBoxForm :: Type -> Parc BoxForm
getBoxForm tp
 = do platform <- getPlatform
      newtypes <- getNewtypes
      return $ getBoxForm' platform newtypes tp

-----------------------------------------------------------
-- Optimize boxed reference counting
-----------------------------------------------------------

-- Generate a ref-count drop of a constructor
genDecRef :: TName -> Parc (Maybe Expr)
genDecRef tname
  = do needs <- needsDupDrop (typeOf tname)
       if not needs
         then return Nothing
         else return $ Just $
                        App (Var (TName nameDecRef funTp) (InfoExternal [(C CDefault, "decref(#1,current_context())")]))
                            [Var tname InfoNone]
  where
    funTp = TFun [(nameNil, typeOf tname)] typeTotal typeUnit

-- value types with reference fields still need a drop
needsDupDrop :: Type -> Parc Bool
needsDupDrop tp
  = do dd <- getDataDef tp
       return $ case dd of
         (DataDefValue vr) | valueReprIsRaw vr -> False
         _                 -> True

isValueType :: Type -> Parc Bool
isValueType tp
  = do dd <- getDataDef tp
       return $ case dd of
         (DataDefValue{}) -> True
         _                -> False

data ValueForm
  = ValueAllRaw   -- just bits
  | ValueOneScan  -- one heap allocated member
  | ValueOther

getValueForm' :: Newtypes -> Type -> Maybe ValueForm
getValueForm' newtypes tp
  = case getDataDef' newtypes tp of
      Just (DataDefValue (ValueRepr _ 0 _)) -> Just ValueAllRaw
      Just (DataDefValue (ValueRepr 0 1 _)) -> Just ValueOneScan
      Just (DataDefValue _)                 -> Just ValueOther
      _                                     -> Nothing

getValueForm :: Type -> Parc (Maybe ValueForm)
getValueForm tp = (`getValueForm'` tp) <$> getNewtypes

getFieldTypes :: Type -> Maybe Name -> Parc (Maybe [Type])
getFieldTypes tp Nothing
  = return Nothing
getFieldTypes tp (Just conName)
  = do mdi <- getDataInfo tp
       case mdi of
         Just di | dataInfoName di == nameOptional -> return Nothing  -- prevent trying to optimize the optional type in specialize above
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
       --             show (mbDi,mbConRepr,mbScanCount,borrowed)
       if borrowed && not isDup
         then do -- parcTrace $ "  borrowed and drop, " ++ show tname
                 return Nothing
         else let normal = (Just (dupDropFun isDup tp mbConRepr mbScanCount (Var tname InfoNone)))
              in case mbDi of
                Just di -> case (dataInfoDef di, dataInfoConstrs di, snd (getDataRepr di)) of
                             (DataDefNormal, [conInfo], [conRepr])  -- data with just one constructor
                               -> do let scan = conReprScanCount conRepr
                                     -- parcTrace $ "  add scan fields: " ++ show scan ++ ", " ++ show tname
                                     return (Just (dupDropFun isDup tp (Just (conRepr,conInfoName conInfo)) (Just scan) (Var tname InfoNone)))
                             (DataDefValue vr, _, _) | valueReprIsRaw vr
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
dupDropFun False {-drop-} tp (Just (conRepr,_)) (Just scanFields) arg  
   | not (conReprIsValue conRepr) && not (isConAsJust conRepr) && not (isBoxType tp) -- drop with known number of scan fields
  = App (Var (TName name coerceTp) (InfoExternal [(C CDefault, "dropn(#1,#2)")])) [arg,makeInt32 (toInteger scanFields)]
  where
    name = nameDrop
    coerceTp = TFun [(nameNil,tp),(nameNil,typeInt32)] typeTotal typeUnit
dupDropFun isDup tp mbConRepr mbScanCount arg
  = App (Var (TName name coerceTp) (InfoExternal [(C CDefault, (if isDup then "dup" else "drop") ++ "(#1)")])) [arg]
  where
    name = if isDup then nameDup else nameDrop
    coerceTp = TFun [(nameNil,tp)] typeTotal (if isDup then tp else typeUnit)


-- | is an expression definitely not reference counted?
exprIsNotRefcounted :: Expr -> Parc Bool
exprIsNotRefcounted expr
  = case expr of
      Lit (LitInt i)   
        -> return (i >= -8191 && i <= 8191)  -- 14 bits is safe on every platform
      _ -> not <$> needsDupDrop (typeOf expr)



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
isBorrowed tn | nameIsNil (getName tn) = return False
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
                      Nothing -> failure $ "Core.Parc.getDataDefInfo: cannot find type: " ++ show name -- ++ "\n" ++ show newtypes
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

getIsDataAsMaybe :: Type -> Parc Bool
getIsDataAsMaybe tp
  = do mbDi <- getDataInfo tp
       return $ case mbDi of
                  Just di -> case fst (getDataRepr di) of
                                DataAsMaybe -> True
                                _ -> False
                  Nothing -> False


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

