-----------------------------------------------------------------------------
-- Copyright 2020-2021, Microsoft Research, Daan Leijen, Alex Reinking
--
-- This is free software; you can redistribute it and/or modify it under the
-- terms of the Apache License, Version 2.0. A copy of the License can be
-- found in the LICENSE file at the root of this distribution.
-----------------------------------------------------------------------------
{-# LANGUAGE NamedFieldPuns, GeneralizedNewtypeDeriving  #-}

-----------------------------------------------------------------------------
-- constructor reuse analysis
-----------------------------------------------------------------------------

module Backend.C.ParcReuse ( parcReuseCore,
                             genDropReuse,
                             orderConFieldsEx, newtypesDataDefRepr, isDataStructLike,
                             constructorSizeOf
                           ) where

import Lib.Trace (trace)
import Control.Monad
import Control.Monad.Reader
import Control.Monad.State
import Data.Char
import Data.Maybe (catMaybes, maybeToList)
import qualified Data.Set as S
import qualified Data.Map as Map
import qualified Data.IntMap as M

import Kind.Newtypes
import Type.Type
import qualified Type.Pretty as Pretty

import Lib.PPrint
import Common.NamePrim
import qualified Common.NameMap as NameMap
import Common.Failure
import Common.Unique
import Common.Syntax

import Core.Core
import Core.Pretty

--------------------------------------------------------------------------
-- Reference count transformation
--------------------------------------------------------------------------

parcReuseCore :: Pretty.Env -> Bool -> Platform -> Newtypes -> Core -> Unique Core
parcReuseCore penv enableReuse platform newtypes core
  = do defs <- runReuse penv enableReuse platform newtypes (ruDefGroups (coreProgDefs core))
       return core{coreProgDefs=defs}
  where penv' = penv{Pretty.coreShowDef=True,Pretty.coreShowTypes=False,Pretty.fullNames=False}
        tr d = trace (show (vcat (map (prettyDefGroup penv') d)))

--------------------------------------------------------------------------
-- Definition groups
--------------------------------------------------------------------------

ruDefGroups :: DefGroups -> Reuse DefGroups
ruDefGroups = mapM ruDefGroup

ruDefGroup :: DefGroup -> Reuse DefGroup
ruDefGroup dg
  = case dg of
      DefRec    defs -> DefRec    <$> mapM ruTopLevelDef defs
      DefNonRec def  -> DefNonRec <$> ruTopLevelDef def

ruTopLevelDef :: Def -> Reuse Def
ruTopLevelDef def
  = withCurrentDef def $
      do expr <- ruExpr (defExpr def)
         return def{defExpr=expr}

--------------------------------------------------------------------------
-- Main PARC algorithm
--------------------------------------------------------------------------

ruExpr :: Expr -> Reuse Expr
ruExpr expr
  = case expr of
      App con@(Con cname repr) args
        -> do args' <- mapM ruExpr args
              ruTryReuseCon cname repr (App con args')
      App ta@(TypeApp (Con cname repr) _) args
        -> do args' <- mapM ruExpr args
              ruTryReuseCon cname repr (App ta args')

      TypeLam tpars body
        -> TypeLam tpars <$> ruExpr body
      TypeApp body targs
        -> (`TypeApp` targs) <$> ruExpr body
      Lam pars eff body
        -> ruLam pars eff body
      App fn args
        -> liftM2 App (ruExpr fn) (mapM ruExpr args)

      Let [] body
        -> ruExpr body
      Let (DefNonRec def:dgs) body
        -> ruLet def (Let dgs body)
      Let _ _
        -> failure ("Backend.C.Reuse.ruExpr: " ++ show expr)

      -- Since this runs after Parc: the scrutinees are variables
      Case scrutinees branches
        -> Case scrutinees <$> ruBranches (map unVar scrutinees) branches
        where unVar (Var n _) = n
              unVar expr = error $ "ParcReusePost: scrutinees must be variables! But got" ++ show expr

      -- Var, Lit, Con
      _ -> return expr

-- | TODO: Add Deconstructed info for the parameters to the scope.
-- This way we can reuse non-matched-on parameters where all constructors
-- have the same shape.
ruLam :: [TName] -> Effect -> Expr -> Reuse Expr
ruLam pars eff body
  = Lam pars eff <$> withNone (ruExpr body)

ruLet :: Def -> Expr -> Reuse Expr
ruLet def expr
  = do (tnames, fn) <- ruLet' def
       (expr', mReuses) <- ruTryReuseNamesIn tnames expr
       (ds, fe) <- fn mReuses
       return $ makeLet (DefNonRec <$> ds) $ fe expr'

ruLetExpr :: Expr -> Reuse ([(TName, Bool)], [Maybe Def] -> Reuse ([Def], Expr -> Expr))
ruLetExpr expr
  = case expr of
      Let [] body
        -> ruLetExpr body
      Let (DefNonRec def:dgs) body
        -> do (ys1, fn1) <- ruLet' def
              (ys2, fn2) <- ruLetExpr (Let dgs body)
              return (ys1 ++ ys2, \mrs
                -> do let (mrs1, mrs2) = splitAt (length ys1) mrs
                      (ds1, fe1) <- fn1 mrs1 
                      (ds2, fe2) <- fn2 mrs2
                      return $ (ds1 ++ ds2, fe1 . fe2))
      _ -> return ([], \_ -> return $ ([], \_ -> expr))

ruLet' :: Def -> Reuse ([(TName, Bool)], [Maybe Def] -> Reuse ([Def], Expr -> Expr))
ruLet' def
  = withCurrentDef def $
      case defExpr def of
          App var@(Var name _) [Var tname _] | getName name == nameDrop
            -> do return $ ([(tname, True)], \mReuses ->
                    case head mReuses of
                      Nothing -> return $ ([], makeLet [DefNonRec def])
                      Just ru -> return $ ([], makeLet [DefNonRec ru]))
          -- See makeDropSpecial:
          -- We assume that makeDropSpecial always occurs in a definition.
          App (Var name _) [Var y _, xUnique, xShared, xDecRef] | getName name == nameDropSpecial
            -> do (uniqYs, fUnique) <- ruLetExpr xUnique
                  return $ ((y, False):uniqYs, \mReuses -> do
                    (rusUnique, rUnique') <- fUnique (tail mReuses)
                    let rUnique = rUnique' exprUnit
                    case head mReuses of
                      Nothing
                        -> do return (rusUnique, makeLet [DefNonRec (makeDef nameNil
                                ( makeIfExpr (genIsUnique y)
                                  (makeStats [rUnique, genFree y])
                                  (makeStats [xShared, xDecRef])))])
                      Just ru
                        -> do rReuse <- genReuseAssign y 
                              return (ru:rusUnique, makeLet [DefNonRec (makeDef nameNil
                                ( makeIfExpr (genIsUnique y)
                                  (makeStats [rUnique, rReuse])
                                  (makeStats [xShared, xDecRef])))]))
          _ -> do de <- ruExpr (defExpr def)
                  return $ ([], \_ -> return ([], makeLet [DefNonRec (def{defExpr=de})]))

ruBranches :: [TName] -> [Branch] -> Reuse [Branch]
ruBranches scrutinees branches
  = do (branches', rs, avs) <- unzip3 <$> mapM (ruBranch scrutinees) branches
       setAvailable (availableIntersect avs)
       let rus = reusedUnion rs
       setReused rus
       let reuseDrops = Map.fromSet genReuseDrop rus
       return (map ($ reuseDrops) branches')

ruBranch :: [TName] -> Branch -> Reuse (Map.Map TName Expr -> Branch, Reused, Available)
ruBranch scrutinees (Branch pats guards)
  = fmap to3 $ isolateGetAvailable $ isolateDeconstructed $ isolateGetReused $
    do reuses <- concat <$> zipWithM ruPattern scrutinees pats  -- must be in depth order for Parc
       mapM_ addDeconstructed reuses
       (guards', avs)  <- unzip <$> mapM ruGuard guards
       setAvailable (availableIntersect avs)
       return (\f -> Branch pats (map ($ f) guards'))
  where
    to3 ((a,b),c) = (a,b,c)

    addDeconstructed :: (TName, (ReuseInfo, Int, Int)) -> Reuse ()
    addDeconstructed (name, (ReuseInfo patName pat, size, scan))
      = do reuseName <- uniqueTName typeReuse
           updateDeconstructed (NameMap.insert (getName name) (reuseName, ReuseInfo patName pat, size, scan))

ruPattern :: TName -> Pattern -> Reuse [(TName, (ReuseInfo, Int {-byte size-}, Int {-scan fields-}))]
ruPattern _ (PatVar tname pat) = ruPattern tname pat
ruPattern varName pat@PatCon{patConName,patConPatterns,patConRepr,patTypeArgs,patConInfo=ci}
  = do reuses <- concat <$> mapM (ruPattern varName) patConPatterns
       if getName patConName == nameBoxCon
        then return reuses  -- don't reuse boxes
        else  do newtypes <- getNewtypes
                 platform <- getPlatform
                 -- use type scheme of con, not the instantiated type, to calculate the correct size
                 let (size,scan) = constructorSizeOf platform newtypes (TName (conInfoName ci) (conInfoType ci)) patConRepr
                 if size > 0
                   then do -- ruTrace $ "add for reuse: " ++ show (getName tname) ++ ": " ++ show size
                           return ((varName, (ReuseInfo varName pat, size, scan)):reuses)
                   else return reuses
ruPattern _ _ = return []

ruGuard :: Guard -> Reuse (Map.Map TName Expr -> Guard, Available)
ruGuard (Guard test expr)  -- expects patAdded in depth-order
  = isolateGetAvailable $
    do test' <- withNone $ ruExpr test
       expr' <- ruExpr expr
       reusedHere <- getReused
       return $ \reuseDrops
         -> let dropsHere = Map.elems $ reuseDrops Map.\\ Map.fromSet (const undefined) reusedHere
            in Guard test' (makeStats (dropsHere ++ [expr']))


ruTryReuseCon :: TName -> ConRepr -> Expr -> Reuse Expr
ruTryReuseCon cname repr conApp
  = do newtypes <- getNewtypes
       platform <- getPlatform
       let (size,_) = constructorSizeOf platform newtypes cname repr
       available <- getAvailable
       -- ruTrace $ "try reuse: " ++ show (getName cname) ++ ": " ++ show size
       case M.lookup size available of
         Just (rinfo0:rinfos0)
           -> do let (rinfo,rinfos) = pick cname rinfo0 rinfos0
                 setAvailable (M.insert size rinfos available)
                 markReused (reuseName rinfo)
                 return (genAllocAt rinfo conApp)
         _ -> return conApp
  where
    -- pick a good match: for now we prefer the same constructor
    -- todo: match also common fields/arguments to help specialized reuse
    pick cname rinfo []
      = (rinfo,[])
    pick cname rinfo@(ReuseInfo name (PatCon{patConName})) rinfos  | patConName == cname
      = (rinfo,rinfos)
    pick cname rinfo (rinfo':rinfos)
      = let (r,rs) = pick cname rinfo' rinfos in (r,rinfo:rs)

ruTryReuseNameIn :: Bool -> TName -> Expr -> Reuse (Expr, Maybe Def)
ruTryReuseNameIn shouldGenDrop tname expr
  = do (expr', ds) <- ruTryReuseNamesIn [(tname, shouldGenDrop)] expr
       return (expr', head ds)

ruTryReuseNamesIn :: [(TName, Bool)] -> Expr -> Reuse (Expr, [Maybe Def])
ruTryReuseNamesIn tnames expr
  = do dss <- getDeconstructed
       rus <- fmap concat $ forM tnames $ \(tname, shouldGenDrop) -> do
        case NameMap.lookup (getName tname) dss of
          Nothing -> return [Nothing]
          Just (reuseName, ReuseInfo patName pat, size, scan)
            -> do mReuse <- ruTryReuse shouldGenDrop (reuseName, patName, size, scan)
                  case mReuse of
                    Nothing -> return [Nothing]
                    Just ru -> return [Just (reuseName, size, pat, ru)]
       isolateAvailable $
         do forM_ rus $ \m ->
             case m of
               Just (reuseName, size, pat, _)
                 -> updateAvailable (M.insertWith (++) size [ReuseInfo reuseName pat])
               Nothing -> return ()
            expr' <- ruExpr expr
            rus' <- forM rus $ \m -> case m of
              Just (reuseName, _, _, ru) -> do
                wasReused <- askAndDeleteReused reuseName
                return $ if wasReused then Just ru else Nothing
              Nothing -> return Nothing
            return (expr', rus')

-- generate drop_reuse for each reused in patAdded
ruTryReuse :: Bool -> (TName, TName, Int, Int) -> Reuse (Maybe Def)
ruTryReuse shouldGenDrop (rName, patName, size, scan)
  = do av <- getAvailable
       enable <- getEnableReuse
       if not enable then return Nothing
       else case M.lookup size av of
         Just rinfos  | rName `elem` (map reuseName rinfos)
           -> do let rest = filter (\r -> rName /= reuseName r) rinfos
                 setAvailable (M.insert size rest av)
                 return Nothing
         _ -> if shouldGenDrop
                then return (Just (makeTDef rName (genDropReuse patName (makeInt32 (toInteger scan)))))
                else do return (Just (makeTDef rName genReuseNull))

-- Generate a reuse of a constructor
genDropReuse :: TName -> Expr {- : int32 -} -> Expr
genDropReuse tname scan
  = App (Var (TName nameDropReuse funTp) (InfoExternal [(C, "drop_reuse(#1,#2,kk_context())")]))
        [Var tname InfoNone, scan]
  where
    tp    = typeOf tname
    funTp = TFun [(nameNil,tp),(nameNil,typeInt32)] typeTotal typeReuse

-- generate allocation-at of a constructor application
-- at should have tyep `typeReuse`
-- conApp should have form  App (Con _ _) conArgs    : length conArgs >= 1
genAllocAt :: ReuseInfo -> Expr -> Expr
genAllocAt (ReuseInfo reuseName pat) conApp
  = App (Var (TName nameAllocAt typeAllocAt) (InfoArity 0 2)) [Var reuseName (InfoReuse pat), conApp]
  where
    conTp = typeOf conApp
    typeAllocAt = TFun [(nameNil,typeReuse),(nameNil,conTp)] typeTotal conTp

-- Generate a test if a (locally bound) name is unique
genIsUnique :: TName -> Expr
genIsUnique tname
  = App (Var (TName nameIsUnique funTp) (InfoExternal [(C, "is_unique(#1)")]))
        [Var tname InfoNone]
  where funTp = TFun [(nameNil, typeOf tname)] typeTotal typeBool

-- Generate a free of a constructor
genFree :: TName -> Expr
genFree tname
  = App (Var (TName nameFree funTp) (InfoExternal [(C, "kk_constructor_free(#1)")]))
        [Var tname InfoNone]
  where funTp = TFun [(nameNil, typeOf tname)] typeTotal typeUnit


-- Generate a drop of a reuse
genReuseDrop :: TName -> Expr
genReuseDrop tname
  = App (Var (TName nameReuseDrop funTp) (InfoExternal [(C, "kk_reuse_drop")]))
        [Var tname InfoNone]
  where funTp = TFun [(nameNil, typeOf tname)] typeTotal typeReuse

genSetNull :: TName -> Reuse Expr
genSetNull x
  = genReuseAssignEx x True

genReuseAssign :: TName -> Reuse Expr
genReuseAssign x
  = genReuseAssignEx x False

genReuseAssignEx :: TName -> Bool -> Reuse Expr
genReuseAssignEx x setNull
  = do dss <- getDeconstructed
       case NameMap.lookup (getName x) dss of
          Nothing -> failure $ "Backend.C.Parc.genReuseAssignEx: cannot find: " ++ show x
          Just (r, _, _, _)
            -> let arg = if setNull then genReuseNull else genReuseAddress x
               in return $ genReuseAssignWith r arg

-- Get a null token for reuse inlining
genReuseNull :: Expr
genReuseNull
  = App (Var (TName nameReuseNull funTp) (InfoExternal [(C, "kk_reuse_null")])) []
  where funTp = TFun [] typeTotal typeReuse

-- Generate a reuse a block
genReuseAddress :: TName -> Expr
genReuseAddress tname
  = App (Var (TName nameReuse funTp) (InfoExternal [(C, "reuse_datatype(#1,current_context())")])) [Var tname InfoNone]
  where
    tp    = typeOf tname
    funTp = TFun [(nameNil,tp)] typeTotal typeReuse

genReuseAssignWith :: TName -> Expr -> Expr
genReuseAssignWith reuseName arg
  = let assign = TName nameAssignReuse (TFun [(nameNil,typeReuse),(nameNil,typeReuse)] typeTotal typeUnit)
    in App (Var assign (InfoExternal [(C, "#1 = #2")])) [Var reuseName InfoNone, arg]

--------------------------------------------------------------------------
-- Utilities for readability
--------------------------------------------------------------------------

-- create a unique name specific to this module
uniqueTName :: Type -> Reuse TName
uniqueTName tp = (`TName` tp) <$> uniqueName "ru"

-- for mapping over a set and collecting the results into a list.
foldMapM :: (Monad m, Foldable t) => (a -> m b) -> t a -> m [b]
foldMapM f = foldr merge (return [])
  where merge x r = do y <- f x
                       (y:) <$!> r

maybeStats :: [Maybe Expr] -> Expr -> Expr
maybeStats xs expr
  = makeStats (catMaybes xs ++ [expr])

--------------------------------------------------------------------------
-- Reuse monad
--------------------------------------------------------------------------

-----------------
-- definitions --

type Available = M.IntMap [ReuseInfo]
type Deconstructed = NameMap.NameMap (TName, ReuseInfo, Int {-byte size-}, Int {-scan fields-})
type Reused = S.Set TName

data ReuseInfo = ReuseInfo{ reuseName :: TName, pattern :: Pattern }

data Env = Env { currentDef :: [Def],
                 enableReuse :: Bool,
                 prettyEnv :: Pretty.Env,
                 platform  :: Platform,
                 newtypes :: Newtypes
               }

data ReuseState = ReuseState { uniq :: Int, 
                               available :: Available,
                               deconstructed :: Deconstructed,
                               reused :: Reused }

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

runReuse :: Pretty.Env -> Bool -> Platform -> Newtypes -> Reuse a -> Unique a
runReuse penv enableReuse platform newtypes (Reuse action)
  = withUnique $ \u ->
      let env = Env [] enableReuse penv platform newtypes
          st = ReuseState u M.empty NameMap.empty S.empty 
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

--

getNewtypes :: Reuse Newtypes
getNewtypes = newtypes <$> getEnv

withNewtypes :: (Newtypes -> Newtypes) -> Reuse a -> Reuse a
withNewtypes f = withEnv (\e -> e { newtypes = f (newtypes e) })

--

getPlatform :: Reuse Platform
getPlatform = platform <$> getEnv

---------------------
-- state accessors --

getUniq :: Reuse Int
getUniq = uniq <$> getSt

modifyUniq :: (Int -> Int) -> Reuse ()
modifyUniq f = updateSt (\s -> s { uniq = f (uniq s) })

setUniq :: Int -> Reuse ()
setUniq = modifyUniq . const

--

getAvailable :: Reuse Available
getAvailable = available <$> getSt

updateAvailable :: (Available -> Available) -> Reuse ()
updateAvailable f = updateSt (\s -> s { available = f (available s) })

setAvailable :: Available -> Reuse ()
setAvailable = updateAvailable . const

availableIntersect :: [Available] -> Available
availableIntersect
  = M.unionsWith intersect
  where
    intersect xs ys
      = [r | r@(ReuseInfo rname _) <- xs, rname `elem` map reuseName ys]

getDeconstructed :: Reuse Deconstructed
getDeconstructed = deconstructed <$> getSt

updateDeconstructed :: (Deconstructed -> Deconstructed) -> Reuse ()
updateDeconstructed f = updateSt (\s -> s { deconstructed = f (deconstructed s) })

setDeconstructed :: Deconstructed -> Reuse ()
setDeconstructed = updateDeconstructed . const

deconstructedIntersect :: [Deconstructed] -> Deconstructed
deconstructedIntersect = foldl NameMap.intersection NameMap.empty 

markReused :: TName -> Reuse ()
markReused name = updateSt (\s -> s { reused = S.insert name (reused s) })

askAndDeleteReused :: TName -> Reuse Bool
askAndDeleteReused name
 = do rs <- reused <$> getSt
      let b = S.member name rs
      let rs' = S.delete name rs
      updateSt (\s -> s { reused = rs' })
      return b

getReused :: Reuse Reused
getReused = reused <$> getSt

setReused :: Reused -> Reuse ()
setReused ru = updateSt (\s -> s { reused = ru })

reusedUnion :: [Reused] -> Reused
reusedUnion = S.unions

getEnableReuse :: Reuse Bool
getEnableReuse = asks enableReuse

--

-- | Execute the action with an empty state.
-- Used for the body of lambdas
-- and the test of guards.
withNone :: Reuse a -> Reuse a
withNone action
  = do av0 <- getAvailable
       ds0 <- getDeconstructed
       setAvailable M.empty
       setDeconstructed NameMap.empty
       x <- action
       setAvailable av0
       setDeconstructed ds0
       -- TODO: Reused map?
       return x

isolateAvailable :: Reuse a -> Reuse a
isolateAvailable = fmap fst . isolateGetAvailable

isolateGetAvailable :: Reuse a -> Reuse (a,Available)
isolateGetAvailable action
  = do avs0 <- getAvailable
       x <- action
       avs1 <- getAvailable
       setAvailable avs0
       return (x,avs1)

isolateDeconstructed :: Reuse a -> Reuse a
isolateDeconstructed action
  = do dss <- getDeconstructed
       x <- action
       setDeconstructed dss
       return x

isolateGetReused :: Reuse a -> Reuse (a, Reused)
isolateGetReused action
  = do r0 <- getReused
       x <- action
       r1 <- getReused
       setReused r0
       return (x,r1)

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
      trace ("Core.Reuse: " ++ show (map defName defs) ++ ": " ++ msg) $
        return ()

----------------

-- return the allocated size of a constructor. Return 0 for value types or singletons
constructorSizeOf :: Platform -> Newtypes -> TName -> ConRepr -> (Int {- byte size -}, Int {- scan fields -})
constructorSizeOf platform newtypes conName conRepr
  = case splitFunScheme (typeOf conName) of
      Just (_,_,tpars,_,_)
        -> constructorSize platform newtypes conRepr (map snd tpars)
      _ -> -- trace ("constructor not a function: " ++ show conName ++ ": " ++ show (pretty (typeOf conName))) $
           (0,0)


-- return the allocated size of a constructor. Return 0 for value types or singletons
constructorSize :: Platform -> Newtypes -> ConRepr -> [Type] -> (Int {- byte size -}, Int {- scan fields -})
constructorSize platform newtypes conRepr paramTypes
  = let dataRepr = (conDataRepr conRepr)
    in {-  if dataReprIsValue dataRepr
         then (0,0)
         else-}
        let (fields,size,scan) = orderConFieldsEx platform newtypes (DataOpen == dataRepr) [(nameNil,tp) | tp <- paramTypes]
        in if dataReprIsValue dataRepr
            then (0,scan)
            else (size,scan)


-- order constructor fields of constructors with raw field so the regular fields come first to be scanned.
-- return the ordered fields, the byte size of the allocation, and the scan count (including tags)
orderConFieldsEx :: Platform -> Newtypes -> Bool -> [(Name,Type)] -> ([(Name,Type)],Int,Int)
orderConFieldsEx platform newtypes isOpen fields
  = visit ([],[],0,0) fields
  where
    visit (rraw, rscan, scanCount0, mixCount) []
      = if (mixCount > 1)
         then failure ("Backend.C.ParcReuse.orderConFields: multiple fields with mixed raw/scan fields itself in " ++ show fields)
         else let scanCount = scanCount0 + (if (isOpen) then 1 else 0)  -- +1 for the open datatype tag
                  ssize = scanCount * (sizePtr platform)
                  rsize = alignedSum ssize (map snd (reverse rraw))
                  size  = alignUp rsize (sizeSize platform)
              in (reverse rscan ++ map fst (reverse rraw), size, scanCount)
    visit (rraw,rscan,scanCount,mixCount) (field@(name,tp) : fs)
      = let (dd,dataRepr) = newtypesDataDefRepr newtypes tp
        in case dd of
             DataDefValue raw scan
               -> let extra = if (isDataStructLike dataRepr) then 1 else 0 in -- adjust scan count for added "tag_t" members in structs with multiple constructors
                  if (raw > 0 && scan > 0)
                   then -- mixed raw/scan: put it at the head of the raw fields (there should be only one of these as checked in Kind/Infer)
                        -- but we count them to be sure (and for function data)
                        visit (rraw ++ [(field,raw)], rscan, scanCount + scan  + extra, mixCount + 1) fs
                   else if (raw > 0)
                         then visit ((field,raw):rraw, rscan, scanCount, mixCount) fs
                         else visit (rraw, field:rscan, scanCount + scan + extra, mixCount) fs
             _ -> visit (rraw, field:rscan, scanCount + 1, mixCount) fs


newtypesDataDefRepr :: Newtypes -> Type -> (DataDef,DataRepr)
newtypesDataDefRepr newtypes tp
   = case extractDataDefType tp of
       Nothing   -> (DataDefNormal,DataNormal True)
       Just name | name == nameTpBox -> (DataDefNormal,DataNormal False)
       Just name -> case newtypesLookupAny name newtypes of
                      Nothing -> failure $ "Backend.C.ParcReuse.getDataDefRepr: cannot find type: " ++ show name
                      Just di -> (dataInfoDef di, fst (getDataRepr di))

extractDataDefType tp
 = case expandSyn tp of
     TApp t _      -> extractDataDefType t
     TForall _ _ t -> extractDataDefType t
     TCon tc       -> Just (typeConName tc)
     _             -> Nothing


isDataStructLike (DataAsMaybe) = True
isDataStructLike (DataStruct) = True
isDataStructLike _ = False
