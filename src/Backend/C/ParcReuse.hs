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
import Data.Maybe (catMaybes)
import qualified Data.Set as S
import qualified Data.IntMap as M

import Kind.Newtypes
import Type.Type
import qualified Type.Pretty as Pretty

import Lib.PPrint
import Common.NamePrim
import Common.Failure
import Common.Unique
import Common.Syntax

import Core.Core
import Core.Pretty

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

parcReuseCore :: Pretty.Env -> Platform -> Newtypes -> Core -> Unique Core
parcReuseCore penv platform newtypes core
  | not enabled = return core
  | otherwise   = do defs <- runReuse penv platform newtypes (ruDefGroups True (coreProgDefs core))
                     -- tr defs $ 
                     return core{coreProgDefs=defs}
  where penv' = penv{Pretty.coreShowDef=True,Pretty.coreShowTypes=False,Pretty.fullNames=False}
        tr d = trace (show (vcat (map (prettyDefGroup penv') d)))

--------------------------------------------------------------------------
-- Definition groups
--------------------------------------------------------------------------

ruDefGroups :: Bool -> DefGroups -> Reuse DefGroups
ruDefGroups topLevel = mapM (ruDefGroup topLevel)

ruDefGroup :: Bool -> DefGroup -> Reuse DefGroup
ruDefGroup topLevel dg
  = case dg of
      DefRec    defs -> DefRec    <$> mapM (ruDef topLevel) defs
      DefNonRec def  -> DefNonRec <$> ruDef topLevel def

ruDef :: Bool -> Def -> Reuse Def
ruDef topLevel def
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
        -> Lam pars eff <$> ruExpr body
      App fn args
        -> liftM2 App (ruExpr fn) (mapM ruExpr args)

      Let [] body
        -> ruExpr body
      Let (DefNonRec def:dgs) body
        -> liftM2 makeLet' (ruDef False def) (ruExpr (Let dgs body))
           where makeLet' def' = makeLet [DefNonRec def']
      Let _ _
        -> failure "Backend.C.Reuse.ruExpr"

      Case scrutinees branches
        -> liftM2 Case (mapM ruExpr scrutinees) (ruBranches branches)

      -- Var, Lit, Con
      _ -> return expr

ruTryReuseCon :: TName -> ConRepr -> Expr -> Reuse Expr
ruTryReuseCon cname repr conApp
  = do newtypes <- getNewtypes
       platform <- getPlatform
       let (size,_) = constructorSizeOf platform newtypes cname repr 
       available <- getAvailable
       -- ruTrace $ "try reuse: " ++ show (getName cname) ++ ": " ++ show size
       case M.lookup size available of
         Just tnames | not (S.null tnames)
           -> do let -- (tname, tnames') = S.deleteFindMin tnames
                     n = S.size tnames
                     tname = S.elemAt (n-1) tnames
                     tnames' = S.deleteAt (n-1) tnames
                 setAvailable (M.insert size tnames' available)
                 return (genAllocAt tname conApp)
         _ -> return conApp

ruBranches :: [Branch] -> Reuse [Branch]
ruBranches branches
  = do (branches', avs) <- unzip <$> mapM ruBranch branches
       setAvailable (availableIntersect avs)
       return branches'

ruBranch :: Branch -> Reuse (Branch, Available)
ruBranch (Branch pats guards)
  = isolateAvailable $ 
    do pats' <- mapM patAddNames pats
       reuses <- concat <$> mapM ruPattern pats'  -- must be in depth order for Parc
       added <- mapM addAvailable reuses
       (guards', avs)  <- unzip <$> mapM (ruGuard added) guards      
       setAvailable (availableIntersect avs)
       return (Branch pats' guards')
  where
    addAvailable :: (TName, Int, Int) -> Reuse (TName, TName, Int, Int)
    addAvailable (patName, size, scan)
      = do reuseName <- uniqueTName typeReuse
           updateAvailable (M.insertWith S.union size (S.singleton reuseName))
           return (reuseName, patName, size, scan)

patAddNames :: Pattern -> Reuse Pattern
patAddNames pat
  = case pat of
      PatVar{patPattern=c@PatCon{patConPatterns}}
        -> do pats' <- mapM patAddNames patConPatterns
              return pat{patPattern=c{patConPatterns=pats'}}
      PatCon{patConPatterns,patTypeRes}
        -> do name <- uniqueTName patTypeRes
              pats' <- mapM patAddNames patConPatterns
              return $ PatVar name pat{patConPatterns=pats'}
      PatVar{patPattern=PatVar{}}
        -> failure "Backend.C.ParcReuse.patAddNames"
      _ -> return pat

ruPattern :: Pattern -> Reuse [(TName, Int {-byte size-}, Int {-scan fields-})]
ruPattern (PatVar tname PatCon{patConName,patConPatterns,patConRepr,patTypeArgs,patConInfo=ci})
  = do reuses <- concat <$> mapM ruPattern patConPatterns
       if (getName patConName == nameBoxCon)
        then return reuses  -- don't reuse boxes
        else  do newtypes <- getNewtypes
                 platform <- getPlatform                 
                 -- use type scheme of con, not the instantiated type, to calculate the correct size
                 let (size,scan) = constructorSizeOf platform newtypes (TName (conInfoName ci) (conInfoType ci)) patConRepr 
                 if size > 0
                   then do -- ruTrace $ "add for reuse: " ++ show (getName tname) ++ ": " ++ show size
                           return ((tname, size, scan):reuses)
                   else return reuses
ruPattern _ = return []


ruGuard :: [(TName, TName, Int, Int)] -> Guard -> Reuse (Guard, Available)
ruGuard patAdded (Guard test expr)  -- expects patAdded in depth-order
  = isolateAvailable $
    do test' <- withNoneAvailable $ ruExpr test
       expr' <- ruExpr expr
       mbDefs <- mapM ruTryReuse patAdded
       let dgs = map DefNonRec (catMaybes mbDefs)
       return (Guard test' (makeLet dgs expr'))

-- generate drop_reuse for each reused in patAdded
ruTryReuse :: (TName, TName, Int, Int) -> Reuse (Maybe Def)
ruTryReuse (reuseName, patName, size, scan)
  = do av <- getAvailable
       case M.lookup size av of
         Just tnames | S.member reuseName tnames
           -> do let tnames' = S.delete reuseName tnames
                 setAvailable (M.insert size tnames' av)
                 return Nothing
         _ -> return (Just (makeTDef reuseName (genDropReuse patName (makeInt32 (toInteger scan)))))

-- Generate a reuse of a constructor
genDropReuse :: TName -> Expr {- : int32 -} -> Expr
genDropReuse tname scan 
  = App (Var (TName nameDropReuse funTp) (InfoExternal [(C, "drop_reuse_datatype(#1,#2,current_context())")]))
        [Var tname InfoNone, scan]
  where
    tp    = typeOf tname
    funTp = TFun [(nameNil,tp),(nameNil,typeInt32)] typeTotal typeReuse

-- generate allocation-at of a constructor application
-- at should have tyep `typeReuse`
-- conApp should have form  App (Con _ _) conArgs    : length conArgs >= 1
genAllocAt :: TName -> Expr -> Expr
genAllocAt at conApp
  = App (Var (TName nameAllocAt typeAllocAt) (InfoArity 0 1)) [Var at InfoNone, conApp]
  where
    conTp = typeOf conApp
    typeAllocAt = TFun [(nameNil,typeReuse),(nameNil,conTp)] typeTotal conTp



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

type Available = M.IntMap TNames

data Env = Env { currentDef :: [Def],
                 prettyEnv :: Pretty.Env,
                 platform  :: Platform,
                 newtypes :: Newtypes
               }

data ReuseState = ReuseState { uniq :: Int, available :: Available }

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

runReuse :: Pretty.Env -> Platform -> Newtypes -> Reuse a -> Unique a
runReuse penv platform newtypes (Reuse action)
  = withUnique $ \u ->
      let env = Env [] penv platform newtypes
          st = ReuseState u M.empty
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
availableIntersect = M.unionsWith S.intersection

--

withNoneAvailable :: Reuse a -> Reuse a
withNoneAvailable action
  = do av0 <- getAvailable
       setAvailable M.empty
       x <- action
       setAvailable av0
       return x

isolateAvailable :: Reuse a -> Reuse (a,Available)
isolateAvailable action
  = do avs0 <- getAvailable
       x <- action
       avs1 <- getAvailable
       setAvailable avs0
       return (x,avs1)


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
      _ -> trace ("constructor not a function: " ++ show conName ++ ": " ++ show (pretty (typeOf conName))) $
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
